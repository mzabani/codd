module Codd.Internal.MultiQueryStatement
  ( InTransaction(..)
  , SqlStatementException(..)
  , multiQueryStatement_
  , runSingleStatementInternal_
  ) where

import           Codd.Internal.Retry            ( retry_ )
import           Codd.Parsing                   ( ParsedSql(..)
                                                , SqlPiece(..)
                                                )
import           Codd.Types                     ( RetryPolicy )
import           Control.Monad                  ( void )
import           Control.Monad.Logger           ( MonadLogger )
import           Data.Text                      ( Text )
import qualified Data.Text as Text
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Database.PostgreSQL.LibPQ     as PQ
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.Copy
                                               as DB
import qualified Database.PostgreSQL.Simple.Internal
                                               as PGInternal
import qualified Database.PostgreSQL.Simple.Types
                                               as DB
import           Prelude                 hiding ( takeWhile )
import           Streaming                      ( Of
                                                , Stream
                                                , Of(..)
                                                )
import Streaming.Internal (Stream(..))
import qualified Streaming.Prelude             as Streaming
import           UnliftIO                       ( Exception
                                                , MonadIO
                                                , MonadUnliftIO
                                                , handle
                                                , liftIO
                                                , throwIO
                                                )

-- Multi-query statements are automatically enveloped in a single transaction by the server. This happens because according to
-- https://www.postgresql.org/docs/12/libpq-exec.html, "Multiple queries sent in a single PQexec call are processed in a single transaction,
-- unless there are explicit BEGIN/COMMIT commands included in the query string to divide it into multiple transactions."
-- This creates problem for statements that can't run inside a single transaction (changing enums, creating dbs etc.)
-- Because that seems to be at libpq level, we need to parse SQL (ugh..) and detect plPGSQL bodies and standard SQL to
-- split commands up.. I expect problems from this, really.
-- Note 1: Maybe alex works to translate psqlscan.l to Haskell? Seems like a rather complicated translation when I look at the source,
-- and I don't know anything about lex/flex/alex.. also, we dong't need to be as good as psql in parsing SQL; we just need to find statement boundaries
-- (psql counts parentheses, for instance) to split them up by that.
-- Note 2: The CLI utility psql does the same and splits up commands before sending them to the server. See https://github.com/postgres/postgres/blob/master/src/fe_utils/psqlscan.l

data SqlStatementException = SqlStatementException
  { sqlStatement :: Text
  , psimpleError :: DB.SqlError
  }
  deriving stock Show

instance Exception SqlStatementException

-- | Runs SQL that could be either a row-returning or count-returning statement. Just lie postgresql-simple, throws exceptions in case of SQL errors.
singleStatement_ :: MonadIO m => DB.Connection -> Text -> m ()
singleStatement_ conn sql = do
  res    <- liftIO $ PGInternal.exec conn $ encodeUtf8 sql
  status <- liftIO $ PQ.resultStatus res
  case status of
    PQ.CommandOk -> pure ()
    PQ.TuplesOk  -> pure ()
    _ ->
      liftIO
        $ handle (throwIO . SqlStatementException sql)
        $
          -- Throw to catch and re-throw.. a bit nasty, but should be ok
          PGInternal.throwResultError "singleStatement_" res status

data InTransaction = InTransaction | NotInTransaction RetryPolicy deriving stock (Eq)

-- | A bit like singleStatement_, but following these criteria:
--   1. If already in a transaction, then statements will be executed one-by-one.
--   2. If not in a transaction, then statements will be executed one-by-one and
--      will be retried according to the retry policy.
multiQueryStatement_
  :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
  => InTransaction
  -> DB.Connection
  -> ParsedSql m
  -> m ()
multiQueryStatement_ inTxn conn sql = case (sql, inTxn) of
  (UnparsedSql t, InTransaction) -> singleStatement_ conn t
  (UnparsedSql t, NotInTransaction retryPol) ->
    retry_ retryPol $ singleStatement_ conn t
  (WellParsedSql stms, NotInTransaction retryPol) ->
    -- We retry individual statements in no-txn migrations
                                                     flip Streaming.mapM_ (batchCopyRows stms)
    $ \stm -> retry_ retryPol $ runSingleStatementInternal_ conn stm
  (WellParsedSql stms, InTransaction) ->
    -- We don't retry individual statements in in-txn migrations
    flip Streaming.mapM_ (batchCopyRows stms) $ \stm -> runSingleStatementInternal_ conn stm

-- | Data type that helps batching COPY rows to reduce round-trips. Other statements are
-- currently not batched in any form.
-- Before including other statements in batching, remember that:
-- - Sql statement exceptions will contain much more SQL and will be harder to debug.
-- - We shouldn't batch in no-txn migrations, at least not without being very careful.
data BatchedSqlStatements = StandaloneSqlPiece !SqlPiece | BatchedCopyRows !Text

batchCopyRows
  :: forall m. Monad m => Stream (Of SqlPiece) m () -> Stream (Of BatchedSqlStatements) m ()
batchCopyRows = go ""
  where
    go rowsAcc stream = Effect $ do
      e <- Streaming.next stream
      pure $ case e of
        Left () ->
            -- Last batch must go regardless of size
            if rowsAcc /= "" then Streaming.yield (BatchedCopyRows rowsAcc) else mempty
        Right (el, rest) ->
          case el of
            CopyFromStdinRow row ->
              let
                newLength = Text.length row + Text.length rowsAcc
              in
                -- 512KB minimum for each batch
                if newLength > 512 * 1024 then
                  Streaming.yield (BatchedCopyRows $ rowsAcc <> row) <> go "" rest
                else
                  go (rowsAcc <> row) rest
            _ ->
              -- This could be the COPY terminator, so yield rowsAcc if it's not empty
                 (if rowsAcc /= "" then Streaming.yield (BatchedCopyRows rowsAcc) else mempty)
              <> Streaming.yield (StandaloneSqlPiece el)
              <> go "" rest
    
    -- spanMap :: (a -> Maybe b) -> Stream (Of a) m () -> Stream (Of b) m (Stream (Of a) m ())
    -- spanMap f s = Effect $ do
    --   e <- Streaming.next s
    --   case e of
    --     Left () -> Return mempty
    --     Right (el, rest) ->
    --       case f el of
    --         Nothing -> Return $ Streaming.yield el <> rest
    --         Just v  -> Streaming.yield v <> spanMap f rest
    
    -- isCopyRow :: SqlPiece -> Maybe Text
    -- isCopyRow = \case
    --   CopyFromStdinRow row -> Just row
    --   _ -> Nothing
    
    -- takeCopyRowsUntilSize :: Stream (Of Text) m a -> Stream (Of Text) m a
    -- takeCopyRowsUntilSize = loop ""
    --   where
    --     loop acc s = Effect $ do
    --       e <- Streaming.next s
    --       pure $ case e of
    --         Left r ->
    --           -- Last batch must go regardless of size
    --           if acc /= "" then Streaming.yield acc else Return r
    --         Right (el, rest) ->
    --           let
    --             newLength = Text.length el + Text.length acc
    --           in
    --             -- 512KB minimum for each batch
    --             if newLength > 512 * 1024 then
    --               Streaming.yield (acc <> el) <> loop "" rest
    --             else
    --               loop (acc <> el) rest
    -- singlePieceOrMultipleRows :: Stream (Of SqlPiece) m () -> m (Either SqlPiece (Stream (Of Text) m ()))
    -- singlePieceOrMultipleRows ps = do
    --   eP1Rest <- Streaming.next ps
    --   case eP1Rest of
    --     Left () -> error "Impossible: empty stream."
    --     Right (p1, rest) -> case p1 of
    --       CopyFromStdinRow r -> Streaming.yield r <> Streaming.map sqlPieceToText rest
    --       _ -> do
    --         -- Ensure `rest` is empty for now.
    --         emptyResult <- Streaming.next rest
    --         case emptyResult of
    --           Right _ -> error "Impossible: stream should be empty!"
    --           _ -> pure $ Left p1
    -- flatStream :: Stream (Of [BatchedSqlStatements]) m ()
    -- flatStream =
    --   Streaming.mapped
    --   (\groupStream -> do
    --       ePieceRows <- singlePieceOrMultipleRows groupStream
    --       case ePieceRows of
    --         Left sp -> pure [ StandaloneSqlPiece sp ]
    --         Right rowsStream -> )
    --   rowsGroupedStream

    -- rowsGroupedStream :: Stream (Stream (Of SqlPiece) m) m ()
    -- rowsGroupedStream =
    --   Streaming.groupBy
    --     (\p1 p2 ->
    --       case (p1, p2) of
    --         (CopyFromStdinRow _, CopyFromStdinRow _) -> True
    --         _ -> False) s

runSingleStatementInternal_ :: MonadIO m => DB.Connection -> BatchedSqlStatements -> m ()
runSingleStatementInternal_ conn (StandaloneSqlPiece p) =
  case p of
    CommentPiece     _ -> pure ()
    WhiteSpacePiece  _ -> pure ()
    BeginTransaction s -> singleStatement_ conn s
    CommitTransaction s -> singleStatement_ conn s
    RollbackTransaction s -> singleStatement_ conn s
    OtherSqlPiece s -> singleStatement_ conn s
    CopyFromStdinStatement copyStm -> liftIO $ DB.copy_ conn $ DB.Query (encodeUtf8 copyStm)
    CopyFromStdinRow copyRow ->
      -- This should actually never happen due to batching
      liftIO $ DB.putCopyData conn $ encodeUtf8 copyRow
    CopyFromStdinEnd _ -> liftIO $ void $ DB.putCopyEnd conn
runSingleStatementInternal_ conn (BatchedCopyRows rows) = liftIO $ DB.putCopyData conn $ encodeUtf8 rows
