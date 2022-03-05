module Codd.Internal.MultiQueryStatement
  ( InTransaction(..)
  , SqlStatementException(..)
  , multiQueryStatement_
  , runSingleStatementInternal_
  ) where

import           Codd.Internal.Retry            ( retry )
import           Codd.Parsing                   ( ParsedSql(..)
                                                , SqlPiece(..)
                                                )
import           Codd.Types                     ( RetryPolicy )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Logger           ( MonadLogger )
import           Data.Text                      ( Text )
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
  (UnparsedSql _ t, InTransaction) -> singleStatement_ conn t
  (UnparsedSql _ t, NotInTransaction retryPol) ->
    retry retryPol $ singleStatement_ conn t
  (WellParsedSql stms, NotInTransaction retryPol) ->
    -- We retry individual statements in no-txn migrations
    flip Streaming.mapM_ stms $ \stm -> do
      liftIO $ print stm
      retry retryPol $ runSingleStatementInternal_ conn stm
  (WellParsedSql stms, InTransaction) ->
    -- We don't retry individual statements in in-txn migrations
                                         flip Streaming.mapM_ stms $ \stm -> do
    liftIO $ print stm
    runSingleStatementInternal_ conn stm

runSingleStatementInternal_ :: MonadIO m => DB.Connection -> SqlPiece -> m ()
runSingleStatementInternal_ _    (CommentPiece     _) = pure ()
runSingleStatementInternal_ _    (WhiteSpacePiece  _) = pure ()
runSingleStatementInternal_ conn (BeginTransaction s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CommitTransaction s) =
  singleStatement_ conn s
runSingleStatementInternal_ conn (RollbackTransaction s) =
  singleStatement_ conn s
runSingleStatementInternal_ conn (OtherSqlPiece s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CopyFromStdinPiece copyStm copyData _terminator)
  = liftIO $ do
    DB.copy_ conn $ DB.Query (encodeUtf8 copyStm)
    when (copyData /= "") $ do
      DB.putCopyData conn $ encodeUtf8 copyData
      DB.putCopyData conn $ encodeUtf8 "\n"
    void $ DB.putCopyEnd conn
