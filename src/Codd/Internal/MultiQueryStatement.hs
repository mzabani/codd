module Codd.Internal.MultiQueryStatement
    ( SqlStatementException(..)
    , StatementApplied(..)
    , multiQueryStatement_
    , runSingleStatementInternal_
    , singleStatement_
    , skipCountableRunnableStatements
    ) where

import           Codd.Logging                   ( CoddLogger )
import           Codd.Parsing                   ( SqlPiece(..) )
import           Codd.Query                     ( txnStatus )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
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
import           Streaming                      ( Of(..)
                                                , Stream
                                                )
import qualified Streaming.Internal            as S
import qualified Streaming.Prelude             as Streaming
import qualified Streaming.Prelude             as S
import           UnliftIO                       ( Exception
                                                , IOException
                                                , MonadUnliftIO
                                                , SomeException
                                                , fromException
                                                , handle
                                                , handleJust
                                                , liftIO
                                                )

data SqlStatementException = SqlStatementException
    { sqlStatement :: Text
    , psimpleError :: DB.SqlError
    }
    deriving stock Show
instance Exception SqlStatementException

data StatementApplied = NotACountableStatement
    -- ^ Some SQL is comprised only of comments and white space. That is valid and parsed SQL that will make it to the functions in this module, but it's important we recognize them so we don't send them to postgres, and so our callers, who wish to count real applied statements, can do exactly that.
    -- This counting is necessary to resume failed no-txn migration application from the right statement if necessary, and while for that end we could count white space and comments, those are much more prone to change in our custom parser than actual individual SQL statements. Plus, they would make no sense to a human manually checking them.
     | StatementApplied PQ.TransactionStatus
     -- ^ Contains the transaction state _after_ the statement was applied.
     | StatementErred SqlStatementException


-- | Runs SQL that could be either a row-returning or count-returning statement. Unlike postgresql-simple, this does not throw exceptions in case of SQL errors, but rather returns them.
singleStatement_
    :: MonadUnliftIO m => DB.Connection -> Text -> m StatementApplied
singleStatement_ conn sql = do
    res    <- liftIO $ PGInternal.exec conn $ encodeUtf8 sql
    status <- liftIO $ PQ.resultStatus res
    case status of
        PQ.CommandOk -> StatementApplied <$> txnStatus conn
        PQ.TuplesOk  -> StatementApplied <$> txnStatus conn
        _ ->
            liftIO
                $ handle (pure . StatementErred . SqlStatementException sql)
                $
              -- Throw to catch and with that get the statement that failed and error message.. a bit nasty, but should be ok
                  PGInternal.throwResultError "singleStatement_" res status

-- | Returns a Stream with the transaction status after each applied statement, until either every statement
-- is applied or one of them throws an exception, which will be in the return value of the stream.
-- The returned stream does not contain statements that aren't meant to be counted.
multiQueryStatement_
    :: forall m
     . (MonadUnliftIO m, CoddLogger m)
    => DB.Connection
    -> Stream (Of SqlPiece) m ()
    -> Stream (Of PQ.TransactionStatus) m (Maybe SqlStatementException)
multiQueryStatement_ conn sql =
    partitionEithersReturn id
        $ S.mapMaybe
              (\case
                  NotACountableStatement -> Nothing
                  StatementApplied ts    -> Just $ Right ts
                  StatementErred   e     -> Just $ Left e
              )
        $ Streaming.mapM (runSingleStatementInternal_ conn) sql
  where
    -- | Like `S.partitionEithers`, but with `Left` being considered an error and put as the stream's return value if one exists while the `Rights` are streamed.
    partitionEithersReturn
        :: (a -> Either e s) -> Stream (Of a) m r -> Stream (Of s) m (Maybe e)
    partitionEithersReturn f = \case
        S.Step (el :> rest) -> case f el of
            Left  err -> S.Return $ Just err
            Right x   -> S.Step $ x :> partitionEithersReturn f rest
        S.Effect m -> S.Effect $ partitionEithersReturn f <$> m
        S.Return _ -> S.Return Nothing

isCountableRunnable :: SqlPiece -> Bool
isCountableRunnable = \case
    OtherSqlPiece          _ -> True
    CommentPiece           _ -> False
    WhiteSpacePiece        _ -> False
    BeginTransaction       _ -> True
    CommitTransaction      _ -> True
    RollbackTransaction    _ -> True
    CopyFromStdinStatement _ -> False
    CopyFromStdinRows      _ -> False
    CopyFromStdinEnd       _ -> True

-- | Skips the first N countable-runnable statements from the stream and any non-countable-runnable pieces
-- like white space or comments so that the next piece in the stream is the (N+1)th runnable statement
-- in the original stream.
skipCountableRunnableStatements
    :: Monad m => Int -> Stream (Of SqlPiece) m r -> Stream (Of SqlPiece) m r
skipCountableRunnableStatements numCountableRunnableToSkip =
    S.catMaybes
        . S.scan
              (\(skipped, _) p -> if skipped >= numCountableRunnableToSkip
                  then (skipped, Just p)
                  else if isCountableRunnable p
                      then (skipped + 1, Nothing)
                      else (skipped, Nothing)
              )
              (0, Nothing)
              snd

runSingleStatementInternal_
    :: MonadUnliftIO m => DB.Connection -> SqlPiece -> m StatementApplied
runSingleStatementInternal_ conn p = case p of
    CommentPiece        _ -> applied
    WhiteSpacePiece     _ -> applied
    BeginTransaction    s -> singleStatement_ conn s
    CommitTransaction   s -> singleStatement_ conn s
    RollbackTransaction s -> singleStatement_ conn s
    OtherSqlPiece       s -> singleStatement_ conn s
    CopyFromStdinStatement copyStm ->
        liftIO
            $  handleCopyErrors (Just copyStm)
            $  DB.copy_ conn (DB.Query (encodeUtf8 copyStm))
            >> applied
    CopyFromStdinRows copyRows ->
        -- I haven't seen errors coming from sending rows to the server yet; it seems they only happen
        -- on `putCopyEnd` or the initial `copy_`. Still, let's be cautious and prepare for it.
        liftIO
            $  handleCopyErrors Nothing
            $  DB.putCopyData conn (encodeUtf8 copyRows)
            >> applied
    CopyFromStdinEnd _ ->
        liftIO $ handleCopyErrors Nothing $ DB.putCopyEnd conn >> applied
  where
    handleCopyErrors (fromMaybe "" -> stmt) = handleJust
        (\(e :: SomeException) ->
            -- In my tests, COPY errors are of type `IOException` for `putCopyEnd` and of type `SqlError` for `copy_`.
            -- Sadly the ones of type `IOException` don't contain e.g. error codes, but at least their message shows the failed statement.
            -- They also _sometimes_ contain an internal postgresql-simple error concatenated to the actual database error, which isn't great, so we remove it if it's there.
            -- We should file a bug report to postgresql-simple.
            -- We transform those into `SqlError` here since all of the codebase is prepared for that.
                                  case () of
            ()
                | Just sqlEx <- fromException @DB.SqlError e
                -> Just sqlEx
                | Just ioEx <- fromException @IOException e
                -> let fullError = Text.pack $ show ioEx
                   in
                       Just DB.SqlError
                           { DB.sqlState       = ""
                           , DB.sqlExecStatus  = DB.FatalError
                           , DB.sqlErrorMsg    =
                               encodeUtf8
                               $ fromMaybe fullError
                               $ Text.stripPrefix
                                     "user error (Database.PostgreSQL.Simple.Copy.putCopyEnd: failed to parse command status\nConnection error: ERROR:  "
                                     fullError
                           , DB.sqlErrorDetail = ""
                           , DB.sqlErrorHint   = ""
                           }
                | otherwise
                -> Nothing -- Let it blow up if we don't expect it
        )
        (pure . StatementErred . SqlStatementException stmt)
    applied :: MonadUnliftIO n => n StatementApplied
    applied = if isCountableRunnable p
        then StatementApplied <$> txnStatus conn
        else pure NotACountableStatement
