module Codd.Internal.MultiQueryStatement
    ( InTransaction(..)
    , SqlStatementException(..)
    , multiQueryStatement_
    , runSingleStatementInternal_
    ) where

import           Codd.Internal.Retry            ( retry_ )
import           Codd.Logging                   ( MonadLogger )
import           Codd.Parsing                   ( ParsedSql(..)
                                                , SqlPiece(..)
                                                )
import           Codd.Types                     ( RetryPolicy )
import           Control.Monad                  ( void )
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

data SqlStatementException = SqlStatementException
    { sqlStatement :: Text
    , psimpleError :: DB.SqlError
    }
    deriving stock Show

instance Exception SqlStatementException

-- | Runs SQL that could be either a row-returning or count-returning statement. Just like postgresql-simple, throws exceptions in case of SQL errors.
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
    :: (MonadUnliftIO m, MonadLogger m)
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
        flip Streaming.mapM_ stms
            $ \stm -> retry_ retryPol $ runSingleStatementInternal_ conn stm
    (WellParsedSql stms, InTransaction) ->
      -- We don't retry individual statements in in-txn migrations
                                           flip Streaming.mapM_ stms
        $ \stm -> runSingleStatementInternal_ conn stm

runSingleStatementInternal_ :: MonadIO m => DB.Connection -> SqlPiece -> m ()
runSingleStatementInternal_ _    (CommentPiece     _) = pure ()
runSingleStatementInternal_ _    (WhiteSpacePiece  _) = pure ()
runSingleStatementInternal_ conn (BeginTransaction s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CommitTransaction s) =
    singleStatement_ conn s
runSingleStatementInternal_ conn (RollbackTransaction s) =
    singleStatement_ conn s
runSingleStatementInternal_ conn (OtherSqlPiece s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CopyFromStdinStatement copyStm) =
    liftIO $ DB.copy_ conn $ DB.Query (encodeUtf8 copyStm)
runSingleStatementInternal_ conn (CopyFromStdinRows copyRows) =
    liftIO $ DB.putCopyData conn $ encodeUtf8 copyRows
runSingleStatementInternal_ conn (CopyFromStdinEnd _) =
    liftIO $ void $ DB.putCopyEnd conn
