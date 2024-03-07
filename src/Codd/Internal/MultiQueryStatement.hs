module Codd.Internal.MultiQueryStatement
    ( InTransaction(..)
    , SqlStatementException(..)
    , multiQueryStatement_
    , runSingleStatementInternal_
    ) where

import           Codd.Internal.Retry            ( retry_ )
import           Codd.Logging                   ( CoddLogger )
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
import           Streaming                      ( Of
                                                , Stream
                                                )
import qualified Streaming.Prelude             as Streaming
import qualified Streaming.Prelude             as S
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


txnStatus :: MonadIO m => DB.Connection -> m PQ.TransactionStatus
txnStatus conn = liftIO $ PGInternal.withConnection conn PQ.transactionStatus

-- | Runs SQL that could be either a row-returning or count-returning statement. Just like postgresql-simple, throws exceptions in case of SQL errors.
singleStatement_
    :: MonadIO m => DB.Connection -> Text -> m PQ.TransactionStatus
singleStatement_ conn sql = do
    res    <- liftIO $ PGInternal.exec conn $ encodeUtf8 sql
    status <- liftIO $ PQ.resultStatus res
    case status of
        PQ.CommandOk -> txnStatus conn
        PQ.TuplesOk  -> txnStatus conn
        _ ->
            liftIO
                $ handle (throwIO . SqlStatementException sql)
                $
              -- Throw to catch and re-throw.. a bit nasty, but should be ok
                  PGInternal.throwResultError "singleStatement_" res status

data InTransaction = InTransaction | NotInTransaction RetryPolicy deriving stock (Eq)

-- data StatementApplied = WhitespaceOrCommentSkipped | StatementApplied PQ.TransactionStatus

-- isNotCommentOrWhiteSpace :: StatementApplied -> Bool
-- isNotCommentOrWhiteSpace (StatementApplied _) = True
-- isNotCommentOrWhiteSpace _                    = False

-- | A bit like singleStatement_, but following these criteria:
--   1. If already in a transaction, then statements will be executed one-by-one.
--   2. If not in a transaction, then statements will be executed one-by-one and
--      will be retried according to the retry policy.
multiQueryStatement_
    :: (MonadUnliftIO m, CoddLogger m)
    => InTransaction
    -> DB.Connection
    -> ParsedSql m
    -> Stream (Of (Either Text SqlPiece)) m ()
multiQueryStatement_ inTxn conn sql = case (sql, inTxn) of
    (UnparsedSql t, InTransaction) ->
        S.mapM (fmap (const $ Left t) . singleStatement_ conn) (S.yield t) -- There must be some simpler function to avoid mapM and yield..
    (UnparsedSql t, NotInTransaction retryPol) -> S.mapM
        (fmap (const $ Left t) . retry_ retryPol . singleStatement_ conn)
        (S.yield t)
    (WellParsedSql stms, NotInTransaction retryPol) ->
      -- We retry individual statements in no-txn migrations
        flip Streaming.mapM stms $ \stm ->
            fmap (const $ Right stm)
                $ retry_ retryPol
                $ runSingleStatementInternal_ conn stm
    (WellParsedSql stms, InTransaction) ->
      -- We don't retry individual statements in in-txn migrations
                                           Streaming.mapM
        (\stm -> Right stm <$ runSingleStatementInternal_ conn stm)
        stms

runSingleStatementInternal_
    :: MonadIO m => DB.Connection -> SqlPiece -> m PQ.TransactionStatus
runSingleStatementInternal_ conn (CommentPiece     _) = txnStatus conn
runSingleStatementInternal_ conn (WhiteSpacePiece  _) = txnStatus conn
runSingleStatementInternal_ conn (BeginTransaction s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CommitTransaction s) =
    singleStatement_ conn s
runSingleStatementInternal_ conn (RollbackTransaction s) =
    singleStatement_ conn s
runSingleStatementInternal_ conn (OtherSqlPiece s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CopyFromStdinStatement copyStm) = do
    liftIO $ DB.copy_ conn $ DB.Query (encodeUtf8 copyStm)
    txnStatus conn
runSingleStatementInternal_ conn (CopyFromStdinRows copyRows) = do
    liftIO $ DB.putCopyData conn $ encodeUtf8 copyRows
    txnStatus conn
runSingleStatementInternal_ conn (CopyFromStdinEnd _) = do
    liftIO $ void $ DB.putCopyEnd conn
    txnStatus conn
