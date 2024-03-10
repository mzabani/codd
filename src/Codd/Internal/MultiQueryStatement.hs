module Codd.Internal.MultiQueryStatement
    ( SqlStatementException(..)
    , multiQueryStatement_
    , runSingleStatementInternal_
    , txnStatus
    ) where

import           Codd.Logging                   ( CoddLogger )
import           Codd.Parsing                   ( ParsedSql(..)
                                                , SqlPiece(..)
                                                )
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
import           Streaming                      ( Of(..)
                                                , Stream
                                                )
import qualified Streaming.Internal            as S
import qualified Streaming.Prelude             as Streaming
import qualified Streaming.Prelude             as S
import           UnliftIO                       ( Exception
                                                , MonadIO
                                                , MonadUnliftIO
                                                , handle
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

txnStatus :: MonadIO m => DB.Connection -> m PQ.TransactionStatus
txnStatus conn = liftIO $ PGInternal.withConnection conn PQ.transactionStatus

-- | Runs SQL that could be either a row-returning or count-returning statement. Unlike postgresql-simple, this does not throw exceptions in case of SQL errors, but rather returns them.
singleStatement_ :: MonadIO m => DB.Connection -> Text -> m StatementApplied
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
    -> ParsedSql m
    -> Stream (Of PQ.TransactionStatus) m (Maybe SqlStatementException)
multiQueryStatement_ conn sql =
    partitionEithersReturn id
        $ S.mapMaybe
              (\case
                  NotACountableStatement -> Nothing
                  StatementApplied ts    -> Just $ Right ts
                  StatementErred   e     -> Just $ Left e
              )
        $ case sql of
             -- There must be some simpler function to avoid mapM and yield..              
              UnparsedSql t -> S.mapM (singleStatement_ conn) (S.yield t)
              WellParsedSql stms ->
                  Streaming.mapM (runSingleStatementInternal_ conn) stms
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

runSingleStatementInternal_
    :: MonadIO m => DB.Connection -> SqlPiece -> m StatementApplied
runSingleStatementInternal_ _ (CommentPiece _) = pure NotACountableStatement
runSingleStatementInternal_ _ (WhiteSpacePiece _) = pure NotACountableStatement
runSingleStatementInternal_ conn (BeginTransaction s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CommitTransaction s) =
    singleStatement_ conn s
runSingleStatementInternal_ conn (RollbackTransaction s) =
    singleStatement_ conn s
runSingleStatementInternal_ conn (OtherSqlPiece s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CopyFromStdinStatement copyStm) = do
    liftIO $ DB.copy_ conn $ DB.Query (encodeUtf8 copyStm)
    StatementApplied <$> txnStatus conn
runSingleStatementInternal_ conn (CopyFromStdinRows copyRows) = do
    liftIO $ DB.putCopyData conn $ encodeUtf8 copyRows
    pure NotACountableStatement
runSingleStatementInternal_ conn (CopyFromStdinEnd _) = do
    liftIO $ void $ DB.putCopyEnd conn
    pure NotACountableStatement
