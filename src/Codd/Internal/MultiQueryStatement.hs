module Codd.Internal.MultiQueryStatement
  ( SqlStatementException (..),
    StatementApplied (..),
    applyStatementStream,
    runSingleStatementInternal_,
    singleStatement_,
    skipCountableRunnableStatements,
    forceStreamConcurrently,
    -- Exported for tests
    forceStreamConcurrentlyInspect,
  )
where

import Codd.Logging (CoddLogger)
import Codd.Parsing (SqlPiece (..))
import Codd.Query (txnStatus)
import Control.Exception (BlockedIndefinitelyOnSTM)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Copy as DB
import qualified Database.PostgreSQL.Simple.Internal as PGInternal
import qualified Database.PostgreSQL.Simple.Types as DB
import GHC.Num (Natural)
import Streaming
  ( Of (..),
    Stream,
  )
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S
import qualified Streaming.Prelude as Streaming
import UnliftIO
  ( Exception,
    IOException,
    MonadUnliftIO,
    SomeException,
    fromException,
    handle,
    handleJust,
    liftIO,
    throwIO,
    try,
  )
import UnliftIO.Concurrent (MVar, forkIO, isEmptyMVar, putMVar)
import qualified UnliftIO.STM as STM
import Prelude hiding (takeWhile)

data SqlStatementException = SqlStatementException
  { sqlStatement :: Text,
    psimpleError :: DB.SqlError
  }
  deriving stock (Show)

instance Exception SqlStatementException

data StatementApplied
  = -- | Some SQL is comprised only of comments and white space. That is valid and parsed SQL that will make it to the functions in this module, but it's important we recognize them so we don't send them to postgres, and so our callers, who wish to count real applied statements, can do exactly that.
    -- This counting is necessary to resume failed no-txn migration application from the right statement if necessary, and while for that end we could count white space and comments, those are much more prone to change in our custom parser than actual individual SQL statements. Plus, they would make no sense to a human manually checking them.
    NotACountableStatement
  | -- | Contains the transaction state _after_ the statement was applied.
    StatementApplied PQ.TransactionStatus
  | StatementErred SqlStatementException

-- | Runs SQL that could be either a row-returning or count-returning statement. Unlike postgresql-simple, this does not throw exceptions in case of SQL errors, but rather returns them.
singleStatement_ ::
  (MonadUnliftIO m) => DB.Connection -> Text -> m StatementApplied
singleStatement_ conn sql = do
  res <- liftIO $ PGInternal.exec conn $ encodeUtf8 sql
  status <- liftIO $ PQ.resultStatus res
  case status of
    PQ.CommandOk -> StatementApplied <$> txnStatus conn
    PQ.TuplesOk -> StatementApplied <$> txnStatus conn
    _ ->
      liftIO $
        handle (pure . StatementErred . SqlStatementException sql) $
          -- Throw to catch and with that get the statement that failed and error message.. a bit nasty, but should be ok
          PGInternal.throwResultError "singleStatement_" res status

-- | Returns a Stream that should be a copy of the supplied stream, but puts a background thread to work on forcing the supplied stream concurrently so the consumer of the returned stream is more likely to find elements without having to wait for them to be produced by upstream.
-- May evaluate and keep in memory elements ahead of time up to the supplied number+2.
-- Make sure to consume the returned Stream entirely/linearly. Read more caveats below.
-- - If forcing effects concurrently throws an exception, the caller will only see that exception if they
--   consume the returned stream sufficiently.
-- - Even without exceptions, if the caller doesn't consume the returned stream completely, the behaviour is
--   then different from what it'd be without using this function, since future side-effects will have been applied.
-- - Since side effects can depend on the state of the World and this alters the order in which side effects run,
--   the entire program can have its behaviour changed.
forceStreamConcurrently :: forall m a r. (MonadUnliftIO m) => Natural -> Stream (Of a) m r -> Stream (Of a) m r
forceStreamConcurrently = forceStreamConcurrentlyInspect Nothing

data NoMoreConsumersOfTheReturnedStreamException = NoMoreConsumersOfTheReturnedStreamException
  deriving stock (Show)

instance Exception NoMoreConsumersOfTheReturnedStreamException

forceStreamConcurrentlyInspect ::
  forall m a r.
  (MonadUnliftIO m) =>
  -- | Supply an empty MVar that will be written to iff the stream returned by this function is not consumed linearly
  Maybe (MVar ()) ->
  Natural ->
  Stream (Of a) m r ->
  Stream (Of a) m r
forceStreamConcurrentlyInspect returnedStreamNotConsumedLinearly futureElementsQueueSize stream = S.Effect $ do
  case returnedStreamNotConsumedLinearly of
    Nothing -> pure ()
    Just mv -> unlessM (isEmptyMVar mv) $ error "Please supply an empty MVar to store the background thread's exit state"
  evaluatedElements :: STM.TBQueue (Either SomeException (Either r a)) <- STM.newTBQueueIO (futureElementsQueueSize + 1)
  void $ forkIO $ do
    streamReturnOrException <-
      try $
        S.mapsM_
          ( \(el :> eff) -> do
              -- If the stream returned by this function isn't linearly consumed, garbage collection will make
              -- writing to the TBQueue eventually throw a `BlockedIndefinitelyOnSTM` exception as this background thread
              -- will be the only one still holding on to the associated TVars.
              -- When that happens we don't want to try and write to the TBQueue again as it'd block forever. We just want to
              -- exit gracefully.
              handleJust (\(_ :: BlockedIndefinitelyOnSTM) -> Just ()) (\() -> throwIO NoMoreConsumersOfTheReturnedStreamException) $ STM.atomically $ STM.writeTBQueue evaluatedElements (Right $ Right el)
              pure eff
          )
          stream
    case streamReturnOrException of
      Left (ex :: SomeException) -> do
        case fromException ex of
          Just (_ :: NoMoreConsumersOfTheReturnedStreamException) ->
            -- We don't rethrow to avoid top-level exception handlers from detecting this, which is handled behavior after all
            case returnedStreamNotConsumedLinearly of
              Nothing -> pure ()
              Just mv ->
                putMVar mv ()
          Nothing ->
            STM.atomically $ STM.writeTBQueue evaluatedElements (Left ex)
      Right streamReturn ->
        STM.atomically $ STM.writeTBQueue evaluatedElements (Right $ Left streamReturn)

  pure $ go evaluatedElements
  where
    unlessM f g =
      f >>= \case
        True -> pure ()
        False -> g
    go :: STM.TBQueue (Either SomeException (Either r a)) -> Stream (Of a) m r
    go evaluatedElements = S.Effect $ do
      nextEl <- STM.atomically $ STM.readTBQueue evaluatedElements
      pure $ case nextEl of
        Right (Right el) -> S.Step (el :> go evaluatedElements)
        Right (Left r) -> S.Return r
        Left ex -> S.Effect $ throwIO ex

-- | Returns a Stream that applies each SqlPiece and that returns the transaction's status
-- after each applied statement, until either every statement is applied or one of them
-- throws an exception, which will be in the return value of the stream.
-- The returned stream does not contain statements that aren't countable-runnable.
applyStatementStream ::
  forall m.
  (MonadUnliftIO m, CoddLogger m) =>
  DB.Connection ->
  Stream (Of SqlPiece) m () ->
  Stream (Of PQ.TransactionStatus) m (Maybe SqlStatementException)
applyStatementStream conn sql =
  partitionEithersReturn id
    $ S.mapMaybe
      ( \case
          NotACountableStatement -> Nothing
          StatementApplied ts -> Just $ Right ts
          StatementErred e -> Just $ Left e
      )
    $ Streaming.mapM
      (runSingleStatementInternal_ conn)
      sql
  where
    -- \| Like `S.partitionEithers`, but with `Left` being considered an error and put as the stream's return value if one exists while the `Rights` are streamed.
    partitionEithersReturn ::
      (a -> Either e s) -> Stream (Of a) m r -> Stream (Of s) m (Maybe e)
    partitionEithersReturn f = \case
      S.Step (el :> rest) -> case f el of
        Left err -> S.Return $ Just err
        Right x -> S.Step $ x :> partitionEithersReturn f rest
      S.Effect m -> S.Effect $ partitionEithersReturn f <$> m
      S.Return _ -> S.Return Nothing

isCountableRunnable :: SqlPiece -> Bool
isCountableRunnable = \case
  OtherSqlPiece _ -> True
  CommentPiece _ -> False
  WhiteSpacePiece _ -> False
  BeginTransaction _ -> True
  CommitTransaction _ -> True
  RollbackTransaction _ -> True
  CopyFromStdinStatement _ -> False
  CopyFromStdinRows _ -> False
  CopyFromStdinEnd _ -> True

-- | Skips the first N countable-runnable statements from the stream and any non-countable-runnable pieces
-- like white space or comments so that the next piece in the stream is the (N+1)th runnable statement
-- in the original stream.
skipCountableRunnableStatements ::
  (Monad m) => Int -> Stream (Of SqlPiece) m r -> Stream (Of SqlPiece) m r
skipCountableRunnableStatements numCountableRunnableToSkip =
  S.catMaybes
    . S.scan
      ( \(skipped, _) p ->
          if skipped >= numCountableRunnableToSkip
            then (skipped, Just p)
            else
              if isCountableRunnable p
                then (skipped + 1, Nothing)
                else (skipped, Nothing)
      )
      (0, Nothing)
      snd

runSingleStatementInternal_ ::
  (MonadUnliftIO m) => DB.Connection -> SqlPiece -> m StatementApplied
runSingleStatementInternal_ conn p = case p of
  CommentPiece _ -> applied
  WhiteSpacePiece _ -> applied
  BeginTransaction s -> singleStatement_ conn s
  CommitTransaction s -> singleStatement_ conn s
  RollbackTransaction s -> singleStatement_ conn s
  OtherSqlPiece s -> singleStatement_ conn s
  CopyFromStdinStatement copyStm ->
    liftIO $
      handleCopyErrors (Just copyStm) $
        DB.copy_ conn (DB.Query (encodeUtf8 copyStm))
          >> applied
  CopyFromStdinRows copyRows ->
    -- I haven't seen errors coming from sending rows to the server yet; it seems they only happen
    -- on `putCopyEnd` or the initial `copy_`. Still, let's be cautious and prepare for it.
    liftIO $
      handleCopyErrors Nothing $
        DB.putCopyData conn (encodeUtf8 copyRows)
          >> applied
  CopyFromStdinEnd _ ->
    liftIO $ handleCopyErrors Nothing $ DB.putCopyEnd conn >> applied
  where
    handleCopyErrors (fromMaybe "" -> stmt) =
      handleJust
        ( \(e :: SomeException) ->
            -- In my tests, COPY errors are of type `IOException` for `putCopyEnd` and of type `SqlError` for `copy_`.
            -- Sadly the ones of type `IOException` don't contain e.g. error codes, but at least their message shows the failed statement.
            -- They also _sometimes_ contain an internal postgresql-simple error concatenated to the actual database error, which isn't great, so we remove it if it's there.
            -- We should file a bug report to postgresql-simple.
            -- We transform those into `SqlError` here since all of the codebase is prepared for that.
            case () of
              ()
                | Just sqlEx <- fromException @DB.SqlError e ->
                    Just sqlEx
                | Just ioEx <- fromException @IOException e ->
                    let fullError = Text.pack $ show ioEx
                     in Just
                          DB.SqlError
                            { DB.sqlState = "",
                              DB.sqlExecStatus = DB.FatalError,
                              DB.sqlErrorMsg =
                                encodeUtf8 $
                                  fromMaybe fullError $
                                    Text.stripPrefix
                                      "user error (Database.PostgreSQL.Simple.Copy.putCopyEnd: failed to parse command status\nConnection error: ERROR:  "
                                      fullError,
                              DB.sqlErrorDetail = "",
                              DB.sqlErrorHint = ""
                            }
                | otherwise ->
                    Nothing -- Let it blow up if we don't expect it
        )
        (pure . StatementErred . SqlStatementException stmt)
    applied :: (MonadUnliftIO n) => n StatementApplied
    applied =
      if isCountableRunnable p
        then StatementApplied <$> txnStatus conn
        else pure NotACountableStatement
