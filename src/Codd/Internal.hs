{-# LANGUAGE BlockArguments, AllowAmbiguousTypes #-}
module Codd.Internal where

import           Prelude                 hiding ( readFile )

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal.MultiQueryStatement
                                                ( SqlStatementException
                                                , StatementApplied(..)
                                                , multiQueryStatement_
                                                , singleStatement_
                                                , skipNonCountableRunnableStatements
                                                )
import           Codd.Internal.Retry            ( RetryIteration(..)
                                                , retryFold
                                                )
import           Codd.Logging                   ( CoddLogger
                                                , logError
                                                , logInfo
                                                , logInfoNoNewline
                                                , logWarn
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , AppliedMigration(..)
                                                , EnvVars(..)
                                                , FileStream(..)
                                                , ParsedSql(..)
                                                , SqlMigration(..)
                                                , hoistAddedSqlMigration
                                                , parseAddedSqlMigration
                                                , parseSqlPiecesStreaming
                                                , substituteEnvVarsInSqlPiecesStream
                                                )
import           Codd.Query                     ( CanStartTxn
                                                , InTxn
                                                , InTxnT
                                                , NotInTxn
                                                , execvoid_
                                                , query
                                                , txnStatus
                                                , unsafeQuery1
                                                , withTransaction
                                                )
import           Codd.Representations           ( DbRep
                                                , logSchemasComparison
                                                , readRepresentationsFromDbWithSettings
                                                )
import           Codd.Types                     ( TxnIsolationLvl(..) )
import           Control.Monad                  ( (>=>)
                                                , foldM
                                                , forM
                                                , forM_
                                                , unless
                                                , void
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import           Data.Functor                   ( (<&>) )
import qualified Data.List                     as List
import           Data.List                      ( sortOn )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromMaybe )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( DiffTime
                                                , NominalDiffTime
                                                , UTCTime
                                                , diffTimeToPicoseconds
                                                , picosecondsToDiffTime, secondsToDiffTime
                                                )
import qualified Database.PostgreSQL.LibPQ     as PQ
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import qualified Formatting                    as Fmt
import           Streaming                      ( Of(..) )
import qualified Streaming.Prelude             as Streaming
import           System.Clock                   ( Clock(Monotonic)
                                                , TimeSpec(..)
                                                , getTime
                                                )
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                )
import           UnliftIO                       ( Exception
                                                , MonadUnliftIO
                                                , hClose
                                                , newIORef
                                                , readIORef
                                                , timeout
                                                , writeIORef, try, SomeException
                                                )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Directory             ( listDirectory )
import           UnliftIO.Exception             ( IOException
                                                , bracket
                                                , catchJust
                                                , handleJust
                                                , throwIO
                                                , tryJust
                                                )
import           UnliftIO.IO                    ( IOMode(ReadMode)
                                                , openFile
                                                )
import           UnliftIO.MVar                  ( modifyMVar
                                                , newMVar
                                                , readMVar, modifyMVar_
                                                )
import           UnliftIO.Resource              ( MonadResource
                                                , ReleaseKey
                                                , ResourceT
                                                , allocate
                                                , release
                                                , runResourceT
                                                )
import Data.Bifunctor (second)

dbIdentifier :: Text -> DB.Query
dbIdentifier s = "\"" <> fromString (Text.unpack s) <> "\""

-- | Tries to connect until a connection succeeds or until a timeout, and returns a connection or throws an exception.
connectWithTimeout
    :: MonadUnliftIO m => DB.ConnectInfo -> DiffTime -> m DB.Connection
connectWithTimeout connStr timeLimit = do
    -- It feels bad using `timeout` for this given the asynchronous exception it can throw, but name resolution can block `DB.connect` for an arbitrary amount of time.
    -- The risk is we interrupt `DB.connect` after the operating system socket is open, which would mean we'd leave it hanging
    -- open for a while. This is probably not very probable, and not a terrible consequence as a single failing connection means
    -- we'll likely close codd itself pretty soon.
    -- The "decent" alternative (which I feel isn't worth it) here is to wrap postgresql-simple's `connectPostgreSQL` in a `onException` that closes the open handle on exception, or copy that code over ourselves and do that here. See https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/src/Database.PostgreSQL.Simple.Internal.html#connectPostgreSQL
    lastErrorRef <- newIORef (Nothing :: Maybe IOException)
    mconn        <- timeout
        (fromInteger $ diffTimeToPicoseconds timeLimit `div` 1_000_000)
        (tryConnect lastErrorRef)
    lastError <- readIORef lastErrorRef
    case (mconn, lastError) of
        (Just conn, _       ) -> pure conn
        (Nothing  , Just err) -> throwIO err
        (Nothing, Nothing) ->
            throwIO $ userError "Timeout connecting to database"
  where
    tryConnect lastErrorRef = do
        attempt <-
            tryJust
                    (\e -> if isServerNotAvailableError e
                        then Just e
                        else Nothing
                    )
                $ liftIO (DB.connect connStr)
        case attempt of
            Right conn -> pure conn
            Left  e    -> do
                writeIORef lastErrorRef (Just e)
                threadDelay 100_000 -- Wait 100ms before retrying
                tryConnect lastErrorRef

-- | Tries to connect until a connection succeeds or until a timeout,
--   executes the supplied action and disposes of the opened Connection.
withConnection
    :: MonadUnliftIO m
    => DB.ConnectInfo
    -> DiffTime
    -> (DB.Connection -> m a)
    -> m a
withConnection connStr timeLimit =
    bracket (connectWithTimeout connStr timeLimit) (liftIO . DB.close)

-- | Verifies if a libpq error means the server is not ready to accept connections yet,
-- either by not being listening at all or still being initializing.
isServerNotAvailableError :: IOException -> Bool
isServerNotAvailableError e =
    let err = Text.pack $ show e
    in  "libpq"
            `Text.isInfixOf` err
            && ("could not connect to server: Connection refused"
               `Text.isInfixOf` err
               || "Is the server running on that host and accepting"
               `Text.isInfixOf` err
               ||               "server closed the connection"
               `Text.isInfixOf` err
               ||               "the database system is starting up"
               `Text.isInfixOf` err
               )

-- | Returns true for errors such as "permission denied for database xxx"
isPermissionDeniedError :: DB.SqlError -> Bool
isPermissionDeniedError e = DB.sqlState e == "42501"

data BootstrapCheck = BootstrapCheck
    { defaultConnAccessible :: Bool
    , coddSchemaVersion     :: CoddSchemaVersion
    }

-- | Returns info on what kind of bootstrapping will be necessary,
-- waiting up to the time limit for postgres to be up before throwing
-- an exception.
checkNeedsBootstrapping
    :: MonadUnliftIO m => DB.ConnectInfo -> DiffTime -> m BootstrapCheck
checkNeedsBootstrapping connInfo connectTimeout =
    handleJust
            (\e ->
                -- 1. No server available is a big "No", meaning we throw an exception.
                   if isServerNotAvailableError e
                then Nothing

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -- 2. Maybe the default migration connection string doesn't work because:
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -- - The DB does not exist.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -- - CONNECT rights not granted.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -- - User doesn't exist.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -- In any case, it's best to be conservative and consider any libpq errors
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -- here as errors that might just require bootstrapping.
                else if isLibPqError e
                    then Just BootstrapCheck
                        { defaultConnAccessible = False
                        , coddSchemaVersion     = CoddSchemaDoesNotExist
                        }

                -- 3. Let other exceptions blow up
                    else Nothing
            )
            pure
        $ withConnection connInfo
                         connectTimeout
                         (fmap (BootstrapCheck True) . detectCoddSchema)
  where
    isLibPqError :: IOException -> Bool
    isLibPqError e =
        let err = Text.pack $ show e in "libpq: failed" `Text.isInfixOf` err

data PendingMigrations m = PendingMigrations
    { pendingMigs    :: [BlockOfMigrations m]
    , bootstrapCheck :: BootstrapCheck
    -- ^ More information on what kind of bootstrapping is necessary.
    }

collectAndApplyMigrations
    :: ( MonadUnliftIO m
       , CoddLogger m
       , MonadThrow m
       , EnvVars m
       , NotInTxn m
       , txn ~ InTxnT (ResourceT m)
       )
    => ([BlockOfMigrations txn] -> DB.Connection -> txn a)
    -> CoddSettings
    -> Maybe [AddedSqlMigration m]
    -- ^ Instead of collecting migrations from disk according to codd settings, use these if they're defined.
    -> DiffTime
    -> m a
collectAndApplyMigrations lastAction settings@CoddSettings { migsConnString, sqlMigrations, txnIsolationLvl } mOverrideMigs connectTimeout
    = do
        let dbName = Text.pack $ DB.connectDatabase migsConnString
        let waitTimeInSecs :: Double = realToFrac connectTimeout
        logInfo
            $  "Checking if database <MAGENTA>"
            <> dbName
            <> "</MAGENTA> is accessible with the configured connection string... (waiting up to <CYAN>"
            <> Text.pack (show @Int $ truncate waitTimeInSecs)
            <> "sec</CYAN>)"

        runResourceT $ do
            let migsToUse = maybe
                    (Left sqlMigrations)
                    (Right . map (hoistAddedSqlMigration lift))
                    mOverrideMigs
            pendingMigs <- collectPendingMigrations migsConnString
                                                    migsToUse
                                                    txnIsolationLvl
                                                    connectTimeout
            applyCollectedMigrations lastAction
                                     settings
                                     pendingMigs
                                     connectTimeout

-- | Applies the supplied migrations, running blocks of (in-txn, same-connection-string) migrations each within their own
-- separate transactions.
-- Behaviour here unfortunately is _really_ complex right now. Important notes:
-- - Iff there's a single (in-txn, same-connection-string) block of migrations *with* the default connection string,
--   then "actionAfter" runs in the same transaction (and thus in the same connection as well) as that block.
--   Otherwise it runs after all migrations, in an explicit transaction of
--   its own and in the default connection, which is not necessarily the same connection as the last migration's.
applyCollectedMigrations
    :: forall m a txn
     . ( MonadUnliftIO m
       , CoddLogger m
       , MonadResource m
       , NotInTxn m
       , txn ~ InTxnT m
       )
    => ([BlockOfMigrations txn] -> DB.Connection -> txn a)
    -> CoddSettings
    -> PendingMigrations m
    -> DiffTime
    -> m a
applyCollectedMigrations actionAfter CoddSettings { migsConnString = defaultConnInfo, retryPolicy, txnIsolationLvl } PendingMigrations { pendingMigs, bootstrapCheck } connectTimeout
    = do
  -- This function is complex because:
  -- 1. We need to open connections as late as possible due to bootstrapping.
  -- 2. We want to insert into codd_schema.sql_migrations as early as possible even for custom-connection migrations.
  -- 3. When possible, we want to insert into codd_schema.sql_migrations in the same transaction the migrations are running.
        let dbName = Text.pack $ DB.connectDatabase defaultConnInfo
            hoistedBlocks :: [BlockOfMigrations txn] =
                map (hoistBlockOfMigrations lift) pendingMigs

        -- Note: We could probably compound this Monad with StateT instead of using an MVar, but IIRC that creates issues
        -- with MonadUnliftIO.
        connsPerInfo <- newMVar (mempty :: [(DB.ConnectInfo, DB.Connection)])
        appliedMigs <- newMVar (mempty :: [(AppliedMigration, MigrationRegistered)])
        coddSchemaExists <- newMVar $ coddSchemaVersion bootstrapCheck /= CoddSchemaDoesNotExist
        let openConn :: DB.ConnectInfo -> m (ReleaseKey, DB.Connection)
            openConn cinfo = flip allocate DB.close $ do
                mConn <- lookup cinfo <$> readMVar connsPerInfo
                case mConn of
                    Just conn -> pure conn
                    Nothing   -> modifyMVar connsPerInfo $ \m -> do
                                        -- logInfo $ "Connecting to (TODO: REDACT PASSWORD) " <> Text.pack (show cinfo)
                        conn <- connectWithTimeout cinfo connectTimeout
                        pure ((cinfo, conn) : m, conn)

            queryConn :: DB.ConnectInfo -> m (Maybe DB.Connection)
            queryConn cinfo = lookup cinfo <$> readMVar connsPerInfo

            createCoddSchemaAndFlushPendingMigrations :: m ()
            createCoddSchemaAndFlushPendingMigrations = do
                mDefaultConn <- queryConn defaultConnInfo
                csExists <- readMVar coddSchemaExists
                case mDefaultConn of
                    Just defaultConn -> do
                        unless csExists $ do
                            logInfo
                                "Creating or updating <MAGENTA>codd_schema</MAGENTA>..."
                            createCoddSchema @txn
                                maxBound
                                txnIsolationLvl
                                defaultConn
                            modifyMVar_ coddSchemaExists $ const $ pure True
                        modifyMVar_ appliedMigs $ \apmigs -> do
                            withTransaction @txn txnIsolationLvl defaultConn $
                                forM_ [ am | (am, MigrationNotRegistered) <- apmigs ]
                                    $ \AppliedMigration {..} -> registerRanMigration @txn
                                          defaultConn
                                          txnIsolationLvl
                                          appliedMigrationName
                                          appliedMigrationTimestamp
                                          (SpecificTime appliedMigrationAt)
                                          appliedMigrationDuration
                                          appliedMigrationNumStatements
                                          MigrationAppliedSuccessfully -- TODO: What if a failing no-txn migration ran just enough to make the default connection string accessible? Then we want to register it properly here, not assume it succeeded.
                            pure $ map (second (const MigrationRegistered)) apmigs
                    Nothing -> pure ()

            -- | The function used to register applied migrations for when a block of in-txn migrations using the default connection string are to be applied.
            -- This will use same transaction as the one used to apply the migrations to insert into codd_schema.sql_migrations.
            registerAppliedMigDefaultConnInTxnBlock :: FilePath -> DB.UTCTimestamp -> DiffTime -> Int -> MigrationApplicationStatus -> txn ()
            registerAppliedMigDefaultConnInTxnBlock appliedMigrationName appliedMigrationTimestamp appliedMigrationDuration appliedMigrationNumStatements apStatus = do
                (_, defaultConn) <- lift $ openConn defaultConnInfo
                timeApplied <- registerRanMigration @txn defaultConn txnIsolationLvl appliedMigrationName appliedMigrationTimestamp NowInPostgresTime appliedMigrationDuration appliedMigrationNumStatements apStatus

                modifyMVar_ appliedMigs $ \apmigs -> pure $ apmigs ++ [(AppliedMigration { appliedMigrationName
                      , appliedMigrationTimestamp
                      , appliedMigrationAt = timeApplied
                      , appliedMigrationDuration
                      , appliedMigrationNumStatements
                      }, MigrationRegistered)]

            -- | The function used to register applied migrations for when either a no-txn migration or a block of in-txn migrations _not_ using the default connection string are to be applied.
            -- This will account for the possibility that the default connection string still isn't accessible by storing in-memory that some migrations were applied but not registered,
            -- and also will BEGIN..COMMIT-wrap the insertion using the default connection if it's available.
            registerAppliedMigIfPossibleOthers :: DB.Connection -> FilePath -> DB.UTCTimestamp -> DiffTime -> Int -> MigrationApplicationStatus -> m ()
            registerAppliedMigIfPossibleOthers blockConn appliedMigrationName appliedMigrationTimestamp appliedMigrationDuration appliedMigrationNumStatements apStatus = do
                csExists <- readMVar coddSchemaExists
                case (apStatus, csExists) of
                    (NoTxnMigrationFailed, False) -> do
                        -- Super duper ultra extra special case: we try to create codd_schema as a partially-run no-txn migration may have applied statements that make the default connection string accessible. The same isn't possible with in-txn migrations.
                        -- This will increase the delay between retry intervals beyond what the user has specified since we're adding a bit of a timeout to each retry of failed no-txn migrations. Since this is an extremely rare error case, it probably doesn't
                        -- matter too much what we do. I have questions if we should even support this, to be honest. Hacky stuff below:
                        void $ try @m @SomeException $ withConnection defaultConnInfo (min 0.3 connectTimeout) $ \_conn -> do
                            void $ openConn defaultConnInfo
                            createCoddSchemaAndFlushPendingMigrations
                    _ -> pure ()
                mDefaultConn <- queryConn defaultConnInfo
                (registered, appliedAt) <-
                    case mDefaultConn of
                        Nothing -> fmap (MigrationNotRegistered,) $ 
                                                    DB.fromOnly <$> unsafeQuery1 blockConn "SELECT clock_timestamp()" ()
                        Just defaultConn -> do
                            timeApplied <- withTransaction @txn txnIsolationLvl defaultConn $ registerRanMigration @txn defaultConn txnIsolationLvl appliedMigrationName appliedMigrationTimestamp NowInPostgresTime appliedMigrationDuration appliedMigrationNumStatements apStatus
                            pure (MigrationRegistered, timeApplied)

                modifyMVar_ appliedMigs $ \apmigs -> pure $ apmigs ++ [(AppliedMigration { appliedMigrationName
                      , appliedMigrationTimestamp
                      , appliedMigrationAt = appliedAt
                      , appliedMigrationDuration
                      , appliedMigrationNumStatements
                      }, registered)]
                

        singleInTxnBlockResult :: Maybe a <-
            foldM
                (\_ block -> do
                    let
                        cinfo = fromMaybe defaultConnInfo
                                          (blockCustomConnInfo block)
                    (_, conn)                   <- openConn cinfo


                    -- Create codd_schema and flush previously applied migrations if possible. We do this here
                    -- since we expect _some_ of the migration blocks to use the default connection string, and after
                    -- that we can register migrations were applied.
                    createCoddSchemaAndFlushPendingMigrations

                    case
                        ( block
                        , isOneShotApplication defaultConnInfo pendingMigs
                        )
                        of
                        (BlockInTxn inTxnBlock, True) -> runInTxnBlockDefaultConn
                            (fmap Just . actionAfter hoistedBlocks)
                            conn
                            inTxnBlock
                            registerAppliedMigDefaultConnInTxnBlock
                        (BlockInTxn inTxnBlock, False) -> if cinfo == defaultConnInfo then runInTxnBlockDefaultConn (const $ pure Nothing) conn inTxnBlock registerAppliedMigDefaultConnInTxnBlock else runInTxnBlockNotDefaultConn
                            (const $ pure Nothing)
                            conn
                            inTxnBlock
                            (registerAppliedMigIfPossibleOthers conn)
                        (BlockNoTxn noTxnBlock, _) -> runNoTxnMig
                            conn
                            noTxnBlock
                            (registerAppliedMigIfPossibleOthers conn)

                )
                Nothing
                pendingMigs

        actAfterResult <- case singleInTxnBlockResult of
            Just result -> pure result
            Nothing     -> do
              -- It is possible to have only non-default-connection-string migrations.
              -- In that case, we assume the default-connection-string will be valid after those migrations
              -- and use that to register all applied migrations and then run "actionAfter".
                (_, defaultConn) <- openConn defaultConnInfo
                createCoddSchemaAndFlushPendingMigrations
                withTransaction txnIsolationLvl defaultConn
                    $ actionAfter hoistedBlocks defaultConn

        logInfo
            $ "<GREEN>Successfully</GREEN> applied all migrations to </GREEN><MAGENTA>"
            <> dbName
            <> "</MAGENTA>"
        return actAfterResult

  where
    runInTxnBlockNotDefaultConn
        :: (DB.Connection -> txn b)
        -> DB.Connection
        -> ConsecutiveInTxnMigrations m
        -> (FilePath -> DB.UTCTimestamp -> DiffTime -> Int -> MigrationApplicationStatus -> m ()) -- ^ Running in the `m` monad is correct. In-txn migrations can run inside a transaction in a connection that is not the default one, and that's what `txn` would be here: a transaction _possibly_ in a connection different than the one that will be used to apply migrations.
        -> m b
    runInTxnBlockNotDefaultConn act conn migBlock registerMig = do
        res <-
         -- Naturally, we retry entire in-txn block transactions on error, not individual statements or individual migrations
            retryFold @MigrationApplicationFailure
                    retryPolicy
                    (\previousBlock RetryIteration { tryNumber } ->
                        if tryNumber == 0
                            then pure previousBlock
                            else reReadBlock previousBlock
                    )
                    migBlock
                    (\case
                        Left lastEx -> do
                            logError
                                "Failed after all configured retries. Giving up."
                            throwIO lastEx
                        Right ret -> pure ret
                    )
                $ \blockFinal -> do
                      logInfo "<MAGENTA>BEGIN</MAGENTA>ning transaction"
                      withTransaction @txn txnIsolationLvl conn
                          $ do
                                let hoistedMigs
                                        :: NonEmpty (AddedSqlMigration txn)
                                    hoistedMigs =
                                        hoistAddedSqlMigration lift
                                            <$> inTxnMigs blockFinal
                                errorOrOk <- forMExcept hoistedMigs
                                            $ applySingleMigration conn
                                                                   (\fp ts duration numStmts apStatus -> lift $ registerMig fp ts duration numStmts apStatus)
                                                                   0
                                case errorOrOk of
                                    Left e -> do
                                        logInfo
                                          "<MAGENTA>ROLLBACK</MAGENTA>ed transaction"
                                        pure $ Left e
                                    Right () -> 
                                        Right <$> act conn
        logInfo "<MAGENTA>COMMIT</MAGENTA>ed transaction"
        pure res

    runInTxnBlockDefaultConn
        :: (DB.Connection -> txn b)
        -> DB.Connection
        -> ConsecutiveInTxnMigrations m
        -> (FilePath -> DB.UTCTimestamp -> DiffTime -> Int -> MigrationApplicationStatus -> txn ()) -- ^ Using the `txn` is right: registering applied migrations happens in the default connection, and so it will happen in the same transaction as the migrations themselves.
        -> m b
    runInTxnBlockDefaultConn act conn migBlock registerMig = do
        res <-
         -- Naturally, we retry entire in-txn block transactions on error, not individual statements or individual migrations
            retryFold @MigrationApplicationFailure
                    retryPolicy
                    (\previousBlock RetryIteration { tryNumber } ->
                        if tryNumber == 0
                            then pure previousBlock
                            else reReadBlock previousBlock
                    )
                    migBlock
                    (\case
                        Left lastEx -> do
                            logError
                                "Failed after all configured retries. Giving up."
                            throwIO lastEx
                        Right ret -> pure ret
                    )
                $ \blockFinal -> do
                      logInfo "<MAGENTA>BEGIN</MAGENTA>ning transaction"
                      withTransaction txnIsolationLvl conn
                          $ do
                                let hoistedMigs
                                        :: NonEmpty (AddedSqlMigration txn)
                                    hoistedMigs =
                                        hoistAddedSqlMigration lift
                                            <$> inTxnMigs blockFinal
                                errorOrOk <- forMExcept hoistedMigs
                                    $ applySingleMigration conn
                                                           registerMig
                                                           0
                                case errorOrOk of
                                    Left e -> do
                                        logInfo
                                          "<MAGENTA>ROLLBACK</MAGENTA>ed transaction"
                                        pure $ Left e
                                    Right () -> 
                                        Right <$> act conn
        logInfo "<MAGENTA>COMMIT</MAGENTA>ed transaction"
        pure res

    runNoTxnMig
        :: DB.Connection
        -> SingleNoTxnMigration m
        -> (FilePath -> DB.UTCTimestamp -> DiffTime -> Int -> MigrationApplicationStatus -> m ()) -- ^ This is `m` instead of `txn` and is correct, unlike in `runInTxnBlock`. See reasons presented there.
        -> m (Maybe x)
    runNoTxnMig conn mig registerMig = do
        retryFold @MigrationApplicationFailure
                retryPolicy
                (\(previousMig, _) RetryIteration { lastError } ->
                    case lastError of
                        Nothing -> pure (previousMig, 0)
                        Just MigrationApplicationFailure { noTxnMigRetryInstructions }
                            -> case noTxnMigRetryInstructions of
                                Nothing ->
                                    error
                                        "Internal error in codd, please report. This is supposed to be a no-txn migration, yet the internal error does not contain retry instructions"
                                Just (NoTxnMigMustRestartAfterSkipping numStmtsToSkip _timeSpentApplying)
                                    -> do
                                        logWarn
                                            $  "Skipping the first "
                                            <> Fmt.sformat Fmt.int
                                                           numStmtsToSkip
                                            <> " SQL statements, which have already been applied, and start applying from the "
                                            <> Fmt.sformat
                                                   Fmt.ords
                                                   (numStmtsToSkip + 1)
                                            <> " statement"
                                        -- TODO: Resuming from the Nth statement shall soon be possible not just for a `BEGIN` statement, but also from a fresh invocation of "codd up" after a previous one failed!
                                        -- Re-reading from disk only to skip statements is unnecessary work when the failed statement was not in an explicit `BEGIN ... COMMIT` section, but having more than one code path to handle such extremely rare errors is not worth it
                                        freshBlock <- reReadMig previousMig
                                        pure (freshBlock, numStmtsToSkip)
                )
                (mig, 0)
                (\case
                    Left lastErr -> do
                        logError
                            "Failed after all configured retries. Giving up."
                        -- The warning below would be very nice to have, but we don't want it appearing for new users of codd when they're trying `codd add` for the first time. We should know if this migration application happens during `codd add` and omit the warning below.
                        -- logWarn "IMPORTANT:\n\
                        --        \ If this is a database you care about and can't simply recreate (e.g. a Production database), then read the following _very carefully_:\n\
                        --        \ A no-txn migration failed to be applied completely but may have had some of its statements applied. Your database may have been left in an intermediary state.\n\
                        --        \ If you think this is a temporary error and that resuming from the exact statement inside the no-txn migration that failed might work, you can just run `codd up` and codd _will resume_ migration application precisely from that last failed statement.\n\
                        --        \ If you're going to do that, however, do _not_ edit the migration as changing the position of the failed statement inside it will make codd silently continue from the wrong place.\n\
                        --        \ But if this is not going away merely by resuming application from the last failed statement, one other option you have is to look at the last error above to see how many statements codd applied and which statement failed. This should help you pinpoint the precise failing statement inside the migration, even if it might not be entirely obvious how codd counts statements internally. \n\
                        --        \ Once you know which statement that is, you can edit the migration and remove that failing statement and all others that come after it. You can then rewrite that part of the migration in a way you think will fix the problem. Just make sure you don't change anything before it. After that you can run `codd up` and codd will resume application after skipping the statements that had been applied, meaning it will resume from the first statement in the rewritten part of the migration."
                        throwIO lastErr
                    Right ret -> pure ret
                )
            $ \(migFinal, numStmtsToSkip) ->
                  fmap (const Nothing) <$> applySingleMigration
                                      conn
                                      registerMig
                                      numStmtsToSkip
                                      (singleNoTxnMig migFinal)

data CoddSchemaVersion = CoddSchemaDoesNotExist | CoddSchemaV1 | CoddSchemaV2 -- ^ V2 includes duration of each migration's application
     | CoddSchemaV3 -- ^ V3 includes the number of SQL statements applied per migration, allowing codd to resume application of even failed no-txn migrations correctly
  deriving stock (Bounded, Enum, Eq, Ord, Show)

detectCoddSchema :: MonadIO m => DB.Connection -> m CoddSchemaVersion
detectCoddSchema conn = do
    cols :: [Text] <-
        map DB.fromOnly
            <$> query
                    conn
                    "select attname from pg_attribute join pg_class on attrelid=pg_class.oid join pg_namespace on relnamespace=pg_namespace.oid where relname='sql_migrations' AND nspname='codd_schema' AND attnum >= 1 order by attnum"
                    ()
    case cols of
        [] -> pure CoddSchemaDoesNotExist
        ["id", "migration_timestamp", "applied_at", "name"] ->
            pure CoddSchemaV1
        ["id", "migration_timestamp", "applied_at", "name", "application_duration"]
            -> pure CoddSchemaV2
        ["id", "migration_timestamp", "applied_at", "name", "application_duration", "num_applied_statements", "no_txn_failed_at"]
            -> pure CoddSchemaV3
        _ ->
            error
                $ "Internal codd error. Unless you've manually modified the codd_schema.sql_migrations table, this is a bug in codd. Please report it and include the following as column names in your report: "
                ++ show cols

createCoddSchema
    :: forall txn m
     . (MonadUnliftIO m, NotInTxn m, txn ~ InTxnT m)
    => CoddSchemaVersion
    -- ^ Desired schema version. This should always be `maxBound` in the app; it's meant to assume other values only in tests
    -> TxnIsolationLvl
    -> DB.Connection
    -> m ()
createCoddSchema targetVersion txnIsolationLvl conn =
    withTransaction @txn txnIsolationLvl conn $ do
        currentSchemaVersion <- detectCoddSchema conn
        catchJust
            (\e -> if isPermissionDeniedError e then Just () else Nothing)
            (go currentSchemaVersion)
            (\() ->
                throwIO
                    $ userError
                          "Not enough permissions to create or update codd's internal schema. Please check that your default connection string can create tables, sequences and GRANT them permissions."
            )
  where
    go currentSchemaVersion
        | targetVersion < currentSchemaVersion
        = error
            "Cannot migrate newer codd_schema version to an older version. Please report this as a bug in codd."
        | targetVersion == currentSchemaVersion
        = pure ()
        | otherwise
        = do
            case currentSchemaVersion of
                CoddSchemaDoesNotExist -> do
                    execvoid_
                        conn
                        "CREATE SCHEMA codd_schema; GRANT USAGE ON SCHEMA codd_schema TO PUBLIC;"
                    execvoid_ conn
                        $ "CREATE TABLE codd_schema.sql_migrations ( \
                        \  id SERIAL PRIMARY KEY\
                        \, migration_timestamp timestamptz not null\
                        \, applied_at timestamptz not null \
                        \, name text not null \
                        \, unique (name), unique (migration_timestamp));"
                        <> -- It is not necessary to grant SELECT on the table, but it helps _a lot_ with a test and shouldn't hurt.
                           -- SELECT on the sequence enables dumps by unprivileged users
                           "GRANT INSERT,SELECT ON TABLE codd_schema.sql_migrations TO PUBLIC;\
                           \GRANT USAGE ,SELECT ON SEQUENCE codd_schema.sql_migrations_id_seq TO PUBLIC;"
                CoddSchemaV1 -> execvoid_
                    conn
                    "ALTER TABLE codd_schema.sql_migrations ADD COLUMN application_duration INTERVAL"
                CoddSchemaV2 -> execvoid_
                    conn
                    "ALTER TABLE codd_schema.sql_migrations ADD COLUMN num_applied_statements INT, ADD COLUMN no_txn_failed_at timestamptz, ALTER COLUMN applied_at DROP NOT NULL, ADD CONSTRAINT no_txn_mig_applied_or_failed CHECK ((applied_at IS NULL) <> (no_txn_failed_at IS NULL))"
                CoddSchemaV3 -> pure ()

            -- `succ` is a partial function, but it should never throw in this context
            go (succ currentSchemaVersion)

-- | Collects pending migrations and separates them according to being bootstrap
--   or not.
collectPendingMigrations
    :: forall m
     . ( MonadUnliftIO m
       , CoddLogger m
       , MonadResource m
       , MonadThrow m
       , NotInTxn m
       , EnvVars m
       )
    => DB.ConnectInfo
    -> Either [FilePath] [AddedSqlMigration m]
    -> TxnIsolationLvl
    -> DiffTime
    -> m (PendingMigrations m)
collectPendingMigrations defaultConnString sqlMigrations txnIsolationLvl connectTimeout
    = do
        bootCheck   <- checkNeedsBootstrapping defaultConnString connectTimeout
        pendingMigs <- collect bootCheck

        unless (defaultConnAccessible bootCheck) $ do
            logInfo
                "Default connection string is not accessible. Codd will run in bootstrap mode, expecting the first migrations will contain custom connection strings and will create/bootstrap the database."

            -- The specific error below isn't necessary at this stage, but it's much more informative
            -- than the errors we'd generate further ahead.
            let bootstrapMigBlocks =
                    takeWhile hasNonDefaultConnectionString pendingMigs
            when (null bootstrapMigBlocks) $ do
                logError
                    "The earliest existing migration has no custom connection string or there are no migrations at all. Exiting."
                liftIO exitFailure

        pure $ PendingMigrations pendingMigs bootCheck

  where
    hasNonDefaultConnectionString block =
        let mConnInfo = migrationCustomConnInfo $ addedSqlMig $ case block of
                BlockInTxn (ConsecutiveInTxnMigrations (m1 :| _) _) -> m1
                BlockNoTxn (SingleNoTxnMigration       m         _) -> m
        in  case mConnInfo of
                Nothing       -> False
                Just connInfo -> DB.connectDatabase defaultConnString
                    /= DB.connectDatabase connInfo
    collect bootCheck = do
        logInfoNoNewline "Looking for pending migrations..."
        migsAlreadyApplied :: [FilePath] <-
            if coddSchemaVersion bootCheck == CoddSchemaDoesNotExist
                then pure []
                else withConnection
                    defaultConnString
                    connectTimeout
                    (\conn ->
                        withTransaction @(InTxnT m) txnIsolationLvl conn
                            $   map DB.fromOnly
                            <$> query
                                    conn
                                    "SELECT name FROM codd_schema.sql_migrations WHERE applied_at IS NOT NULL"
                                    ()
                    )

        blocksOfPendingMigs <- parseMigrationFiles migsAlreadyApplied
                                                   sqlMigrations
        logInfo
            $  " [<CYAN>"
            <> Fmt.sformat
                   Fmt.int
                   (sum $ map (NE.length . allMigs) blocksOfPendingMigs)
            <> " found</CYAN>]"
        forM_ (concatMap (NE.toList . allMigs) blocksOfPendingMigs) $ \mig -> do
            case migrationSql $ addedSqlMig mig of
                UnparsedSql _ ->
                    logWarn
                        $ Text.pack (migrationName $ addedSqlMig mig)
                        <> " is not to be parsed and thus will be treated in its entirety as an in-txn migration with a single statement, without support for COPY."
                _ -> pure ()
        pure blocksOfPendingMigs

-- | Opens a UTF-8 file allowing it to be read in streaming fashion. This function delays opening the file
-- until the returned Stream's first chunk is forced, and closes the file immediately after the returned Stream
-- is fully consumed.
delayedOpenStreamFile :: MonadResource m => FilePath -> m (FileStream m)
delayedOpenStreamFile filePath = do
    -- We can't use Streaming.fromHandle because it uses hGetLine, which removes '\n' from lines it
    -- reads, but does not append '\n' to the Strings it returns, making it impossible to know 
    -- whether the last line had a '\n' after it.
    -- See https://hackage.haskell.org/package/streaming-0.2.3.1/docs/src/Streaming.Prelude.html#fromHandle
    -- So we copied streaming's implementation and modified it slightly.
    releaseKey <- newIORef Nothing
    pure $ FileStream { filePath
                      , releaseKey
                      , fileStream = lazyStream releaseKey
                      }
  where
    -- | Lazy stream because it waits until the file is demanded to open it in the first place
    lazyStream releaseKeyIORef = do
        (releaseKey, handle) <- lift
            $ allocate (openFile filePath ReadMode) hClose
        writeIORef releaseKeyIORef (Just releaseKey)
        go releaseKeyIORef handle
    go rkioref h = do
        str <- liftIO $ Text.hGetChunk h
        case str of
            "" -> do
                hClose h
                lift $ writeIORef rkioref Nothing
            _ -> do
                Streaming.yield str
                go rkioref h

closeFileStream :: MonadResource m => FileStream m -> m ()
closeFileStream (FileStream _ releaseKey _) = do
    mrkey <- readIORef releaseKey
    forM_ mrkey release

-- | Returns all migrations on the supplied folders (including possibly already applied ones)
-- except for the explicitly supplied ones.
listMigrationsFromDisk
    :: MonadIO m
    => [FilePath]
    -- ^ The folders where to look for migrations.
    -> [FilePath]
    -- ^ Migration filenames (without their directories) to exclude from returned list.
    -> m [FilePath]
listMigrationsFromDisk sqlDirs excludeList = do
    fmap (sortOn takeFileName . concat) $ forM sqlDirs $ \dir -> do
        filesInDir <- listDirectory dir
        return $ map (dir </>) $ filter
            (\fn -> ".sql" `List.isSuffixOf` fn && fn `notElem` excludeList)
            filesInDir

parseMigrationFiles
    :: forall m
     . ( MonadUnliftIO m
       , CoddLogger m
       , MonadResource m
       , MonadThrow m
       , EnvVars m
       )
    => [FilePath]
    -> Either [FilePath] [AddedSqlMigration m]
    -> m [BlockOfMigrations m]
parseMigrationFiles migsCompleted sqlMigrations = do
    pendingParsedMigrations :: [ ( Either String (FileStream m)
          , AddedSqlMigration m
          )
        ]                                                       <-
        either (listPendingFromDisk >=> readFromDisk)
               (pure . readFromMemory . listPendingFromMemory)
               sqlMigrations

    -- Group consecutive in-txn migrations with the same connection string together for atomic application
    pure
        $   NE.groupBy
                (\(_, AddedSqlMigration m1 _) (_, AddedSqlMigration m2 _) ->
                    migrationInTxn m1
                        && migrationInTxn m2
                        && migrationCustomConnInfo m1
                        == migrationCustomConnInfo m2
                )
                pendingParsedMigrations
        <&> \migs ->
                let firstMig = snd $ NE.head migs
                in  if migrationInTxn (addedSqlMig firstMig)
                        then BlockInTxn ConsecutiveInTxnMigrations
                            { inTxnMigs   = snd <$> migs
                            , reReadBlock = reRead migs
                            }
                        else BlockNoTxn SingleNoTxnMigration
                            { singleNoTxnMig = firstMig
                            , reReadMig      = reReadNoTxn migs
                            }

  where
    reRead oldMigsAndPaths = do
        -- Close handles of all migrations in the block, re-open and read+parse them
        filePaths <- forM oldMigsAndPaths $ \case
            (Left _memStream, _) ->
                error "Re-reading in-memory streams is not yet implemented"
            (Right fileStream, _) ->
                closeFileStream fileStream >> pure (filePath fileStream)
        newMigs <- readFromDisk filePaths
        pure ConsecutiveInTxnMigrations { inTxnMigs   = snd <$> newMigs
                                        , reReadBlock = reRead newMigs
                                        }
    -- | TODO: This is a near duplicate of `reRead`. Improve this.
    reReadNoTxn oldMigsAndPaths = do
        -- Close handles of all migrations in the block, re-open and read+parse them
        filePaths <- forM oldMigsAndPaths $ \case
            (Left _memStream, _) ->
                error "Re-reading in-memory streams is not yet implemented"
            (Right fileStream, _) ->
                closeFileStream fileStream >> pure (filePath fileStream)
        newMigs <- readFromDisk filePaths
        pure SingleNoTxnMigration { singleNoTxnMig = snd (NE.head newMigs)
                                  , reReadMig      = reReadNoTxn newMigs
                                  }
    readFromMemory
        :: [AddedSqlMigration m]
        -> [(Either String (FileStream m), AddedSqlMigration m)]
    readFromMemory ams =
        map
                (\asqlmig@(AddedSqlMigration mig _) ->
                    (Left $ migrationName mig, asqlmig)
                )
            $ sortOn (\(AddedSqlMigration _ ts) -> ts) ams
    listPendingFromMemory = filter
        (\(AddedSqlMigration mig _) -> migrationName mig `notElem` migsCompleted
        )
    listPendingFromDisk sqlDirs = listMigrationsFromDisk sqlDirs migsCompleted

    readFromDisk
        :: forall t
         . Traversable t
        => t FilePath
        -> m (t (Either String (FileStream m), AddedSqlMigration m))
    readFromDisk pendingSqlMigrationFiles =
        forM pendingSqlMigrationFiles $ \pendingMigrationPath -> do
            fs :: FileStream m <- delayedOpenStreamFile pendingMigrationPath
            let fn = takeFileName $ filePath fs
            parsedMig <- parseAddedSqlMigration fn fs
            case parsedMig of
                Left err -> do
                    throwIO
                        $  userError
                        $  "Fatal error parsing migration '"
                        <> fn
                        <> "': "
                        <> err
                Right asqlmig@(AddedSqlMigration mig _) -> do
                    case migrationSql mig of
                        UnparsedSql _ -> do
                            -- We can close the file with peace of mind in this case, as it has been read into memory in its entirety. In fact,
                            -- it's already closed because the stream was consumed completely, but let's be explicit.
                            closeFileStream fs
                            pure (Right fs, asqlmig)
                        _ -> do
                            -- Close the file so we don't crash due to the shell's open files limit. The handle will be opened again
                            -- when the stream is forced next time.
                            -- This isn't terribly pretty and assumes the file won't change in between now and the moment it'll be opened again for SQL to be effectivelly applied,
                            -- but that assumption is probably fine and unavoidable if we want to report errors in any pending migrations before we start applying their SQL.
                            closeFileStream fs
                            fileStreamAgain :: FileStream m <-
                                delayedOpenStreamFile pendingMigrationPath
                            let sqlPiecesStreamAgain =
                                    substituteEnvVarsInSqlPiecesStream
                                            ( migrationEnvVars
                                            $ addedSqlMig asqlmig
                                            )
                                        $ parseSqlPiecesStreaming
                                        $ fileStream fileStreamAgain
                                asqlmigAgain = asqlmig
                                    { addedSqlMig = (addedSqlMig asqlmig)
                                        { migrationSql = WellParsedSql
                                            sqlPiecesStreamAgain
                                        }
                                    }
                            pure (Right fileStreamAgain, asqlmigAgain)

-- | This can be used as a last-action when applying migrations to
-- strict-check schemas, logging differences, success and throwing
-- an exception if they mismatch.
strictCheckLastAction
    :: (MonadUnliftIO m, CoddLogger m, InTxn m)
    => CoddSettings
    -> DbRep
    -> ([BlockOfMigrations m] -> DB.Connection -> m ())
strictCheckLastAction coddSettings expectedReps blocksOfMigs conn = do
    cksums <- readRepresentationsFromDbWithSettings coddSettings conn
    unless (isOneShotApplication (migsConnString coddSettings) blocksOfMigs)
        $ do
              logWarn
                  "Because it wasn't possible to apply all pending migrations in a single transaction, all migrations have been applied. We'll run a schema check."
    logSchemasComparison cksums expectedReps
    when (cksums /= expectedReps) $ throwIO $ userError
        "Exiting. Database's schema differ from expected."

-- | This can be used as a last-action when applying migrations to
-- lax-check schemas, logging differences or success, but
-- _never_ throwing exceptions and returning the database schema.
laxCheckLastAction
    :: (MonadUnliftIO m, CoddLogger m, InTxn m)
    => CoddSettings
    -> DbRep
    -> ([BlockOfMigrations m] -> DB.Connection -> m DbRep)
laxCheckLastAction coddSettings expectedReps _blocksOfMigs conn = do
    cksums <- readRepresentationsFromDbWithSettings coddSettings conn
    logSchemasComparison cksums expectedReps
    pure cksums

-- | A collection of consecutive migrations. Consecutive in-txn migrations with the same connection string are grouped together,
-- no-txn migrations appear alone.
data BlockOfMigrations m = BlockInTxn (ConsecutiveInTxnMigrations m) | BlockNoTxn (SingleNoTxnMigration m)
data SingleNoTxnMigration m = SingleNoTxnMigration
    { singleNoTxnMig :: AddedSqlMigration m
    , reReadMig      :: m (SingleNoTxnMigration m)
    }
data ConsecutiveInTxnMigrations m = ConsecutiveInTxnMigrations
    { inTxnMigs   :: NonEmpty (AddedSqlMigration m)
    , reReadBlock :: m (ConsecutiveInTxnMigrations m)
    }
allMigs :: BlockOfMigrations m -> NonEmpty (AddedSqlMigration m)
allMigs (BlockInTxn b) = inTxnMigs b
allMigs (BlockNoTxn b) = singleNoTxnMig b :| []

hoistBlockOfMigrations
    :: forall m n
     . (Monad m, Monad n)
    => (forall x . m x -> n x)
    -> BlockOfMigrations m
    -> BlockOfMigrations n
hoistBlockOfMigrations hoist = \case
    BlockInTxn b -> BlockInTxn $ hoistInTxnBlock b
    BlockNoTxn b -> BlockNoTxn $ hoistNoTxnBlock b
  where
    hoistInTxnBlock ConsecutiveInTxnMigrations {..} =
        let hoistedAllMigs     = hoistAddedSqlMigration hoist <$> inTxnMigs
            hoistedReReadBlock = hoist $ reReadBlock <&> hoistInTxnBlock
        in  ConsecutiveInTxnMigrations { inTxnMigs   = hoistedAllMigs
                                       , reReadBlock = hoistedReReadBlock
                                       }
    hoistNoTxnBlock SingleNoTxnMigration {..} =
        let hoistedAllMigs     = hoistAddedSqlMigration hoist singleNoTxnMig
            hoistedReReadBlock = hoist $ reReadMig <&> hoistNoTxnBlock
        in  SingleNoTxnMigration { singleNoTxnMig = hoistedAllMigs
                                 , reReadMig      = hoistedReReadBlock
                                 }

-- | Returns True only if all pending migrations are in-txn and of the same connection string, meaning they'll all be applied
-- in a single transaction.
isOneShotApplication :: DB.ConnectInfo -> [BlockOfMigrations m] -> Bool
isOneShotApplication defaultConnInfo pending = case pending of
    [] -> True
    [block@(BlockInTxn _)] ->
        fromMaybe defaultConnInfo (blockCustomConnInfo block) == defaultConnInfo
    _ -> False

blockCustomConnInfo :: BlockOfMigrations m -> Maybe DB.ConnectInfo
blockCustomConnInfo (BlockInTxn (ConsecutiveInTxnMigrations (AddedSqlMigration { addedSqlMig } :| _) _))
    = migrationCustomConnInfo addedSqlMig
blockCustomConnInfo (BlockNoTxn (SingleNoTxnMigration (AddedSqlMigration { addedSqlMig }) _))
    = migrationCustomConnInfo addedSqlMig

data NoTxnMigFailureRetryInstructions = NoTxnMigMustRestartAfterSkipping
                                            Int
                                            -- ^ Number of statements successfully applied, which is also the number of statements to skip in the next attempt
                                            DiffTime
                                            -- ^ Time spent applying statements in this failed attempt
    deriving stock Show
data MigrationApplicationFailure = MigrationApplicationFailure
    { sqlStatementEx            :: SqlStatementException
    , noTxnMigRetryInstructions :: Maybe NoTxnMigFailureRetryInstructions
    }
    deriving stock Show
instance Exception MigrationApplicationFailure

forMExcept :: Monad m => NonEmpty a -> (a -> m (Either e ())) -> m (Either e ())
forMExcept nl f = go (NE.toList nl)
    where
        go [] = pure $ Right ()
        go (x:xs) = f x >>= \case
                                Left e -> pure $ Left e
                                Right () -> go xs

-- | Applies a single migration and returns an error if it failed to be applied or `()` otherwise. Calls the supplied
-- `registerMigRan` after the migration is applied or fails. Does not throw exceptions coming from SQL errors.
applySingleMigration
    :: forall m
     . (MonadUnliftIO m, CoddLogger m)
    => DB.Connection
    -> (FilePath -> DB.UTCTimestamp -> DiffTime -> Int -> MigrationApplicationStatus -> m ())
    -> Int
        -- ^ Number of countable-runnable statements to skip completely. Useful when retrying no-txn migrations from exactly the statements they last failed in.
    -> AddedSqlMigration m
    -> m (Either MigrationApplicationFailure ())
applySingleMigration conn registerMigRan numCountableRunnableStmtsToSkip (AddedSqlMigration sqlMig migTimestamp)
    = do
        let fn = migrationName sqlMig
        logInfoNoNewline $ "Applying " <> Text.pack fn

        ((appliedMigrationNumStatements, errorOrDone, mLastBegin), appliedMigrationDuration) <-
            timeAction $ do
                case
                        ( migrationSql sqlMig
                        , numCountableRunnableStmtsToSkip > 0
                        )
                    of
                        (UnparsedSql _, True) ->
                            error
                                "Internal error in codd. A migration with no-parse cannot have failed in a statement that is not its 0th statement. Please report this as a bug."
                        (UnparsedSql unparsedSql, False) -> do
                            applyResult <- singleStatement_ conn unparsedSql
                            case applyResult of
                                NotACountableStatement ->
                                    error
                                        "Internal error in codd. no-parse migration cannot be NotACountableStatement. Please report this as a bug"
                                StatementErred err ->
                                    pure (0, Just err, Nothing)
                                StatementApplied _ ->
                                    pure (1, Nothing, Nothing)
                        (WellParsedSql sqlStream, _) -> do
                            initialTxnStatus <- txnStatus conn -- TODO: For no-txn migrations this should be "not in a transaction". Should we assert that?
                            ((numStmts, mLastBegin, _) :> errorOrDone) <-
                                Streaming.fold
                                    (\(!l, !lastBegin, !lastTxnStatus) txnStatusNow ->
                                        (l + 1, , txnStatusNow)
                                            $ case
                                                  (lastTxnStatus, txnStatusNow)
                                              of
                                                  (PQ.TransInTrans, PQ.TransInTrans)
                                                      -> lastBegin
                                                  (PQ.TransIdle, PQ.TransIdle)
                                                      -> Nothing
                                                  (PQ.TransIdle, PQ.TransInTrans)
                                                      -> Just (l + 1)
                                                  (PQ.TransInTrans, PQ.TransIdle)
                                                      -> Nothing
                                                  states@(PQ.TransActive, _) ->
                                                      error
                                                          $ "Internal error in codd. It seems libpq returned a transaction status while another statement was running, which should be impossible. Please report this as a bug: "
                                                          ++ show states
                                                  states@(_, PQ.TransActive) ->
                                                      error
                                                          $ "Internal error in codd. It seems libpq returned a transaction status while another statement was running, which should be impossible. Please report this as a bug"
                                                          ++ show states
                                                  states@(PQ.TransInError, _)
                                                      -> error
                                                          $ "Internal error in codd. Erring statements should be in stream's return, not as an element of it. Please report this as a bug"
                                                          ++ show states
                                                  states@(_, PQ.TransInError)
                                                      -> error
                                                          $ "Internal error in codd. Erring statements should be in stream's return, not as an element of it. Please report this as a bug"
                                                          ++ show states
                                                  states@(PQ.TransUnknown, _)
                                                      -> error
                                                          $ "Connection to database may have gone bad. Did someone else kill the connection while codd was applying migrations, perhaps? Codd cannot retry under these circumstances, sadly. Please file a bug report if retrying under such circumstances is important to you."
                                                          ++ show states
                                                  states@(_, PQ.TransUnknown)
                                                      -> error
                                                          $ "Connection to database may have gone bad. Did someone else kill the connection while codd was applying migrations, perhaps? Codd cannot retry under these circumstances, sadly. Please file a bug report if retrying under such circumstances is important to you."
                                                          ++ show states
                                    )
                                    ( numCountableRunnableStmtsToSkip
                                    , Nothing
                                    , initialTxnStatus
                                    )
                                    id
                                $ multiQueryStatement_ conn
                                $ skipNonCountableRunnableStatements
                                      numCountableRunnableStmtsToSkip
                                      sqlStream
                            pure (numStmts, errorOrDone, mLastBegin)

        case errorOrDone of
            Just sqlStatementEx -> do
                logInfo " [<RED>failed</RED>]"
                logError $ Text.pack $ show sqlStatementEx
                noTxnMigRetryInstructions <- if migrationInTxn sqlMig
                    then pure Nothing
                    else do
                        case mLastBegin of
                            Nothing -> do
                                logError
                                    $  "After applying "
                                    <> Fmt.sformat
                                           Fmt.int
                                           appliedMigrationNumStatements
                                    <> " statements from <YELLOW>no-txn</YELLOW> migration "
                                    <> Text.pack fn
                                    <> ", the "
                                    <> Fmt.sformat
                                           Fmt.ords
                                           (appliedMigrationNumStatements + 1)
                                    <> " failed to be applied. Codd will resume the next retry or <MAGENTA>codd up</MAGENTA> from it"
                                registerMigRan
                                            fn
                                            migTimestamp
                                            appliedMigrationDuration
                                            appliedMigrationNumStatements
                                            NoTxnMigrationFailed
                                pure $ Just $ NoTxnMigMustRestartAfterSkipping
                                    appliedMigrationNumStatements appliedMigrationDuration
                            Just lastBeginNum -> do
                                logError
                                    $  "After applying "
                                    <> Fmt.sformat
                                           Fmt.int
                                           appliedMigrationNumStatements
                                    <> " statements from <YELLOW>no-txn</YELLOW> migration "
                                    <> Text.pack fn
                                    <> ", the "
                                    <> Fmt.sformat
                                           Fmt.ords
                                           (appliedMigrationNumStatements + 1)
                                    <> " failed to be applied. Since this failed statement is inside an explicitly started transaction in the migration, codd will resume the next retry or <MAGENTA>codd up</MAGENTA> from the last <GREEN>BEGIN</GREEN>-like statement, which is the "
                                    <> Fmt.sformat Fmt.ords lastBeginNum
                                    <> " statement in this migration"
                                void $ liftIO $ DB.execute_ conn "ROLLBACK"
                                registerMigRan
                                            fn
                                            migTimestamp
                                            appliedMigrationDuration
                                            appliedMigrationNumStatements
                                            NoTxnMigrationFailed
                                logInfo
                                    "<MAGENTA>ROLLBACK</MAGENTA>ed last explicitly started transaction"
                                pure $ Just $ NoTxnMigMustRestartAfterSkipping
                                    (lastBeginNum - 1) appliedMigrationDuration
                pure $ Left $ MigrationApplicationFailure
                    { sqlStatementEx
                    , noTxnMigRetryInstructions
                    }
            Nothing -> do
                    registerMigRan            fn
                        migTimestamp
                        appliedMigrationDuration
                        appliedMigrationNumStatements
                        MigrationAppliedSuccessfully
                    logInfo
                        $  " (<CYAN>"
                        <> prettyPrintDuration appliedMigrationDuration
                        <> "</CYAN>, "
                        <> Fmt.sformat Fmt.int appliedMigrationNumStatements
                        <> ")"
                    pure $ Right ()


  where
    pico_1ns = 1_000
    pico_1ms = 1_000_000_000
    pico_1s :: forall a . Num a => a
    pico_1s = 1_000_000_000_000
    prettyPrintDuration (fromIntegral @Integer @Double . diffTimeToPicoseconds -> dps)
        | dps < pico_1ms
        = Fmt.sformat
                (Fmt.fixed @Double 2)
                (fromIntegral @Integer (round (100 * dps / pico_1ms)) / 100)
            <> "ms"
        | -- e.g. 0.23ms
          dps < pico_1s
        = Fmt.sformat (Fmt.int @Integer) (round $ dps / pico_1ms) <> "ms"
        | -- e.g. 671ms
          otherwise
        = Fmt.sformat
                (Fmt.fixed @Double 1)
                (fromIntegral @Integer (round (10 * dps / pico_1s)) / 10)
            <> "s" -- e.g. 10.5s
    timeAction f = do
        before <- liftIO $ getTime Monotonic
        ret    <- f
        after  <- liftIO $ getTime Monotonic
        pure
            ( ret
            , picosecondsToDiffTime
            $ (pico_1s :: Integer)
            * fromIntegral (sec after - sec before)
            + (pico_1ns :: Integer)
            * fromIntegral (nsec after - nsec before)
            )

data MigrationRegistered = MigrationRegistered | MigrationNotRegistered
data MigrationApplicationStatus = NoTxnMigrationFailed | MigrationAppliedSuccessfully
data MigrationLastStatementAppliedAt = NowInPostgresTime | SpecificTime UTCTime
type RegisterMigrationFunc m = FilePath
       -> DB.UTCTimestamp
       -> MigrationLastStatementAppliedAt
       -> DiffTime
       -> Int
       -> MigrationApplicationStatus
       -> m ()

-- | Registers in the DB that a migration with supplied name and timestamp
--   has been either successfully applied or partially failed (the latter only makes sense for no-txn migrations).
--   Fails if the codd_schema hasn't yet been created.
registerRanMigration
    :: forall txn m
     . (MonadUnliftIO m, MonadIO txn, CanStartTxn m txn)
    => DB.Connection
    -- ^ The default connection, not any other or this might fail.
    -> TxnIsolationLvl
    -> FilePath
    -> DB.UTCTimestamp
    -> MigrationLastStatementAppliedAt -- ^ The time the last statement of the migration was applied or when it failed.
    -> DiffTime
    -> Int -- ^ The number of applied statements
    -> MigrationApplicationStatus
    -> m UTCTime
registerRanMigration conn isolLvl fn migTimestamp appliedAt appliedMigrationDuration numAppliedStatements apStatus
    = let
                    (args, timestampValue) = case (appliedAt, apStatus) of
                                        (NowInPostgresTime, NoTxnMigrationFailed) -> ("?, clock_timestamp()", Nothing)
                                        (NowInPostgresTime, MigrationAppliedSuccessfully) -> ("clock_timestamp(), ?", Nothing)
                                        (SpecificTime t, NoTxnMigrationFailed) -> ("NULL, ?", Just t)
                                        (SpecificTime t, MigrationAppliedSuccessfully) -> ("?, NULL", Just t)

      in
      withTransaction @txn isolLvl conn $ DB.fromOnly <$> unsafeQuery1
        conn ("INSERT INTO codd_schema.sql_migrations as m (migration_timestamp, name, application_duration, num_applied_statements, applied_at, no_txn_failed_at) \
            \                            SELECT ?, ?, ?, ?, " <> args <> " \
            \                            ON CONFLICT (name) DO UPDATE \
            \                               SET application_duration=EXCLUDED.application_duration + m.application_duration \
            \                                 , num_applied_statements=EXCLUDED.num_applied_statements \
            \                            RETURNING COALESCE(applied_at, no_txn_failed_at)")
        ( migTimestamp
        , fn
                -- postgresql-simple does not have a `ToField DiffTime` instance :(
        , realToFrac @Double @NominalDiffTime
        $ fromIntegral (diffTimeToPicoseconds appliedMigrationDuration)
        / 1_000_000_000_000
        , numAppliedStatements
        , timestampValue
        )
