{-# LANGUAGE BlockArguments, AllowAmbiguousTypes #-}
module Codd.Internal where

import           Prelude                 hiding ( readFile )

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal.MultiQueryStatement
                                                ( SqlStatementException
                                                , multiQueryStatement_
                                                )
import           Codd.Internal.Retry            ( RetryIteration(..)
                                                , retryFold
                                                )
import           Codd.Logging                   ( CoddLogger
                                                , logDebug
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
                                                , unsafeQuery1
                                                , withTransaction
                                                )
import           Codd.Representations           ( DbRep
                                                , logSchemasComparison
                                                , readRepresentationsFromDbWithSettings
                                                )
import           Codd.Types                     ( RetryPolicy(..)
                                                , TxnIsolationLvl(..)
                                                , singleTryPolicy
                                                )
import           Control.Monad                  ( (>=>)
                                                , foldM
                                                , forM
                                                , forM_
                                                , unless
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
                                                , picosecondsToDiffTime
                                                )
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
                                                , writeIORef
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
                                                , readMVar
                                                )
import           UnliftIO.Resource              ( MonadResource
                                                , ReleaseKey
                                                , ResourceT
                                                , allocate
                                                , release
                                                , runResourceT
                                                )

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
            isSingleInTxnBlock = case pendingMigs of
                [] -> True
                [block]
                    | blockInTxn block
                        && fromMaybe defaultConnInfo (blockCustomConnInfo block)
                        == defaultConnInfo
                    -> True
                _ -> False

        -- Note: We could probably compound this Monad with StateT instead of using an MVar, but IIRC that creates issues
        -- with MonadUnliftIO.
        connsPerInfo <- newMVar (mempty :: [(DB.ConnectInfo, DB.Connection)])
        let openConn :: DB.ConnectInfo -> m (ReleaseKey, DB.Connection)
            openConn cinfo = flip allocate DB.close $ do
                mConn <- lookup cinfo <$> readMVar connsPerInfo
                case mConn of
                    Just conn -> pure conn
                    Nothing   -> modifyMVar connsPerInfo $ \m -> do
                                        -- Need to unliftIO to log in here?
                                        -- logInfo $ "Connecting to (TODO: REDACT PASSWORD) " <> Text.pack (show cinfo)
                        conn <- connectWithTimeout cinfo connectTimeout
                        pure ((cinfo, conn) : m, conn)

            queryConn :: DB.ConnectInfo -> m (Maybe DB.Connection)
            queryConn cinfo = lookup cinfo <$> readMVar connsPerInfo

        (appliedMigs :: [(AppliedMigration, MigrationRegistered)], finalBootCheck :: BootstrapCheck, singleInTxnBlockResult :: Maybe
                a) <-
            foldM
                (\(previouslyAppliedMigs, bootCheck, _) block -> do
                    let
                        cinfo = fromMaybe defaultConnInfo
                                          (blockCustomConnInfo block)
                    (_, conn)                   <- openConn cinfo
                    mDefaultConn                <- queryConn defaultConnInfo

                    -- The default connection string is the one which should have permissions
                    -- to create codd_schema.
                    (runAfterMig, newBootCheck) <- case mDefaultConn of
                        Nothing -> pure
                            ( \_ _ appliedAt _ _ -> DB.fromOnly <$> unsafeQuery1
                                conn
                                "SELECT COALESCE(?, now())"
                                (DB.Only appliedAt)
                            , bootCheck
                            )
                        Just defaultConn -> do
                            newBootCheck <-
                                if coddSchemaVersion bootCheck /= maxBound
                                    then do
                                        logInfo
                                            "Creating or updating codd_schema..."
                                        createCoddSchema @txn
                                            maxBound
                                            txnIsolationLvl
                                            defaultConn
                                        pure bootCheck
                                            { defaultConnAccessible = True
                                            , coddSchemaVersion     = maxBound
                                            }
                                    else pure bootCheck
                            registerPendingMigrations @txn
                                defaultConn
                                previouslyAppliedMigs
                            pure
                                ( registerRanMigration @txn defaultConn
                                                            txnIsolationLvl
                                , newBootCheck
                                )

                    ApplyMigsResult justAppliedMigs newSingleBlockResult <-
                        if isSingleInTxnBlock
                            then runBlock
                                (fmap Just . actionAfter hoistedBlocks)
                                conn
                                block
                                runAfterMig
                            else runBlock (const $ pure Nothing)
                                          conn
                                          block
                                          runAfterMig

                    -- Keep in mind that migrations are applied but might not be registered if
                    -- we still haven't run any default-connection-string migrations.
                    pure
                        ( map
                            (
                            , if coddSchemaVersion newBootCheck >= CoddSchemaV1
                                then MigrationRegistered
                                else MigrationNotRegistered
                            )
                            (map fst previouslyAppliedMigs ++ justAppliedMigs)
                        , newBootCheck
                        , newSingleBlockResult
                        )
                )
                ([], bootstrapCheck, Nothing)
                pendingMigs

        actAfterResult <- case singleInTxnBlockResult of
            Just result -> pure result
            Nothing     -> do
              -- It is possible to have only non-default-connection-string migrations.
              -- In that case, we assume the default-connection-string will be valid after those migrations
              -- and use that to register all applied migrations and then run "actionAfter".
                (_, defaultConn) <- openConn defaultConnInfo
                when (coddSchemaVersion finalBootCheck /= maxBound) $ do
                    logInfo "Creating or updating codd_schema..."
                    createCoddSchema @txn maxBound txnIsolationLvl defaultConn
                withTransaction @txn txnIsolationLvl defaultConn
                    $ registerPendingMigrations @txn defaultConn appliedMigs
                withTransaction txnIsolationLvl defaultConn
                    $ actionAfter hoistedBlocks defaultConn

        logInfo
            $ "<GREEN>Successfully</GREEN> applied all migrations to </GREEN><MAGENTA>"
            <> dbName
            <> "</MAGENTA>"
        return actAfterResult

  where
    registerPendingMigrations
        :: forall x n
         . (MonadUnliftIO n, MonadIO x, CanStartTxn n x)
        => DB.Connection
        -> [(AppliedMigration, MigrationRegistered)]
        -> n ()
    registerPendingMigrations defaultConn appliedMigs =
        forM_ [ am | (am, MigrationNotRegistered) <- appliedMigs ]
            $ \AppliedMigration {..} -> registerRanMigration @x
                  defaultConn
                  txnIsolationLvl
                  appliedMigrationName
                  appliedMigrationTimestamp
                  (Just appliedMigrationAt)
                  appliedMigrationDuration
                  appliedMigrationNumStatements

    runMigs
        :: (MonadUnliftIO n, CoddLogger n, CanStartTxn n txn)
        => DB.Connection
        -> NonEmpty (AddedSqlMigration n)
        -> (  FilePath
           -> DB.UTCTimestamp
           -> Maybe UTCTime
           -> DiffTime
           -> Int
           -> txn UTCTime
           )
        -> n [AppliedMigration]
    runMigs conn migs runAfterMig =
        fmap NE.toList $ forM migs $ applySingleMigration conn
                                                          runAfterMig
                                                          txnIsolationLvl
    runBlock
        :: (DB.Connection -> txn b)
        -> DB.Connection
        -> BlockOfMigrations m
        -> (  FilePath
           -> DB.UTCTimestamp
           -> Maybe UTCTime
           -> DiffTime
           -> Int
           -> txn UTCTime
           )
        -> m (ApplyMigsResult b)
    runBlock act conn migBlock registerMig = do
        if blockInTxn migBlock
            then do
                res <-
                 -- Naturally, we retry entire in-txn block transactions on error, not individual statements
                    retryFold
                            retryPolicy
                            (\previousBlock RetryIteration { tryNumber } ->
                                if tryNumber == 0
                                    then pure previousBlock
                                    else do
                                        logDebug
                                            "Re-reading migrations of this block from disk"
                                        reReadBlock previousBlock
                            )
                            migBlock
                        $ \blockFinal -> do
                              logInfo "<MAGENTA>BEGIN</MAGENTA>ning transaction"
                              withTransaction txnIsolationLvl conn
                                  $   ApplyMigsResult
                                  <$> runMigs
                                          conn
                                          (allMigs
                                              (hoistBlockOfMigrations
                                                  lift
                                                  blockFinal
                                              )
                                          )
                                          registerMig
                                  <*> act conn
                logInfo "<MAGENTA>COMMIT</MAGENTA>ed transaction"
                pure res
            else
                ApplyMigsResult
                <$> runMigs
                        conn
                        (allMigs migBlock)
                        (\fp ts appliedAt duration numAppliedStmts ->
                            withTransaction txnIsolationLvl conn
                                $ registerMig
                                      fp
                                      ts
                                      appliedAt
                                      duration
                                      numAppliedStmts
                        )
                <*> withTransaction txnIsolationLvl conn (act conn)

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
                    takeWhile isDifferentDbMigBlock pendingMigs
            when (null bootstrapMigBlocks) $ do
                logError
                    "The earliest existing migration has no custom connection string or there are no migrations at all. Exiting."
                liftIO exitFailure

        pure $ PendingMigrations pendingMigs bootCheck

  where
    isDifferentDbMigBlock (BlockOfMigrations (m1 :| _) _) =
        case migrationCustomConnInfo $ addedSqlMig m1 of
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
                        <> " is not to be parsed and thus will be considered in is entirety as in-txn, without support for COPY."
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

    -- Group migrations in blocks of consecutive transactions by in-txn/no-txn and custom
    -- connection string.
    pure
        $   NE.groupWith
                (\(_, AddedSqlMigration m _) ->
                    (migrationInTxn m, migrationCustomConnInfo m)
                )
                pendingParsedMigrations
        <&> \migs -> BlockOfMigrations { allMigs     = snd <$> migs
                                       , reReadBlock = reRead migs
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
        pure BlockOfMigrations { allMigs     = snd <$> newMigs
                               , reReadBlock = reRead newMigs
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

data ApplyMigsResult a = ApplyMigsResult
    { migsAppliedAt     :: [AppliedMigration]
    , actionAfterResult :: a
    }


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
    unless (all blockInTxn blocksOfMigs) $ do
        logWarn
            "IMPORTANT: Due to the presence of no-txn or custom-connection migrations, all migrations have been applied. We'll run a schema check."
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

-- | A collection of consecutive migrations that has the same (in-txn, db-connection)
-- attributes.
data BlockOfMigrations m = BlockOfMigrations
    { allMigs     :: NonEmpty (AddedSqlMigration m)
    , reReadBlock :: m (BlockOfMigrations m)
    }

hoistBlockOfMigrations
    :: forall m n
     . (Monad m, Monad n)
    => (forall x . m x -> n x)
    -> BlockOfMigrations m
    -> BlockOfMigrations n
hoistBlockOfMigrations hoist (BlockOfMigrations {..}) =
    let hoistedAllMigs = hoistAddedSqlMigration hoist <$> allMigs
        hoistedReReadBlock =
            hoist $ reReadBlock <&> hoistBlockOfMigrations hoist
    in  BlockOfMigrations { allMigs     = hoistedAllMigs
                          , reReadBlock = hoistedReReadBlock
                          }

blockInTxn :: BlockOfMigrations m -> Bool
blockInTxn (BlockOfMigrations (AddedSqlMigration { addedSqlMig } :| _) _) =
    migrationInTxn addedSqlMig

blockCustomConnInfo :: BlockOfMigrations m -> Maybe DB.ConnectInfo
blockCustomConnInfo (BlockOfMigrations (AddedSqlMigration { addedSqlMig } :| _) _)
    = migrationCustomConnInfo addedSqlMig

data NoTxnMigFailureRetryInstructions = NoTxnMigMustRestartFromLastExplicitBegin Int -- ^ No-txn migrations can have explicit BEGIN and COMMIT statements. If a statement inside that BEGIN..COMMIT block fails, it's useless to retry it. In that case, we must retry from the last BEGIN statement, whose statement-number inside the migration is contained here.
    | NoTxnMigMustRetryFailedStatement -- ^ When we're not inside an explicit BEGIN..COMMIT block in a no-txn migration, we must retry the statement that failed itself
    deriving stock Show
data MigrationApplicationFailure = MigrationApplicationFailure
    { sqlStatementEx            :: SqlStatementException
    , noTxnMigRetryInstructions :: Maybe NoTxnMigFailureRetryInstructions
    }
    deriving stock Show
instance Exception MigrationApplicationFailure

-- | Applies a single migration and returns the time when it finished being applied. Does not
-- itself register that the migration ran, only runs "afterMigRun" after applying the migration.
applySingleMigration
    :: forall m txn
     . (MonadUnliftIO m, CoddLogger m, CanStartTxn m txn)
    => DB.Connection
    -> (  FilePath
       -> DB.UTCTimestamp
       -> Maybe UTCTime
       -> DiffTime
       -> Int
       -> txn UTCTime
       )
    -> TxnIsolationLvl
    -> AddedSqlMigration m
    -> m AppliedMigration
applySingleMigration conn afterMigRun isolLvl (AddedSqlMigration sqlMig migTimestamp)
    = do
        let fn = migrationName sqlMig
        logInfoNoNewline $ "Applying " <> Text.pack fn

        (appliedMigrationNumStatements, appliedMigrationDuration) <-
            timeAction $ do
                (numStmts :> errorOrDone) <-
                    Streaming.length $ multiQueryStatement_ conn $ migrationSql
                        sqlMig
                case errorOrDone of
                    Just err -> do
                        -- TODO: register partially applied no-txn migration
                        logInfo " [<RED>failed</RED>]"
                        logError "GOING TO THROW EXCEPTION"
                        -- TODO: only for now, throw assuming we're not in an explicit begin..commit block. We'll implement
                        -- that later.
                        noTxnMigRetryInstructions <- if migrationInTxn sqlMig
                            then pure Nothing
                            else do
                                logError
                                    "TODO: register partially applied no-txn migration. Use INSERT ON CONFLICT since we might have progressed after a retry!"
                                pure $ Just NoTxnMigMustRetryFailedStatement
                        throwIO $ MigrationApplicationFailure
                            { sqlStatementEx            = err
                            , noTxnMigRetryInstructions
                            }
                    Nothing -> pure numStmts

        timestamp <- withTransaction isolLvl conn $ afterMigRun
            fn
            migTimestamp
            Nothing
            appliedMigrationDuration
            appliedMigrationNumStatements
        logInfo
            $  " (<CYAN>"
            <> prettyPrintDuration appliedMigrationDuration
            <> "</CYAN>, "
            <> Fmt.sformat Fmt.int appliedMigrationNumStatements
            <> ")"

        pure AppliedMigration { appliedMigrationName = migrationName sqlMig
                              , appliedMigrationTimestamp     = migTimestamp
                              , appliedMigrationAt            = timestamp
                              , appliedMigrationDuration
                              , appliedMigrationNumStatements
                              }
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

-- | Registers in the DB that a migration with supplied name and timestamp
--   has been applied and returns the value used for the "applied_at" column.
--   Fails if the codd_schema hasn't yet been created.
registerRanMigration
    :: forall txn m
     . (MonadUnliftIO m, MonadIO txn, CanStartTxn m txn)
    => DB.Connection
    -- ^ The default connection, not any other or this might fail.
    -> TxnIsolationLvl
    -> FilePath
    -> DB.UTCTimestamp
    -> Maybe UTCTime -- ^ The time the migration finished being applied. If not supplied, pg's now() will be used
    -> DiffTime
    -> Int -- ^ The number of applied statements
    -> m UTCTime
registerRanMigration conn isolLvl fn migTimestamp appliedAt appliedMigrationDuration numAppliedStatements
    = withTransaction @txn isolLvl conn $ DB.fromOnly <$> unsafeQuery1
        conn
        "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, applied_at, application_duration, num_applied_statements) \
            \                            VALUES (?, ?, COALESCE(?, now()), ?, ?) \
            \                            RETURNING applied_at"
        ( migTimestamp
        , fn
        , appliedAt
        ,
                -- postgresql-simple does not have a `ToField DiffTime` instance :(
          realToFrac @Double @NominalDiffTime
        $ fromIntegral (diffTimeToPicoseconds appliedMigrationDuration)
        / 1_000_000_000_000
        , numAppliedStatements
        )
