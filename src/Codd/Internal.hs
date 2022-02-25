{-# LANGUAGE BlockArguments #-}
module Codd.Internal where

import           Prelude                 hiding ( readFile )

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( DbHashes
                                                , logChecksumsComparison
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Internal.MultiQueryStatement
                                                ( InTransaction(..)
                                                , multiQueryStatement_
                                                )
import           Codd.Internal.Retry            ( retry )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , ParsingOptions(..)
                                                , SqlMigration(..)
                                                , parseAddedSqlMigration
                                                )
import           Codd.Query                     ( execvoid_
                                                , query
                                                , unsafeQuery1
                                                )
import           Codd.Types                     ( RetryPolicy(..)
                                                , TxnIsolationLvl(..)
                                                , singleTryPolicy
                                                )
import           Control.Monad                  ( forM
                                                , forM_
                                                , unless
                                                , void
                                                , when, foldM
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Logger           ( MonadLogger
                                                , NoLoggingT
                                                , logDebugN
                                                , logErrorN
                                                , logInfoN
                                                , logWarnN
                                                , runNoLoggingT
                                                )
import           Data.ByteString                ( ByteString
                                                , readFile
                                                )
import           Data.Either                    ( isLeft )
import           Data.Functor                   ( (<&>) )
import qualified Data.List                     as List
import           Data.List                      ( find
                                                , sortOn
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( mapMaybe, fromMaybe
                                                )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Time                      ( UTCTime )
import qualified Database.PostgreSQL.Simple    as DB
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>) )
import           UnliftIO                       ( MonadUnliftIO
                                                , toIO
                                                )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Directory             ( listDirectory )
import           UnliftIO.Exception             ( IOException
                                                , bracket
                                                , catchAny
                                                , handleJust
                                                , onException
                                                , throwIO
                                                , tryJust
                                                )
import qualified Database.PostgreSQL.Simple.Time as DB
import UnliftIO.Resource (runResourceT, ResourceT, allocate, ReleaseKey, release)
import Control.Monad.Trans (MonadTrans(lift))
import UnliftIO.MVar (newMVar, readMVar, modifyMVar)

dbIdentifier :: Text -> DB.Query
dbIdentifier s = "\"" <> fromString (Text.unpack s) <> "\""

-- | Tries to connect until a connection succeeds or until a timeout, executes the supplied action and disposes of the opened Connection.
withConnection
    :: (MonadUnliftIO m, MonadIO m)
    => DB.ConnectInfo
    -> (DB.Connection -> m a)
    -> m a
withConnection connStr action = go (50 :: Int) -- At most 50 * 100ms = 5 seconds
  where
    wrappedAction n eitherConn = do
        case eitherConn of
            Left e -> if n <= 0
                then throwIO e
                else threadDelay (1000 * 100) >> go (n - 1)
            Right conn -> action conn
    go n = bracket (tryConnectError $ liftIO $ DB.connect connStr)
                   (either (const $ pure ()) (liftIO . DB.close))
                   (wrappedAction n)
    tryConnectError = tryJust
        $ \e -> if isServerNotAvailableError e then Just e else Nothing

-- | Verifies if a libpq error means the server is not ready to accept connections yet,
-- either by not being listening at all or still being initializing.
isServerNotAvailableError :: IOException -> Bool
isServerNotAvailableError e =
    let err = Text.pack $ show e
    in  "libpq"
            `Text.isInfixOf` err
            &&
            (  "could not connect to server: Connection refused"
                    `Text.isInfixOf` err
            || "server closed the connection"
                `Text.isInfixOf` err
            || "the database system is starting up"
                `Text.isInfixOf` err)

-- | Returns a Query with a valid "BEGIN" statement that is READ WRITE and has
-- the desired isolation level.
beginStatement :: TxnIsolationLvl -> DB.Query
beginStatement = \case
    DbDefault       -> "BEGIN READ WRITE"
    Serializable    -> "BEGIN READ WRITE,ISOLATION LEVEL SERIALIZABLE"
    RepeatableRead  -> "BEGIN READ WRITE,ISOLATION LEVEL REPEATABLE READ"
    ReadCommitted   -> "BEGIN READ WRITE,ISOLATION LEVEL READ COMMITTED"
    ReadUncommitted -> "BEGIN READ WRITE,ISOLATION LEVEL READ UNCOMMITTED"

beginCommitTxnBracket
    :: (MonadUnliftIO m, MonadIO m)
    => TxnIsolationLvl
    -> DB.Connection
    -> m a
    -> m a
beginCommitTxnBracket isolLvl conn f = do
    iof <- toIO f
    liftIO $ do
        execvoid_ conn $ beginStatement isolLvl
        v <- iof `onException` DB.rollback conn
        DB.commit conn
        pure v

-- | Creates the App's Database and Codd's schema if it does not yet exist.
createEmptyDbIfNecessary
    :: forall m n
     . (MonadUnliftIO m, MonadIO m, n ~ NoLoggingT m)
    => CoddSettings
    -> m ()
createEmptyDbIfNecessary settings@CoddSettings { migsConnString, txnIsolationLvl }
    = runNoLoggingT $ applyMigrationsInternal applyZeroMigs settings
    -- Very special case: it's probably not very interesting to print database and Codd schema creation too many times.
    -- Since this function gets called a lot for "just-in-case" parts of the App, let's hide its output.
  where
    applyZeroMigs
        :: CanUpdateCoddSchema
        -> [NonEmpty AddedSqlMigration]
        -> n (ApplyMigsResult ())
    applyZeroMigs canUpdSchema _ = baseApplyMigsBlock migsConnString
                                                      singleTryPolicy
                                                      (\_ _ -> pure ())
                                                      txnIsolationLvl
                                                      canUpdSchema
                                                      []

-- | Returns True on any libpq connection error with the
-- supplied connection string.
checkNeedsBootstrapping
    :: (MonadUnliftIO m, MonadIO m) => DB.ConnectInfo -> m Bool
checkNeedsBootstrapping connInfo =
    -- brittany-disable-next-identifier
    handleJust
            (\e ->
                -- 1. No server available is a big "No". Bootstrapping will very likely not fix this.
                if isServerNotAvailableError e
                    then Nothing

                -- 2. Maybe the default migration connection string doesn't work because:
                -- - The DB does not exist.
                -- - CONNECT rights not granted.
                -- - User doesn't exist.
                -- In any case, it's best to be conservative and consider any libpq errors
                -- as errors that might just require bootstrapping.
                else if isLibPqError e
                    then Just True

                -- 3. Let other exceptions blow up
                else Nothing
            )
            pure
        $ withConnection connInfo (const $ pure False)
  where
    isLibPqError :: IOException -> Bool
    isLibPqError e =
        let err = Text.pack $ show e in "libpq: failed" `Text.isInfixOf` err

applyMigrationsInternal
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => (CanUpdateCoddSchema -> [BlockOfMigrations] -> m (ApplyMigsResult a))
    -> CoddSettings
    -> m a
applyMigrationsInternal txnApp coddSettings@CoddSettings { migsConnString, retryPolicy, sqlMigrations, txnIsolationLvl }
    = do
        let dbName = Text.pack $ DB.connectDatabase migsConnString
        logDebugN
            $  "Checking if Database '"
            <> dbName
            <> "' is accessible with the configured connection string..."
        needsBootstrapping <- checkNeedsBootstrapping migsConnString

        if not needsBootstrapping
            then do
                logDebugN
                    "Checking if Codd Schema exists and creating it if necessary..."

                withConnection migsConnString $ createCoddSchema txnIsolationLvl

                blocksOfMigsToRun <- collectPendingMigrations coddSettings
                ApplyMigsResult _ actionAfterResult <- txnApp
                    CanUpdateCoddSchema
                    blocksOfMigsToRun

                logDebugN
                    $  "All migrations applied to "
                    <> dbName
                    <> " successfully"
                return actionAfterResult
            else do
                logWarnN
                    "Default connection string is not accessible. Codd will run in bootstrap mode, expecting the first migrations will contain custom connection strings and will create/bootstrap the database."

                (bootstrapMigBlocks, otherMigBlocks) <-
                    collectBootstrapMigrations migsConnString sqlMigrations

                when (null bootstrapMigBlocks) $ do
                    logErrorN
                        "The earliest existing migration has no custom connection string or there are no migrations at all. Exiting."
                    liftIO exitFailure

                let applyBootstrapMigsFunc = baseApplyMigsBlock migsConnString retryPolicy (\_ _ -> pure ()) txnIsolationLvl CannotUpdateCoddSchema
                ApplyMigsResult ranBootstrapMigs _ <- applyBootstrapMigsFunc bootstrapMigBlocks
                stillNeedsBootstrapping <- checkNeedsBootstrapping migsConnString
                when stillNeedsBootstrapping $ do
                    logErrorN
                        $  "It was not possible to connect to database '"
                        <> dbName
                        <> "' after running bootstrap migrations. Exiting."
                    liftIO exitFailure

                withConnection migsConnString $ \conn -> do
                    logInfoN "Creating Codd Schema..."
                    createCoddSchema txnIsolationLvl conn

                    -- Insert bootstrap migrations that already ran into the schema
                    beginCommitTxnBracket txnIsolationLvl conn
                        $ forM_ ranBootstrapMigs
                        $ \(AddedSqlMigration sqlMig migTimestamp, timeFinished) ->
                              liftIO $ void $ DB.execute
                                  conn
                                  "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, applied_at) \
                                \ VALUES (?, ?, ?)"
                                  ( migTimestamp
                                  , migrationName sqlMig
                                  , timeFinished
                                  )

                logInfoN "Running all other remaining migrations..."
                ApplyMigsResult _ actionAfterResult <- txnApp
                    CanUpdateCoddSchema
                    otherMigBlocks
                pure actionAfterResult

isSingleTrue :: [DB.Only Bool] -> Bool
isSingleTrue v = v == [DB.Only True]

_noErrors :: MonadUnliftIO m => m () -> m ()
_noErrors f = catchAny f (const (pure ()))

createCoddSchema :: MonadIO m => TxnIsolationLvl -> DB.Connection -> m ()
createCoddSchema txnIsolationLvl conn =
    liftIO $ beginCommitTxnBracket txnIsolationLvl conn $ do
        schemaAlreadyExists <- isSingleTrue <$> query
            conn
            "SELECT TRUE FROM pg_catalog.pg_namespace WHERE nspname = ?"
            (DB.Only ("codd_schema" :: String))
        tblAlreadyExists <- isSingleTrue <$> query
            conn
            "SELECT TRUE FROM pg_catalog.pg_tables WHERE tablename = ? AND schemaname = ?"
            ("sql_migrations" :: String, "codd_schema" :: String)
        unless schemaAlreadyExists $ execvoid_ conn "CREATE SCHEMA codd_schema; GRANT USAGE ON SCHEMA codd_schema TO PUBLIC;"
        unless tblAlreadyExists $ do
            execvoid_ conn $
                  "CREATE TABLE codd_schema.sql_migrations ( \
                \  id SERIAL PRIMARY KEY\
                \, migration_timestamp timestamptz not null\
                \, applied_at timestamptz not null \
                \, name text not null \
                \, unique (name), unique (migration_timestamp));"
                <> -- It is not necessary to grant SELECT, but it helps _a lot_ with a test and shouldn't hurt.
                   "GRANT INSERT,SELECT ON TABLE codd_schema.sql_migrations TO PUBLIC;\
                   \GRANT USAGE ON SEQUENCE codd_schema.sql_migrations_id_seq TO PUBLIC;"

-- | Assumes no migrations have ever run and returns only the first blocks of migrations that contain
-- some custom connection string that do not connect to the default database, grouped by connection
-- string and in-txn/no-txn. Also returns as second element in the tuple all other migrations in their blocks.
collectBootstrapMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => DB.ConnectInfo
    -> Either [FilePath] [AddedSqlMigration]
    -> m ([BlockOfMigrations], [BlockOfMigrations])
collectBootstrapMigrations defaultConnInfo sqlMigrations =
    parseMigrationFiles [] sqlMigrations <&> span isBootstrapMigBlock
    where
        isBootstrapMigBlock (m1 :| _) = case migrationCustomConnInfo $ addedSqlMig m1 of
            Nothing -> False
            Just connInfo -> DB.connectDatabase defaultConnInfo /= DB.connectDatabase connInfo


-- | Parses on-disk migrations and checks for destructive SQL sections of migrations on the Database to collect which migrations must run,
-- grouped by in-txn/no-txn. The Database must already exist and Codd's schema must have been created.
collectPendingMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> m [BlockOfMigrations]
collectPendingMigrations CoddSettings { migsConnString, sqlMigrations, txnIsolationLvl }
    = do
        withConnection migsConnString $ \conn -> do
            -- Note: there should be no risk of race conditions for the query below, already-run migrations can't be deleted or have its non-null fields set to null again
            logDebugN
                "Checking in the Database which SQL migrations have already been applied..."
            migsAlreadyApplied :: [FilePath] <-
                liftIO
                $   beginCommitTxnBracket txnIsolationLvl conn
                $   map DB.fromOnly
                <$> query
                        conn
                        "SELECT name FROM codd_schema.sql_migrations WHERE applied_at IS NOT NULL"
                        ()

            logDebugN "Parse-checking all pending SQL Migrations..."
            parseMigrationFiles migsAlreadyApplied sqlMigrations

parseMigrationFiles
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => [FilePath]
    -> Either [FilePath] [AddedSqlMigration]
    -> m [BlockOfMigrations]
parseMigrationFiles migsCompleted sqlMigrations = do
    pendingParsedMigrations :: [AddedSqlMigration] <- either
        (\(sqlDirs :: [FilePath]) -> do
            pendingSqlMigrationFiles :: [(FilePath, FilePath)] <-
                fmap (sortOn fst . concat) $ forM sqlDirs $ \dir -> do
                    filesInDir <- listDirectory dir
                    return $ map (\fn -> (fn, dir </> fn)) $ filter
                        (\fn ->
                            ".sql"
                                `List.isSuffixOf` fn
                                &&                fn
                                `notElem`         migsCompleted
                        )
                        filesInDir
            sqlMigrationsContents :: [(FilePath, ByteString)] <-
                liftIO
                $      pendingSqlMigrationFiles
                `forM` (\(fn, fp) -> (,) fn <$> readFile fp)
            -- TODO: decodeUtf8Lenient ?
            parsedMigs <-
                forM sqlMigrationsContents $ \(fn, decodeUtf8 -> sql) -> do
                    let parsedMigGood = parseAddedSqlMigration DoParse fn sql
                    parsedMigFinal <- do
                        case parsedMigGood of
                            Right _ -> pure parsedMigGood
                            Left  _ -> do
                                logWarnN
                                    $ Text.pack fn
                                    <> " could not be parsed and thus will be considered in is entirety as in-txn"
                                pure $ parseAddedSqlMigration NoParse fn sql
                    pure (fn, parsedMigFinal)
            case find (\(_, m) -> isLeft m) parsedMigs of
                Just (fn, Left e) ->
                    error $ "Error parsing migration " ++ fn ++ ": " ++ show e
                _ -> pure ()

            return
                $ either (error "Failed to parse-check pending migrations") id
                $ traverse snd parsedMigs
        )
        (\(ams :: [AddedSqlMigration]) ->
            return
                $ mapMaybe
                      (\case
                          a@(AddedSqlMigration mig _)
                              | migrationName mig `elem` migsCompleted
                              -> Nothing
                              | otherwise
                              -> Just a
                      )
                $ sortOn (\(AddedSqlMigration _ ts) -> ts) ams
        )
        sqlMigrations

    -- Run all Migrations now. Group them in blocks of consecutive transactions by in-txn/no-txn and custom
    -- connection string.
    return $ NE.groupWith
        (\(AddedSqlMigration m _) ->
            (migrationInTxn m, migrationCustomConnInfo m)
        )
        pendingParsedMigrations

data ApplyMigsResult a = ApplyMigsResult
    { migsAppliedAt     :: [(AddedSqlMigration, UTCTime)]
    , actionAfterResult :: a
    }

-- | Applies the supplied migrations, running blocks of (in-txn, same-connection-string) migrations with "txnBracket".
-- Behaviour here unfortunately is _really_ complex right now. Important notes:
-- - Iff there's a single (in-txn, same-connection-string) block of migrations *with* the default connection string,
--   then "actionAfter" runs in the same transaction (and thus in the same connection as well) as that block.
--   Otherwise - and including if there are no migrations - it runs after all migrations, not in an explicit transaction and
--   in the supplied connection - not necessarily in the same as the last migration's.
baseApplyMigsBlock
    :: forall m a
     . (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => DB.ConnectInfo
    -> RetryPolicy
    -> ([BlockOfMigrations] -> DB.Connection -> m a)
    -> TxnIsolationLvl
    -> CanUpdateCoddSchema
    -> [BlockOfMigrations]
    -> m (ApplyMigsResult a)
baseApplyMigsBlock defaultConnInfo retryPol actionAfter isolLvl canUpdSchema blocksOfMigs
    =
        -- This function is complex because:
        -- 1. There are 3 major cases (no migrations, one in-txn default-conn-string block of migrations, all others).
        -- 2. We need to open connections only when strictly necessary due to bootstrapping.
        -- 3. We want to insert into codd_schema.sql_migrations as early as possible even for custom-connection migrations.
        -- 4. When possible, we want to insert into codd_schema.sql_migrations in the same transaction the migrations are running.

        -- So we separate the first two cases (the second hopefully being the most common one) into simpler code paths
        -- and make sure to test the third code path very well.
        case blocksOfMigs of
            [] ->
                withConnection defaultConnInfo $ fmap (ApplyMigsResult []) . actionAfter blocksOfMigs
            [block] | blockInTxn block && fromMaybe defaultConnInfo (blockCustomConnInfo block) == defaultConnInfo ->
                    -- Our very "special" and most common case case:
                    -- all migs being in-txn in the default connection-string
                    withConnection defaultConnInfo $ \defaultConn ->
                        let runAfterMig = getAfterMigRunFunc defaultConn (blockInTxn block)
                        in
                        runBlock (actionAfter blocksOfMigs) defaultConn block runAfterMig
            _ -> runResourceT $ do
                  -- Note: We could probably compound this Monad with StateT instead of using an MVar, but IIRC that creates issues
                  -- with MonadUnliftIO.
                  connsPerInfo <- newMVar (mempty :: [(DB.ConnectInfo, DB.Connection)])
                  let openConn :: DB.ConnectInfo -> ResourceT m (ReleaseKey, DB.Connection)
                      openConn cinfo = flip allocate DB.close $ do
                          mConn <- lookup cinfo <$> readMVar connsPerInfo
                          case mConn of
                              Just conn -> pure conn
                              Nothing -> modifyMVar connsPerInfo $
                                              \m -> do
                                                  conn <- DB.connect cinfo
                                                  pure ((cinfo, conn) : m, conn)

                      queryConn :: DB.ConnectInfo -> ResourceT m (Maybe DB.Connection)
                      queryConn cinfo = lookup cinfo <$> readMVar connsPerInfo

                  -- One optimization: if we can update the schema it means the default connection string
                  -- has been bootstrapped (is accessible) and codd_schema created.
                  -- Keep the default connection open in that case.
                  when (canUpdSchema == CanUpdateCoddSchema) $ void $ openConn defaultConnInfo
                  appliedMigs <- foldM (\appliedMigs block -> do
                                let cinfo = fromMaybe defaultConnInfo (blockCustomConnInfo block)
                                (_, conn) <- openConn cinfo
                                mDefaultConn <- queryConn defaultConnInfo
                                -- If the default connection is available, register any not-yet-registered migrations
                                -- before running the next block and register newly applied migrations no matter
                                -- what connection string they use.
                                runAfterMig <-
                                        case (mDefaultConn, canUpdSchema) of
                                            (Just defaultConn, CanUpdateCoddSchema) -> do
                                                lift $ registerPendingMigrations defaultConn appliedMigs
                                                pure $ getAfterMigRunFunc defaultConn (blockInTxn block)
                                            _ ->
                                                pure $ \_ _ -> DB.fromOnly <$> unsafeQuery1 conn "SELECT now()" ()
                                            

                                ApplyMigsResult newMigs () <- lift $ runBlock
                                                                        (const (pure ()))
                                                                        conn
                                                                        block
                                                                        runAfterMig
                                
                                case mDefaultConn of
                                    Nothing -> pure $ appliedMigs ++ [ (am, t, MigrationNotRegistered) | (am, t) <- newMigs ]
                                    Just _ -> pure $ [ (am, t, MigrationRegistered) | (am, t, _) <- appliedMigs ] ++ [ (am, t, MigrationRegistered) | (am, t) <- newMigs]
                        ) ([] :: [(AddedSqlMigration, UTCTime, MigrationRegistered)]) blocksOfMigs
                  
                  -- Register any unregistered migrations and run "actionAfter"
                  (releaseDefaultConn, defaultConn) <- openConn defaultConnInfo
                  lift $ registerPendingMigrations defaultConn appliedMigs
                  actAfterResult <- lift (actionAfter blocksOfMigs defaultConn)
                  release releaseDefaultConn
                  
                  pure $ ApplyMigsResult (map (\(am, t, _) -> (am, t)) appliedMigs) actAfterResult

  where
    registerPendingMigrations :: DB.Connection -> [(AddedSqlMigration, UTCTime, MigrationRegistered)] -> m ()
    registerPendingMigrations defaultConn appliedMigs =
        case canUpdSchema of
            CannotUpdateCoddSchema -> pure ()
            CanUpdateCoddSchema ->
                liftIO $ forM_
                    [ (am, appliedAt) | (am, appliedAt, MigrationNotRegistered) <- appliedMigs ]
                    (\(AddedSqlMigration mig ts, appliedAt) ->
                        DB.execute defaultConn
                                "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, applied_at) \
                                \                            VALUES (?, ?, ?)"
                                    (ts, migrationName mig, appliedAt))

    getAfterMigRunFunc :: DB.Connection -> Bool -> (FilePath -> DB.UTCTimestamp -> m UTCTime)
    getAfterMigRunFunc defaultConn inTxn =
        if inTxn then registerRanMigration defaultConn
            else \fp time -> beginCommitTxnBracket isolLvl defaultConn $ registerRanMigration defaultConn fp time

    runMigs :: DB.Connection -> RetryPolicy -> [AddedSqlMigration] -> (FilePath -> DB.UTCTimestamp -> m UTCTime) -> m [(AddedSqlMigration, UTCTime)]
    runMigs conn withRetryPolicy migs runAfterMig = forM migs $ \asqlmig ->
        (asqlmig,)
            <$> applySingleMigration conn
                                     withRetryPolicy
                                     runAfterMig
                                     asqlmig
    runBlock
        :: (DB.Connection -> m b)
        -> DB.Connection
        -> BlockOfMigrations
        -> (FilePath -> DB.UTCTimestamp -> m UTCTime)
        -> m (ApplyMigsResult b)
    runBlock act conn migBlock runAfterMig = do
        if blockInTxn migBlock
            then do
                res <- retry retryPol $ do
                    logDebugN "BEGINning transaction"
                    beginCommitTxnBracket isolLvl conn
                        $   ApplyMigsResult
                        <$> runMigs conn singleTryPolicy (NE.toList migBlock) runAfterMig -- We retry entire transactions, not individual statements
                        <*> act conn
                logDebugN "COMMITed transaction"
                pure res
            else
                ApplyMigsResult
                <$> runMigs conn retryPol (NE.toList migBlock) runAfterMig
                <*> act conn

-- | This can be used as a last-action when applying migrations to
-- strict-check checksums, logging differences, success and throwing
-- an exception if they mismatch.
strictCheckLastAction
    :: (MonadUnliftIO m, MonadLogger m)
    => CoddSettings
    -> DbHashes
    -> ([BlockOfMigrations] -> DB.Connection -> m ())
strictCheckLastAction coddSettings expectedHashes blocksOfMigs conn = do
    cksums <- readHashesFromDatabaseWithSettings coddSettings conn
    unless (all blockInTxn blocksOfMigs) $ do
        logWarnN
            "IMPORTANT: Due to the presence of no-txn or custom-connection migrations, all migrations have been applied. We'll run a schema check."
    logChecksumsComparison cksums expectedHashes
    when (cksums /= expectedHashes) $ throwIO $ userError
        "Exiting. Database checksums differ from expected."

-- | This can be used as a last-action when applying migrations to
-- lax-check checksums, logging differences or success, but
-- _never_ throwing exceptions and returning database checksums.
laxCheckLastAction
    :: (MonadUnliftIO m, MonadLogger m)
    => CoddSettings
    -> DbHashes
    -> ([BlockOfMigrations] -> DB.Connection -> m DbHashes)
laxCheckLastAction coddSettings expectedHashes _blocksOfMigs conn = do
    cksums <- readHashesFromDatabaseWithSettings coddSettings conn
    logChecksumsComparison cksums expectedHashes
    pure cksums

type BlockOfMigrations = NonEmpty AddedSqlMigration
blockInTxn :: BlockOfMigrations -> Bool
blockInTxn (AddedSqlMigration { addedSqlMig } :| _) =
    migrationInTxn addedSqlMig

blockCustomConnInfo :: BlockOfMigrations -> Maybe DB.ConnectInfo
blockCustomConnInfo (AddedSqlMigration { addedSqlMig } :| _) =
    migrationCustomConnInfo addedSqlMig

data CanUpdateCoddSchema = CanUpdateCoddSchema | CannotUpdateCoddSchema
    deriving stock (Eq)

-- | Applies a single migration and returns the time when it finished being applied.
applySingleMigration
    :: forall m
     . (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => DB.Connection
    -> RetryPolicy
    -> (FilePath -> DB.UTCTimestamp -> m UTCTime)
    -> AddedSqlMigration
    -> m UTCTime
applySingleMigration conn statementRetryPol afterMigRun (AddedSqlMigration sqlMig migTimestamp)
    = do
        let fn = migrationName sqlMig
        logDebugN $ "Applying " <> Text.pack fn

        case migrationSql sqlMig of
            Nothing -> pure ()
            Just nonDestSql ->
                let inTxn = if migrationInTxn sqlMig
                        then InTransaction
                        else NotInTransaction statementRetryPol
                in  multiQueryStatement_ inTxn conn nonDestSql

        afterMigRun fn migTimestamp

data MigrationRegistered = MigrationRegistered | MigrationNotRegistered

-- | Registers in the DB that a migration with supplied name and timestamp
--   has been applied and returns the DB's now() value (used for the "applied_at" column).
--   Fails if the codd_schema hasn't yet been created.
registerRanMigration :: MonadIO m =>
    DB.Connection
    -- ^ The default connection, not any other or this will fail.
    -> FilePath -> DB.UTCTimestamp -> m UTCTime
registerRanMigration conn fn migTimestamp =
    DB.fromOnly <$> unsafeQuery1 conn
            "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, applied_at) \
            \                            VALUES (?, ?, now()) \
            \                            RETURNING applied_at"
                (migTimestamp, fn)
