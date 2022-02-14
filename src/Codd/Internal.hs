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
                                                , when
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
import           Data.Maybe                     ( isJust
                                                , mapMaybe
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

isServerNotAvailableError :: IOException -> Bool
isServerNotAvailableError e =
    let err = Text.pack $ show e
    in  "libpq"
            `Text.isInfixOf` err
            &&               "server closed the connection"
            `Text.isInfixOf` err

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
applyMigrationsInternal txnApp coddSettings@CoddSettings { migsConnString, sqlMigrations, txnIsolationLvl }
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
                    $ "Database '"
                    <> dbName
                    <> "' does not exist. Codd will run in bootstrap mode, expecting the very first migrations will contain custom connection strings and will create/bootstrap the database appropriately."

                (bootstrapMigBlocks, otherMigBlocks) <-
                    collectBootstrapMigrations sqlMigrations

                when (null bootstrapMigBlocks) $ do
                    logErrorN
                        "The earliest existing migration has no custom connection string or there are no migrations at all. Exiting."
                    liftIO exitFailure

                ApplyMigsResult ranBootstrapMigs _ <- txnApp
                    CannotUpdateCoddSchema
                    bootstrapMigBlocks
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
        unless schemaAlreadyExists $ execvoid_ conn "CREATE SCHEMA codd_schema"
        unless tblAlreadyExists $ do
            execvoid_ conn
                $  "CREATE TABLE codd_schema.sql_migrations ( "
                <> " migration_timestamp timestamptz not null"
                <> ", applied_at timestamptz not null "
                <> ", dest_section_applied_at timestamptz "
                <> ", name text not null "
                <> ", unique (name), unique (migration_timestamp))"

-- | Assumes no migrations have ever run and returns only the first blocks of migrations that contain
-- some custom connection string, grouped by connection string and in-txn/no-txn. Also returns as
-- second element in the tuple all other migrations in their blocks.
collectBootstrapMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => Either [FilePath] [AddedSqlMigration]
    -> m ([BlockOfMigrations], [BlockOfMigrations])
collectBootstrapMigrations sqlMigrations =
    parseMigrationFiles [] sqlMigrations <&> span (isJust . blockCustomConnInfo)


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
-- - Iff there's a single (in-txn, same-connection-string) block of migrations, then "actionAfter" runs in the same transaction
--   (and thus in the same connection as well) as that block.
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
    = do
        -- TODO: This function badly needs refactoring
        case blocksOfMigs of
            [] ->
                withConnection defaultConnInfo
                    $ fmap (ApplyMigsResult [])
                    . actionAfter blocksOfMigs
            [x] | blockInTxn x -> case blockCustomConnInfo x of
                Nothing -> withConnection defaultConnInfo
                    $ \conn -> runBlock (actionAfter blocksOfMigs) conn x
                Just customConnInfo ->
                    withConnection customConnInfo $ \customConn ->
                        runBlock (actionAfter blocksOfMigs) customConn x
            _ ->
                ApplyMigsResult
                    <$> (concatMap (\(ApplyMigsResult ms _) -> ms) <$> forM
                            blocksOfMigs
                            \block -> case blockCustomConnInfo block of
                                Nothing ->
                                    withConnection defaultConnInfo
                                        $ \conn -> runBlock
                                              (const (pure ()))
                                              conn
                                              block
                                Just customConnInfo ->
                                    withConnection customConnInfo
                                        $ \customConn -> runBlock
                                              (const (pure ()))
                                              customConn
                                              block
                        )
                    <*> withConnection defaultConnInfo
                                       (actionAfter blocksOfMigs)


  where
    runMigs conn withRetryPolicy migs = forM migs $ \asqlmig ->
        (,) asqlmig
            <$> applySingleMigration conn
                                     isolLvl
                                     withRetryPolicy
                                     canUpdSchema
                                     asqlmig
    runBlock
        :: (DB.Connection -> m b)
        -> DB.Connection
        -> BlockOfMigrations
        -> m (ApplyMigsResult b)
    runBlock act conn migBlock = do
        if blockInTxn migBlock
            then do
                res <- retry retryPol $ do
                    logDebugN "BEGINning transaction"
                    beginCommitTxnBracket isolLvl conn
                        $   ApplyMigsResult
                        <$> runMigs conn singleTryPolicy (NE.toList migBlock) -- We retry entire transactions, not individual statements
                        <*> act conn
                logDebugN "COMMITed transaction"
                pure res
            else
                ApplyMigsResult
                <$> runMigs conn retryPol (NE.toList migBlock)
                <*> act conn

-- | This can be used as a last-action when applying migrations to
-- hard-check checksums, logging differences, success and throwing
-- an exception if they mismatch.
hardCheckLastAction
    :: (MonadUnliftIO m, MonadLogger m)
    => CoddSettings
    -> DbHashes
    -> ([BlockOfMigrations] -> DB.Connection -> m ())
hardCheckLastAction coddSettings expectedHashes blocksOfMigs conn = do
    cksums <- readHashesFromDatabaseWithSettings coddSettings conn
    unless (all blockInTxn blocksOfMigs) $ do
        logWarnN
            "IMPORTANT: Due to the presence of no-txn migrations, all migrations have been applied. We'll run a schema check."
    logChecksumsComparison cksums expectedHashes
    when (cksums /= expectedHashes) $ throwIO $ userError
        "Exiting. Database checksums differ from expected."

-- | This can be used as a last-action when applying migrations to
-- soft-check checksums, logging differences or success, but
-- _never_ throwing exceptions and returning database checksums.
softCheckLastAction
    :: (MonadUnliftIO m, MonadLogger m)
    => CoddSettings
    -> DbHashes
    -> ([BlockOfMigrations] -> DB.Connection -> m DbHashes)
softCheckLastAction coddSettings expectedHashes _blocksOfMigs conn = do
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

-- | Applies a single migration and returns the time when it finished being applied.
applySingleMigration
    :: forall m
     . (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => DB.Connection
    -> TxnIsolationLvl
    -> RetryPolicy
    -> CanUpdateCoddSchema
    -> AddedSqlMigration
    -> m UTCTime
applySingleMigration conn isolLvl statementRetryPol canUpdSchema (AddedSqlMigration sqlMig migTimestamp)
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
                                                                                                                                                                                                                                                                                                                                    -- since every migration will have both sections marked as ran sequentially.

                                                                                                                                                                                                                                                                                                                -- If already in a transaction, then just execute, otherwise
                                                                                                                                                                                                                                                                                                                -- start read-write txn
        let query1_ :: DB.ToRow b => DB.Query -> b -> m (DB.Only UTCTime)
            query1_ q qargs = if migrationInTxn sqlMig
                then unsafeQuery1 conn q qargs
                else beginCommitTxnBracket isolLvl conn
                    $ unsafeQuery1 conn q qargs

        case canUpdSchema of
            CanUpdateCoddSchema -> DB.fromOnly <$> query1_
                "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, applied_at) \
                            \ VALUES (?, ?, now()) \
                            \ RETURNING applied_at"
                (migTimestamp, fn)
            CannotUpdateCoddSchema -> DB.fromOnly <$> query1_ "SELECT now()" ()
