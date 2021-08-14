module Codd.Internal where

import           Prelude                 hiding ( readFile )

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( DbHashes
                                                , logHashDifferences
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
                                                , logInfoN
                                                , logWarnN
                                                , runNoLoggingT
                                                )
import           Data.ByteString                ( ByteString
                                                , readFile
                                                )
import           Data.Either                    ( isLeft )
import qualified Data.List                     as List
import           Data.List                      ( find
                                                , sortOn
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( isNothing
                                                , mapMaybe
                                                )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Database.PostgreSQL.Simple    as DB
import           System.FilePath                ( (</>) )
import           UnliftIO                       ( MonadUnliftIO
                                                , toIO
                                                )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Directory             ( listDirectory )
import           UnliftIO.Exception             ( bracket
                                                , catchAny
                                                , onException
                                                , throwIO
                                                , tryAny
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
    go n = bracket (tryAny $ liftIO $ DB.connect connStr)
                   (either (const $ pure ()) (liftIO . DB.close))
                   (wrappedAction n)

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
createEmptyDbIfNecessary settings@CoddSettings { txnIsolationLvl } =
    runNoLoggingT $ applyMigrationsInternal applyZeroMigs settings
    -- Very special case: it's probably not very interesting to print database and Codd schema creation too many times.
    -- Since this function gets called a lot for "just-in-case" parts of the App, let's hide its output.
  where
    applyZeroMigs :: DB.Connection -> [NonEmpty MigrationToRun] -> n ()
    applyZeroMigs conn _ = baseApplyMigsBlock singleTryPolicy
                                              (\_ _ -> pure ())
                                              txnIsolationLvl
                                              conn
                                              []

applyMigrationsInternal
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => (DB.Connection -> [BlockOfMigrations] -> m a)
    -> CoddSettings
    -> m a
applyMigrationsInternal txnApp coddSettings@CoddSettings { superUserConnString, dbName, txnIsolationLvl }
    = do
        let unsafeDbName = dbIdentifier dbName
        logDebugN
            $  "Checking if Database "
            <> dbName
            <> " exists and creating it if necessary..."
        withConnection superUserConnString $ \conn -> do
            dbExists <- isSingleTrue <$> query
                conn
                "SELECT TRUE FROM pg_database WHERE datname = ?"
                (DB.Only dbName)
            unless dbExists
                $  execvoid_ conn
                $  "CREATE DATABASE "
                <> unsafeDbName

        logDebugN
            "Checking if Codd Schema exists and creating it if necessary..."
        let appDbSuperUserConnString =
                superUserConnString { DB.connectDatabase = Text.unpack dbName }
        ret <- withConnection appDbSuperUserConnString $ \conn -> do
            -- TODO: Do we assume a failed transaction below means some other app is creating the table and won the race?
            --       We should find a better way to do this in the future.
            liftIO $ beginCommitTxnBracket txnIsolationLvl conn $ do
                schemaAlreadyExists <- isSingleTrue <$> query
                    conn
                    "SELECT TRUE FROM pg_catalog.pg_namespace WHERE nspname = ?"
                    (DB.Only ("codd_schema" :: String))
                tblAlreadyExists <- isSingleTrue <$> query
                    conn
                    "SELECT TRUE FROM pg_catalog.pg_tables WHERE tablename = ? AND schemaname = ?"
                    ("sql_migrations" :: String, "codd_schema" :: String)
                unless schemaAlreadyExists
                    $ execvoid_ conn "CREATE SCHEMA codd_schema"
                unless tblAlreadyExists $ do
                    execvoid_ conn
                        $  "CREATE TABLE codd_schema.sql_migrations ( "
                        <> " migration_timestamp timestamptz not null"
                        <> ", non_dest_section_applied_at timestamptz not null "
                        <> ", dest_section_applied_at timestamptz "
                        <> ", name text not null "
                        <> ", unique (name), unique (migration_timestamp))"

            blocksOfMigsToRun <- collectPendingMigrations coddSettings
            txnApp conn blocksOfMigsToRun

        logDebugN $ "All migrations applied to " <> dbName <> " successfully"
        return ret

  where
    isSingleTrue v = v == [DB.Only True]
    _noErrors f = catchAny f (const (pure ()))

-- | Parses on-disk migrations and checks for destructive SQL sections of migrations on the Database to collect which migrations must run,
-- grouped by in-txn/no-txn. The Database must already exist and Codd's schema must have been created.
collectPendingMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> m [BlockOfMigrations]
collectPendingMigrations CoddSettings { superUserConnString, dbName, sqlMigrations, deploymentWorkflow, txnIsolationLvl }
    = do
        let appDbSuperUserConnString =
                superUserConnString { DB.connectDatabase = Text.unpack dbName }
        withConnection appDbSuperUserConnString $ \conn -> do
            -- Note: there should be no risk of race conditions for the query below, already-run migrations can't be deleted or have its non-null fields set to null again
            logDebugN
                "Checking in the Database which SQL migrations have already run to completion..."
            migsThatHaveRunSomeSection :: [(FilePath, Bool)] <-
                liftIO $ beginCommitTxnBracket txnIsolationLvl conn $ query
                    conn
                    "SELECT name, dest_section_applied_at IS NOT NULL FROM codd_schema.sql_migrations"
                    ()
            let migsCompleted = mapMaybe
                    (\(n, f) -> if f then Just n else Nothing)
                    migsThatHaveRunSomeSection
                migsToRunDestSection = mapMaybe
                    (\(n, f) -> if f then Nothing else Just n)
                    migsThatHaveRunSomeSection

            logDebugN "Parse-checking all pending SQL Migrations..."
            pendingParsedMigrations :: [MigrationToRun] <- either
                (\(sqlDirs :: [FilePath]) -> do
                    allSqlMigrationFiles :: [(FilePath, FilePath)] <-
                        fmap (sortOn fst . concat) $ forM sqlDirs $ \dir -> do
                            filesInDir <- listDirectory dir
                            return $ map (\fn -> (fn, dir </> fn)) $ filter
                                (".sql" `List.isSuffixOf`)
                                filesInDir
                    let
                        pendingSqlMigrationFiles = mapMaybe
                            (\case
                                (n, fp)
                                    | n `elem` migsCompleted -> Nothing
                                    | n `elem` migsToRunDestSection -> Just
                                        ((n, fp), ApplyDestructiveOnly)
                                    | otherwise -> Just
                                        ((n, fp), ApplyNonDestructiveOnly)
                            )
                            allSqlMigrationFiles
                    sqlMigrationsContents :: [ ( (FilePath, ByteString)
                          , ApplySingleMigration
                          )
                        ]                                               <-
                        liftIO
                        $      pendingSqlMigrationFiles
                        `forM` \((fn, fp), ap) ->
                                   readFile fp
                                       >>= (\contents ->
                                               pure ((fn, contents), ap)
                                           )
                    -- TODO: decodeUtf8Lenient ?
                    parsedMigs <-
                        forM sqlMigrationsContents
                            $ \((fn, decodeUtf8 -> sql), ap) -> do
                                  let
                                      parsedMigGood = parseAddedSqlMigration
                                          deploymentWorkflow
                                          DoParse
                                          fn
                                          sql
                                  parsedMigFinal <- do
                                      case parsedMigGood of
                                          Right _ -> pure parsedMigGood
                                          Left  _ -> do
                                              logWarnN
                                                  $ Text.pack fn
                                                  <> " could not be parsed and thus will be considered in is entirety as in-txn"
                                              pure $ parseAddedSqlMigration
                                                  deploymentWorkflow
                                                  NoParse
                                                  fn
                                                  sql
                                  pure
                                      ( fn
                                      , flip MigrationToRun ap
                                          <$> parsedMigFinal
                                      )
                    case find (\(_, m) -> isLeft m) parsedMigs of
                        Just (fn, Left e) ->
                            error
                                $  "Error parsing migration "
                                ++ fn
                                ++ ": "
                                ++ show e
                        _ -> pure ()

                    return
                        $ either
                              (error "Failed to parse-check pending Migrations")
                              id
                        $ traverse snd parsedMigs
                )
                (\(ams :: [AddedSqlMigration]) ->
                    return
                        $ mapMaybe
                              (\case
                                  a@(AddedSqlMigration mig _)
                                      | migrationName mig `elem` migsCompleted -> Nothing
                                      | migrationName mig
                                          `elem` migsToRunDestSection -> Just
                                      $ MigrationToRun
                                            a
                                            ApplyDestructiveOnly
                                      | otherwise -> Just $ MigrationToRun
                                          a
                                          ApplyNonDestructiveOnly
                              )
                        $ sortOn (\(AddedSqlMigration _ ts) -> ts) ams
                )
                sqlMigrations

            -- Run all Migrations now. Group them in blocks of consecutive transactions by in-txn/no-txn.
            return $ NE.groupWith runInTxn pendingParsedMigrations

-- | Applies the supplied migrations, running blocks of in-txn migrations with "txnBracket".
-- Important notes:
-- - Iff there's a single in-txn block of migrations, then "actionAfter" runs in the same transaction as that block.
--   Otherwise - and including if there are no migrations - it runs after the all migrations and not in an explicit transaction.
baseApplyMigsBlock
    :: forall m a
     . (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => RetryPolicy
    -> ([BlockOfMigrations] -> DB.Connection -> m a)
    -> TxnIsolationLvl
    -> DB.Connection
    -> [BlockOfMigrations]
    -> m a
baseApplyMigsBlock retryPol actionAfter isolLvl conn blocksOfMigs = do
    case blocksOfMigs of
        []                 -> actionAfter blocksOfMigs conn
        [x] | blockInTxn x -> runBlock (actionAfter blocksOfMigs) x
        _                  -> do
            forM_ blocksOfMigs $ runBlock (const (pure ()))
            actionAfter blocksOfMigs conn

  where
    runMigs withRetryPolicy migs =
        forM_ migs $ \(MigrationToRun asqlmig ap) ->
            applySingleMigration conn isolLvl withRetryPolicy ap asqlmig
    runBlock :: (DB.Connection -> m b) -> BlockOfMigrations -> m b
    runBlock act migBlock = do
        if blockInTxn migBlock
            then do
                res <- retry retryPol $ do
                    logDebugN "BEGINning transaction"
                    beginCommitTxnBracket isolLvl conn $ do
                        runMigs singleTryPolicy migBlock -- We retry entire transactions, not individual statements
                        act conn
                logDebugN "COMMITed transaction"
                pure res
            else do
                runMigs retryPol migBlock
                act conn

hardCheckLastAction
    :: (MonadUnliftIO m, MonadLogger m)
    => CoddSettings
    -> DbHashes
    -> ([BlockOfMigrations] -> DB.Connection -> m ())
hardCheckLastAction coddSettings expectedHashes blocksOfMigs conn = do
    cksums <- readHashesFromDatabaseWithSettings coddSettings conn
    unless (all blockInTxn blocksOfMigs) $ do
        logWarnN
            "IMPORTANT: Due to the presence of no-txn migrations, hard checking was disabled and reverted to soft checking.\n\
            \This means all migrations have been applied and now we'll run a schema check."
    throwExceptionOnChecksumMismatch cksums expectedHashes

throwExceptionOnChecksumMismatch
    :: (MonadUnliftIO m, MonadLogger m) => DbHashes -> DbHashes -> m ()
throwExceptionOnChecksumMismatch cksums expectedHashes = do
    when (cksums /= expectedHashes) $ do
        logHashDifferences cksums expectedHashes
        throwIO
            $ userError
                  "Database checksums differ from expected. Differences printed above."

    logInfoN "Database and expected schemas match."

data ApplySingleMigration = ApplyDestructiveOnly | ApplyNonDestructiveOnly deriving stock Show

data MigrationToRun = MigrationToRun AddedSqlMigration ApplySingleMigration
    deriving stock Show
runInTxn :: MigrationToRun -> Bool
runInTxn (MigrationToRun (AddedSqlMigration m _) ap) = case ap of
    ApplyDestructiveOnly    -> destructiveInTxn m
    ApplyNonDestructiveOnly -> nonDestructiveInTxn m

type BlockOfMigrations = NonEmpty MigrationToRun
blockInTxn :: BlockOfMigrations -> Bool
blockInTxn (m1 :| _) = runInTxn m1

applySingleMigration
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => DB.Connection
    -> TxnIsolationLvl
    -> RetryPolicy
    -> ApplySingleMigration
    -> AddedSqlMigration
    -> m ()
applySingleMigration conn isolLvl statementRetryPol ap (AddedSqlMigration sqlMig migTimestamp)
    = do
        let fn = migrationName sqlMig
        case ap of
            ApplyNonDestructiveOnly -> do
                if isNothing (destructiveSql sqlMig)
                    then logDebugN $ "Applying " <> Text.pack fn
                    else
                        logDebugN
                        $  "Applying non-destructive section of "
                        <> Text.pack fn

                case nonDestructiveSql sqlMig of
                    Nothing -> pure ()
                    Just nonDestSql ->
                        let inTxn = if nonDestructiveInTxn sqlMig
                                then InTransaction
                                else NotInTransaction statementRetryPol
                        in  multiQueryStatement_ inTxn conn nonDestSql
                                                                                                                        -- since every migration will have both sections marked as ran sequentially.

                                                                                                    -- If already in a transaction, then just execute, otherwise
                                                                                                    -- start read-write txn
                let exec_ q qargs = if nonDestructiveInTxn sqlMig
                        then DB.execute conn q qargs
                        else beginCommitTxnBracket isolLvl conn
                            $ DB.execute conn q qargs
                liftIO $ void $ exec_
                    "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, non_dest_section_applied_at, dest_section_applied_at) VALUES (?, ?, now(), CASE WHEN ? THEN now() END)"
                    (migTimestamp, fn, isNothing (destructiveSql sqlMig))
            ApplyDestructiveOnly -> do
                logDebugN $ "Applying destructive section of " <> Text.pack fn
                case destructiveSql sqlMig of
                    Nothing -> pure ()
                    Just destSql ->
                        let inTxn = if destructiveInTxn sqlMig
                                then InTransaction
                                else NotInTransaction statementRetryPol
                        in  multiQueryStatement_ inTxn conn destSql

                -- If already in a transaction, then just execute, otherwise
                -- start read-write txn
                let exec_ q qargs = if destructiveInTxn sqlMig
                        then DB.execute conn q qargs
                        else beginCommitTxnBracket isolLvl conn
                            $ DB.execute conn q qargs
                liftIO $ void $ exec_
                    "UPDATE codd_schema.sql_migrations SET dest_section_applied_at = now() WHERE name=?"
                    (DB.Only fn)
            -- TODO: Assert 1 row was updated
