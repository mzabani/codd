module Codd.Internal where

import Prelude hiding (readFile)

import Codd.Internal.MultiQueryStatement (mqStatement_)
import Codd.Parsing (parseAddedSqlMigration)
import Codd.Query (execvoid_, query)
import Codd.Types (CoddSettings(..), SqlMigration(..), AddedSqlMigration(..))
import Codd.Hashing (DbHashes, readHashesFromDatabaseWithSettings)
import Control.Monad (void, when, forM, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString, readFile)
import Data.Maybe (isNothing, mapMaybe)
import qualified Data.List as List
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO, toIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (listDirectory)
import UnliftIO.Exception (bracket, catchAny, throwIO, tryAny, finally)

dbIdentifier :: Text -> DB.Query
dbIdentifier s = "\"" <> fromString (Text.unpack s) <> "\""

-- | Tries to connect until a connection succeeds or until a timeout, executes the supplied action and disposes of the opened Connection.
withConnection :: (MonadUnliftIO m, MonadIO m) => DB.ConnectInfo -> (DB.Connection -> m a) -> m a
withConnection connStr action = go (50 :: Int) -- At most 50 * 100ms = 5 seconds
  where
      wrappedAction n eitherConn = do
          case eitherConn of
              Left e -> if n <= 0 then throwIO e else threadDelay (1000 * 100) >> go (n - 1)
              Right conn -> action conn
      go n = bracket (tryAny $ liftIO $ DB.connect connStr) (either (const $ pure ()) (liftIO . DB.close)) (wrappedAction n)

beginCommitTxnBracket :: (MonadUnliftIO m, MonadIO m) => DB.Connection -> m a -> m a
beginCommitTxnBracket conn f = do
    iof <- toIO f
    liftIO $ DB.withTransaction conn iof

beginRollbackTxnBracket :: (MonadUnliftIO m, MonadIO m) => DB.Connection -> m a -> m a
beginRollbackTxnBracket conn f = (execvoid_ conn "BEGIN" >> f) `finally` execvoid_ conn "ROLLBACK"

type TxnBracket m = forall a. DB.Connection -> m a -> m a

checkExpectedHashesAfterAction :: (MonadUnliftIO m, MonadIO m) => CoddSettings -> Maybe DbHashes -> DB.Connection -> m ()
checkExpectedHashesAfterAction coddSettings mExpectedHashes conn = do
    case mExpectedHashes of
        Nothing -> pure ()
        Just expectedHashes -> do
            dbhashes <- readHashesFromDatabaseWithSettings coddSettings conn
            when (dbhashes /= expectedHashes) $ throwIO $ userError $ "DB Hash check failed."

applyMigrationsInternal :: (MonadUnliftIO m, MonadIO m) => TxnBracket m -> (DB.Connection -> TxnBracket m -> [BlockOfMigrations] -> m a) -> CoddSettings -> m a
applyMigrationsInternal txnBracket txnApp (coddSettings@CoddSettings { superUserConnString, dbName }) = do
    let
        unsafeDbName = dbIdentifier dbName
    liftIO $ putStr "Checking if Database exists and creating it if necessary... "
    withConnection superUserConnString $ \conn -> do
        dbExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_database WHERE datname = ?" (DB.Only dbName)
        when (not dbExists) $ execvoid_ conn $ "CREATE DATABASE " <> unsafeDbName
    liftIO $ putStrLn "[ OK ]"
    
    liftIO $ putStr "Checking if Codd Schema exists and creating it if necessary... "
    let appDbSuperUserConnString = superUserConnString { DB.connectDatabase = Text.unpack dbName }
    ret <- withConnection appDbSuperUserConnString $ \conn -> do
        -- TODO: Do we assume a failed transaction below means some other app is creating the table and won the race?
        --       We should find a better way to do this in the future.
        liftIO $ DB.withTransaction conn $ do
            schemaAlreadyExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_catalog.pg_namespace WHERE nspname = ?" (DB.Only ("codd_schema" :: String))
            tblAlreadyExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_catalog.pg_tables WHERE tablename = ? AND schemaname = ?" ("sql_migrations" :: String, "codd_schema" :: String)
            when (not schemaAlreadyExists) $ execvoid_ conn $ "CREATE SCHEMA codd_schema"
            when (not tblAlreadyExists) $ do
                execvoid_ conn $ "CREATE TABLE codd_schema.sql_migrations ( " <>
                    " migration_timestamp timestamptz not null" <>
                    ", non_dest_section_applied_at timestamptz not null " <>
                    ", dest_section_applied_at timestamptz " <>
                    ", name text not null " <>
                    ", unique (name), unique (migration_timestamp))"
        liftIO $ putStrLn "[ OK ]"

        blocksOfMigsToRun <- collectPendingMigrations coddSettings
        txnApp conn txnBracket blocksOfMigsToRun
    
    liftIO $ putStrLn "All migrations applied successfully"
    return ret

    where isSingleTrue v = v == [ DB.Only True ]
          _noErrors f = catchAny f (const (pure ()))

-- | Parses on-disk migrations and checks for destructive SQL sections of migrations on the Database to collect which migrations must run,
-- grouped by in-txn/no-txn. The Database must already exist and Codd's schema must have been created.
collectPendingMigrations :: (MonadUnliftIO m, MonadIO m) => CoddSettings -> m [BlockOfMigrations]
collectPendingMigrations (CoddSettings { superUserConnString, dbName, sqlMigrations, deploymentWorkflow }) = do
    let appDbSuperUserConnString = superUserConnString { DB.connectDatabase = Text.unpack dbName }
    withConnection appDbSuperUserConnString $ \conn -> do
        -- Note: there should be no risk of race conditions for the query below, already-run migrations can't be deleted or have its non-null fields set to null again
        liftIO $ putStr "Checking in the Database which SQL migrations have already run to completion... "
        migsThatHaveRunSomeSection :: [(FilePath, Bool)] <- liftIO $ DB.withTransaction conn $ query conn "SELECT name, dest_section_applied_at IS NOT NULL FROM codd_schema.sql_migrations" ()
        let migsCompleted = mapMaybe (\(n, f) -> if f then Just n else Nothing) migsThatHaveRunSomeSection
            migsToRunDestSection = mapMaybe (\(n, f) -> if f then Nothing else Just n) migsThatHaveRunSomeSection
        liftIO $ putStrLn "[ OK ]"

        liftIO $ putStr "Parse-checking all pending SQL Migrations... "
        pendingParsedMigrations :: [MigrationToRun] <-
            either
                (\(sqlDirs :: [FilePath]) -> do
                    allSqlMigrationFiles :: [(FilePath, FilePath)] <- fmap (sortOn fst) $ fmap concat $ forM sqlDirs $ \dir -> do
                            filesInDir <- listDirectory dir
                            return $ map (\fn -> (fn, dir </> fn)) $ filter (".sql" `List.isSuffixOf`) filesInDir
                    let pendingSqlMigrationFiles = mapMaybe (\case (n, fp) | n `elem` migsCompleted -> Nothing
                                                                            | n `elem` migsToRunDestSection -> Just ((n, fp), ApplyDestructiveOnly)
                                                                            | otherwise -> Just ((n, fp), ApplyNonDestructiveOnly))
                                                                allSqlMigrationFiles
                    sqlMigrationsContents :: [((FilePath, ByteString), ApplySingleMigration)] <- liftIO $ pendingSqlMigrationFiles `forM` \((fn, fp), ap) -> readFile fp >>= (\contents -> pure ((fn, contents), ap))
                    -- TODO: decodeUtf8Lenient ?
                    return $ either (error "Failed to parse-check pending Migrations") id $ traverse (\((fn, decodeUtf8 -> sql), ap) -> (flip MigrationToRun ap) <$> parseAddedSqlMigration deploymentWorkflow fn sql) sqlMigrationsContents
                )
                (\(ams :: [AddedSqlMigration]) ->
                    return 
                        $ mapMaybe
                            (\case a@(AddedSqlMigration mig _)
                                                | migrationName mig `elem` migsCompleted -> Nothing
                                                | migrationName mig `elem` migsToRunDestSection -> Just $ MigrationToRun a ApplyDestructiveOnly
                                                | otherwise -> Just $ MigrationToRun a ApplyNonDestructiveOnly)
                        $ sortOn (\(AddedSqlMigration _ ts) -> ts) ams
                )
            sqlMigrations
        -- ^ TODO: We are assuming manually provided AddedSqlMigrations are to have only their non-destructive sections run. This is an insufficient model for blue-green-safe deployments. Fix later.
        liftIO $ putStrLn "[ OK ]"
        
        -- Run all Migrations now. Group them in blocks of consecutive transactions by in-txn/no-txn.
        return $ NE.groupWith runInTxn pendingParsedMigrations

mainAppApplyMigsBlock :: (MonadUnliftIO m, MonadIO m) => CoddSettings -> Maybe DbHashes -> DB.Connection -> TxnBracket m -> [BlockOfMigrations] -> m ()
mainAppApplyMigsBlock coddSettings mExpectedHashes conn txnBracket pendingMigBlocks = do
    let actionAfter = checkExpectedHashesAfterAction coddSettings mExpectedHashes
    baseApplyMigsBlock actionAfter conn txnBracket pendingMigBlocks

-- | Note: "actionAfter" runs in the same transaction if all supplied migrations are in-txn, or separately, and not in an explicit transaction, otherwise.
baseApplyMigsBlock :: forall m a. (MonadUnliftIO m, MonadIO m) => (DB.Connection -> m a) -> DB.Connection -> TxnBracket m -> [BlockOfMigrations] -> m a
baseApplyMigsBlock actionAfter conn txnBracket blocksOfMigs = do
    case blocksOfMigs of
        [] -> actionAfter conn
        (b1 : [])
            | blockInTxn b1 -> runBlock actionAfter b1
        _ -> do
            forM_ blocksOfMigs $ runBlock (const (pure ()))
            actionAfter conn

    where
        runMigs migs = forM_ migs $ \(MigrationToRun asqlmig ap) -> applySingleMigration conn ap asqlmig
        runBlock :: (DB.Connection -> m b) -> BlockOfMigrations -> m b
        runBlock act migBlock = do
            if blockInTxn migBlock then do
                txnBracket conn $ do
                    runMigs migBlock
                    act conn
            else do
                runMigs migBlock
                act conn

data ApplySingleMigration = ApplyDestructiveOnly | ApplyNonDestructiveOnly deriving stock Show

data MigrationToRun = MigrationToRun AddedSqlMigration ApplySingleMigration deriving stock Show
runInTxn :: MigrationToRun -> Bool
runInTxn (MigrationToRun (AddedSqlMigration m _) ap) =
    case ap of
        ApplyDestructiveOnly -> destructiveInTxn m
        ApplyNonDestructiveOnly -> nonDestructiveInTxn m

type BlockOfMigrations = NonEmpty MigrationToRun
blockInTxn :: BlockOfMigrations -> Bool
blockInTxn (m1 :| _) = runInTxn m1

applySingleMigration :: MonadIO m => DB.Connection -> ApplySingleMigration -> AddedSqlMigration -> m ()
applySingleMigration conn ap (AddedSqlMigration sqlMig migTimestamp) = do
    let fn = migrationName sqlMig
    case ap of
            ApplyNonDestructiveOnly -> do
                if isNothing (destructiveSql sqlMig) then
                    liftIO $ putStr $ "Applying " <> fn
                else
                    liftIO $ putStr $ "Applying non-destructive section of " <> fn
                
                case nonDestructiveSql sqlMig of
                    Nothing -> pure ()
                    Just nonDestSql ->
                        -- This is ugly, but we don't trust "mqStatement_" that much yet, so if we happen
                        -- to be in a transaction, we have the luxury of not relying on our Sql Parser..
                        -- At least until it becomes more trustworthy
                        if nonDestructiveInTxn sqlMig then
                            execvoid_ conn $ DB.Query $ encodeUtf8 nonDestSql
                        else
                            mqStatement_ conn nonDestSql
                -- We mark the destructive section as ran if it's empty as well. This goes well with the Simple Deployment workflow,
                -- since every migration will have both sections marked as ran sequentially.
                liftIO $ void $ DB.execute conn "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, non_dest_section_applied_at, dest_section_applied_at) VALUES (?, ?, now(), CASE WHEN ? THEN now() END)" (migTimestamp, fn, isNothing (destructiveSql sqlMig))
            ApplyDestructiveOnly -> do
                liftIO $ putStr $ "Applying destructive section of " <> fn
                case destructiveSql sqlMig of
                    Nothing -> pure ()
                    Just destSql ->
                        if destructiveInTxn sqlMig then
                            execvoid_ conn $ DB.Query $ encodeUtf8 destSql
                        else
                            mqStatement_ conn destSql
                liftIO $ void $ DB.execute conn "UPDATE codd_schema.sql_migrations SET dest_section_applied_at = now() WHERE name=?" (DB.Only fn)
                -- TODO: Assert 1 row was updated
    liftIO $ putStrLn " [ OK ]"