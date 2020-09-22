module Codd.Internal where

import Prelude hiding (readFile)

import Codd.Parsing (parseAddedSqlMigration)
import Codd.Query (execvoid_, query)
import Codd.Types (DbVcsInfo(..), DeploymentWorkflow(..), SqlMigration(..), AddedSqlMigration(..))
import Codd.Hashing (DbHashes, readHashesFromDatabase)
import Control.Monad (void, when, forM, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString, readFile)
import Data.Maybe (isNothing)
import qualified Data.List as List
import Data.List (sortOn)
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
connectAndDispose :: (MonadUnliftIO m, MonadIO m) => DB.ConnectInfo -> (DB.Connection -> m a) -> m a
connectAndDispose connStr action = go (50 :: Int) -- At most 50 * 100ms = 5 seconds
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

applyMigrationsInternal :: (MonadUnliftIO m, MonadIO m) => (DB.Connection -> m a -> m a) -> (DB.Connection -> DeploymentWorkflow -> [AddedSqlMigration] -> m a) -> DbVcsInfo-> Maybe DbHashes -> m a
applyMigrationsInternal txnBracket txnApp (DbVcsInfo { superUserConnString, dbName, appUser, sqlMigrations, deploymentWorkflow }) hashCheck = do
    let
        unsafeDbName = dbIdentifier dbName
        unsafeAppUser = dbIdentifier appUser
    liftIO $ putStr "Parse-checking all SQL Migrations... "
    parsedMigrations :: [AddedSqlMigration] <- either (\(sqlDirs :: [FilePath]) -> do
        sqlMigrationFiles :: [(FilePath, FilePath)] <- fmap (sortOn fst) $ fmap concat $ forM sqlDirs $ \dir -> do
                filesInDir <- listDirectory dir
                return $ fmap (\fn -> (fn, dir </> fn)) $ filter (".sql" `List.isSuffixOf`) filesInDir
        sqlMigrationsContents :: [(FilePath, ByteString)] <- liftIO $ sqlMigrationFiles `forM` \(fn, fp) -> (fn,) <$> readFile fp
        -- TODO: decodeUtf8Lenient. Also, better exceptions with MonadThrow ?
        return $ either (error "Failed to parse-check Migrations") id $ traverse (\(fn, decodeUtf8 -> sql) -> parseAddedSqlMigration fn sql) sqlMigrationsContents
        ) (return . id) sqlMigrations
    liftIO $ putStrLn "[ OK ]"
    liftIO $ putStr "Going to apply sql migrations... "
    connectAndDispose superUserConnString $ \conn -> do
        dbExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_database WHERE datname = ?" (DB.Only dbName)
        userExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_catalog.pg_roles WHERE rolname = ?" (DB.Only appUser)
        when (not dbExists) $ execvoid_ conn $ "CREATE DATABASE " <> unsafeDbName
        when (not userExists) $ do
            noErrors $ execvoid_ conn $ "CREATE USER " <> unsafeAppUser
            noErrors $ execvoid_ conn $ "GRANT CONNECT ON DATABASE " <> unsafeDbName <> " TO " <> unsafeAppUser
    
    let appDbSuperUserConnString = superUserConnString { DB.connectDatabase = Text.unpack dbName }
    ret <- connectAndDispose appDbSuperUserConnString $ \conn -> do
        -- TODO: Do we assume a failed transaction below means some other app is creating the table and won the race?
        --       We should find a better way to do this in the future.
        liftIO $ DB.withTransaction conn $ do
            tblAlreadyExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_catalog.pg_tables WHERE tablename = ?" (DB.Only ("sql_migrations" :: String))
            when (not tblAlreadyExists) $ do
                execvoid_ conn $ "CREATE TABLE sql_migrations ( " <>
                    " migration_timestamp timestamptz not null" <>
                    ", non_dest_section_applied_at timestamptz not null " <>
                    ", dest_section_applied_at timestamptz " <>
                    ", name text not null " <>
                    ", unique (name), unique (migration_timestamp))"
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE, REFERENCES, TRIGGER ON TABLES TO " <> unsafeAppUser
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT USAGE, SELECT, UPDATE ON SEQUENCES TO " <> unsafeAppUser
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT EXECUTE ON FUNCTIONS TO " <> unsafeAppUser
        
        -- Run all Migrations in a single transaction now
        txnBracket conn $ do
            ret <- txnApp conn deploymentWorkflow parsedMigrations
            case hashCheck of
                Nothing -> pure ret
                Just h -> do
                    dbhashes <- readHashesFromDatabase conn
                    when (dbhashes /= h) $ throwIO $ userError $ "DB Hash check failed. Aborting applying migrations."
                    pure ret
    
    liftIO $ putStrLn "All migrations applied successfully"
    return ret

    where isSingleTrue v = v == [ DB.Only True ]
          noErrors f = catchAny f (const (pure ()))

mainAppApplyMigsBlock :: (MonadUnliftIO m, MonadIO m) => DB.Connection -> DeploymentWorkflow -> [AddedSqlMigration] -> m ()
mainAppApplyMigsBlock = baseApplyMigsBlock (const $ pure ())

baseApplyMigsBlock :: (MonadUnliftIO m, MonadIO m) => (DB.Connection -> m a) -> DB.Connection -> DeploymentWorkflow -> [AddedSqlMigration] -> m a
baseApplyMigsBlock actionAfter conn deploymentWorkflow sqlMigrations = do
    execvoid_ conn "LOCK sql_migrations IN ACCESS EXCLUSIVE MODE"
    let migNames = map (migrationName . addedSqlMig) sqlMigrations
    -- ^ missing can include migrations whose dest. sections haven't been applied yet (those will have True in their tuples)
    -- let missingInOrder = [(mig, destApplied) | mig <- sqlMigrations, (missingName, destApplied) <- missing, migrationName mig == missingName]
    case deploymentWorkflow of
        BlueGreenSafeDeploymentUpToAndIncluding timestampLastMigration -> do
            liftIO $ putStrLn $ "BlueGreen up to " ++ show timestampLastMigration
            missing :: [FilePath] <- liftIO $ map DB.fromOnly <$> DB.query conn "WITH allMigrations (name) AS (VALUES ?) SELECT allMigrations.name FROM allMigrations JOIN sql_migrations applied USING (name) WHERE applied.dest_section_applied_at IS NULL AND applied.migration_timestamp <= ?" (DB.In migNames, timestampLastMigration)
            liftIO $ putStrLn $ "Destructive sections of " ++ show missing
            let missingInOrder :: [AddedSqlMigration] = filter ((`elem` missing) . migrationName . addedSqlMig) sqlMigrations
            liftIO $ putStrLn $ "[ " <> show (length missingInOrder) <> " with still unapplied destructive sections ]"
            forM_ missingInOrder $ applySingleMigration conn ApplyDestructiveOnly
        _ -> pure ()
    missing :: [FilePath] <- liftIO $ map DB.fromOnly <$> DB.returning conn "WITH allMigrations (name) AS (VALUES (?)) SELECT allMigrations.name FROM allMigrations LEFT JOIN sql_migrations applied USING (name) WHERE applied.name IS NULL" (map DB.Only migNames)
    let missingInOrder :: [AddedSqlMigration] = filter ((`elem` missing) . migrationName . addedSqlMig) sqlMigrations
    liftIO $ putStrLn $ "[ " <> show (length missingInOrder) <> " with still unapplied non-destructive sections ]"
    forM_ missingInOrder $ applySingleMigration conn ApplyNonDestructiveOnly

    actionAfter conn

data ApplySingleMigration = ApplyDestructiveOnly | ApplyNonDestructiveOnly

applySingleMigration :: MonadIO m => DB.Connection -> ApplySingleMigration -> AddedSqlMigration -> m ()
applySingleMigration conn deploymentWorkflow (AddedSqlMigration sqlMig migTimestamp) = do
    let fn = migrationName sqlMig
        mNonDestSql = encodeUtf8 <$> nonDestructiveSql sqlMig
        mDestSql = encodeUtf8 <$> destructiveSql sqlMig
    case deploymentWorkflow of
            ApplyNonDestructiveOnly -> do
                liftIO $ putStr $ "Applying non-destructive section of " <> fn
                liftIO $ print mNonDestSql
                case mNonDestSql of
                    Nothing -> pure ()
                    Just nonDestSql -> execvoid_ conn $ DB.Query nonDestSql
                -- We mark the destructive section as ran if it's empty as well. This goes well with the Simple Deployment workflow,
                -- since every migration will have both sections marked as ran sequentially.
                liftIO $ void $ DB.execute conn "INSERT INTO sql_migrations (migration_timestamp, name, non_dest_section_applied_at, dest_section_applied_at) VALUES (?, ?, now(), CASE WHEN ? THEN now() END)" (migTimestamp, fn, isNothing (destructiveSql sqlMig))
            ApplyDestructiveOnly -> do
                liftIO $ putStr $ "Applying destructive section of " <> fn
                case mDestSql of
                    Nothing -> pure ()
                    Just destSql -> execvoid_ conn $ DB.Query destSql
                liftIO $ void $ DB.execute conn "UPDATE sql_migrations SET dest_section_applied_at = now() WHERE name=?" (DB.Only fn)
                -- TODO: Assert 1 row was updated
    liftIO $ putStrLn " [ OK ]"