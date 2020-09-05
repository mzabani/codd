module Codd.Internal where

import Prelude hiding (readFile)

import Codd.Parsing (parseSqlMigration)
import Codd.Query (execvoid_, query)
import Codd.Types (DbVcsInfo(..), ApplyMigrations(..), SqlMigration(..))
import Control.Monad (void, when, forM, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString, readFile)
import qualified Data.List as List
import Data.List (sortOn)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO, toIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (listDirectory)
import UnliftIO.Exception (bracket, catchAny, throwIO, tryAny, finally)

dbIdentifier :: String -> DB.Query
dbIdentifier s = "\"" <> fromString s <> "\""

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

applyMigrationsInternal :: (MonadUnliftIO m, MonadIO m) => (DB.Connection -> m a -> m a) -> (DB.Connection -> ApplyMigrations -> [SqlMigration] -> m a) -> DbVcsInfo -> ApplyMigrations -> m a
applyMigrationsInternal txnBracket txnApp (DbVcsInfo { superUserConnString, dbName, appUser, sqlMigrations }) applyType = do
    let
        unsafeDbName = dbIdentifier dbName
        unsafeAppUser = dbIdentifier appUser
    liftIO $ putStr "Parse-checking all SQL Migrations... "
    parsedMigrations :: [SqlMigration] <- either (\(sqlDirs :: [FilePath]) -> do
        sqlMigrationFiles :: [(FilePath, FilePath)] <- fmap (sortOn fst) $ fmap concat $ forM sqlDirs $ \dir -> do
                filesInDir <- listDirectory dir
                return $ fmap (\fn -> (fn, dir </> fn)) $ filter (".sql" `List.isSuffixOf`) filesInDir
        sqlMigrationsContents :: [(FilePath, ByteString)] <- liftIO $ sqlMigrationFiles `forM` \(fn, fp) -> (fn,) <$> readFile fp
        -- TODO: decodeUtf8Lenient. Also, better exceptions with MonadThrow ?
        return $ either (error "Failed to parse-check Migrations") id $ traverse (\(fn, decodeUtf8 -> sql) -> parseSqlMigration fn sql) sqlMigrationsContents
        ) (return . id) sqlMigrations
    liftIO $ putStrLn "[ OK ]"
    liftIO $ putStr "Going to apply sql migrations... "
    connectAndDispose superUserConnString $ \conn -> do
        dbExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_database WHERE datname = ?" (DB.Only dbName)
        userExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_catalog.pg_roles WHERE rolname = ?" (DB.Only appUser)
        -- TODO: Totally unsafe against SQL injections, but probably not a problem
        when (not dbExists) $ execvoid_ conn $ "CREATE DATABASE " <> unsafeDbName
        when (not userExists) $ do
            noErrors $ execvoid_ conn $ "CREATE USER " <> unsafeAppUser
            noErrors $ execvoid_ conn $ "GRANT CONNECT ON DATABASE " <> unsafeDbName <> " TO " <> unsafeAppUser
    
    let appDbSuperUserConnString = superUserConnString { DB.connectDatabase = dbName }
    ret <- connectAndDispose appDbSuperUserConnString $ \conn -> do
        -- TODO: Do we assume a failed transaction below means some other app is creating the table and won the race?
        --       We should find a better way to do this in the future.
        liftIO $ DB.withTransaction conn $ do
            tblAlreadyExists <- isSingleTrue <$> query conn "SELECT TRUE FROM pg_catalog.pg_tables WHERE tablename = ?" (DB.Only ("sql_migrations" :: String))
            when (not tblAlreadyExists) $ do
                execvoid_ conn $ "CREATE TABLE sql_migrations ( " <>
                    " non_dest_section_applied_at timestamptz not null " <>
                    ", dest_section_applied_at timestamptz " <>
                    ", name text not null " <>
                    ", unique (name))"
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE, REFERENCES, TRIGGER ON TABLES TO " <> unsafeAppUser
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT USAGE, SELECT, UPDATE ON SEQUENCES TO " <> unsafeAppUser
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT EXECUTE ON ROUTINES TO " <> unsafeAppUser
        
        -- Run all Migrations in a single transaction now
        txnBracket conn $ txnApp conn applyType parsedMigrations
    
    liftIO $ putStrLn "All migrations applied successfully"
    return ret

    where isSingleTrue v = v == [ DB.Only True ]
          noErrors f = catchAny f (const (pure ()))

mainAppApplyMigsBlock :: (MonadUnliftIO m, MonadIO m) => DB.Connection -> ApplyMigrations -> [SqlMigration] -> m ()
mainAppApplyMigsBlock = baseApplyMigsBlock (const $ pure ())

baseApplyMigsBlock :: (MonadUnliftIO m, MonadIO m) => (DB.Connection -> m a) -> DB.Connection -> ApplyMigrations -> [SqlMigration] -> m a
baseApplyMigsBlock actionAfter conn applyType sqlMigrations = do
    case applyType of
        IncludeDestructiveOfAlreadyRun -> error "IncludeDestructiveOfAlreadyRun not implemented"
        BothNonAndDestructive -> error "BothNonAndDestructive not implemented"
        _ -> pure ()
    
    -- TODO: Types of migration applying (Non-Dest, Dest etc.)
    execvoid_ conn "LOCK sql_migrations IN ACCESS EXCLUSIVE MODE"
    missing :: [FilePath] <- liftIO $ map DB.fromOnly <$> DB.returning conn "WITH allMigrations (name) AS (VALUES (?)) SELECT allMigrations.name FROM allMigrations LEFT JOIN sql_migrations applied USING (name) WHERE applied.name IS NULL" (map (DB.Only . migrationName) sqlMigrations)
    let missingInOrder = filter ((`elem` missing) . migrationName) sqlMigrations
    liftIO $ putStrLn $ "[ " <> show (length missingInOrder) <> " still unapplied ]"
    forM_ missingInOrder $ applySingleMigration conn IndNonDestructive

    actionAfter conn

data IndividualMigrationApplication = IndNonDestructive | IndDestructive
applySingleMigration :: MonadIO m => DB.Connection -> IndividualMigrationApplication -> SqlMigration -> m ()
applySingleMigration conn applyType sqlMig = do
    let fn = migrationName sqlMig
        mNonDestSql = encodeUtf8 <$> nonDestructiveSql sqlMig
        mDestSql = encodeUtf8 <$> destructiveSql sqlMig
    case applyType of
            IndNonDestructive -> do
                liftIO $ putStr $ "Applying non-destructive section of " <> fn
                case mNonDestSql of
                    Nothing -> pure ()
                    Just nonDestSql -> execvoid_ conn $ DB.Query $ nonDestSql
                liftIO $ void $ DB.execute conn "INSERT INTO sql_migrations (name, non_dest_section_applied_at) VALUES (?, now())" (DB.Only fn)
            IndDestructive -> do
                liftIO $ putStr $ "Applying destructive section of " <> fn
                case mDestSql of
                    Nothing -> pure ()
                    Just destSql -> execvoid_ conn $ DB.Query $ destSql
                liftIO $ void $ DB.execute conn "UPDATE sql_migrations SET dest_section_applied_at = now() WHERE name=?" (DB.Only fn)
                -- TODO: Assert 1 row was updated
    liftIO $ putStrLn " [ OK ]"