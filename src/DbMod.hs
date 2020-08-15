module DbMod (DbVcsInfo(..), bringDbUpToDate, withDbAndDrop) where

import Prelude hiding (readFile)
import Control.Monad (void, when, forM, forM_)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import UnliftIO.Exception (bracket, catchAny, throwIO, tryAny)
import Data.String (fromString)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import UnliftIO.Concurrent (threadDelay)
import RIO.List (sortOn)
import RIO.Directory (listDirectory)
import RIO.ByteString (readFile)
import qualified RIO.List as List
import System.FilePath ((</>))

data DbVcsInfo = DbVcsInfo {
    dbName :: String
    -- ^ The name of the Database the Application will connect to
    , appUser :: String
    -- ^ The name of the User which will be created and authorized to access any assets created for the App's database.
    --   This is usually the App's User.
    , superUserConnString :: DB.ConnectInfo
    -- ^ A Connection String which has the power to create databases, grant privileges and a lot more.
    , sqlMigrationsDirs :: [FilePath]
    -- ^ A list of directories with .sql files. Files from all directories are collected into a single list and then run in alphabetical order. Filed whose names don't end in .sql are ignored.
}

-- | Tries to connect until a connection succeeds or until a timeout, executes the supplied action and disposes of the opened Connection.
connectAndDispose :: (MonadUnliftIO m, MonadIO m) => DB.ConnectInfo -> (DB.Connection -> m a) -> m a
connectAndDispose connStr action = go (50 :: Int) -- At most 50 * 100ms = 5 seconds
  where
      wrappedAction n eitherConn = do
          case eitherConn of
              Left e -> if n <= 0 then throwIO e else threadDelay (1000 * 100) >> go (n - 1)
              Right conn -> action conn
      go n = bracket (tryAny $ liftIO $ DB.connect connStr) (either (const $ pure ()) (liftIO . DB.close)) (wrappedAction n)

dbIdentifier :: String -> DB.Query
dbIdentifier s = "\"" <> fromString s <> "\""

-- | Creates the new Database if necessary, applies every single migration and returns a Connection String
--   that allows the Application User to connect (not the superuser).
bringDbUpToDate :: MonadIO m => DbVcsInfo -> m DB.ConnectInfo
bringDbUpToDate DbVcsInfo { superUserConnString, dbName, appUser, sqlMigrationsDirs } = liftIO $ do
    let
        unsafeDbName = dbIdentifier dbName
        unsafeAppUser = dbIdentifier appUser
    putStr "Going to apply sql migrations... "
    connectAndDispose superUserConnString $ \conn -> do
        dbExists <- isSingleTrue <$> DB.query conn "SELECT TRUE FROM pg_database WHERE datname = ?" (DB.Only dbName)
        userExists <- isSingleTrue <$> DB.query conn "SELECT TRUE FROM pg_catalog.pg_roles WHERE rolname = ?" (DB.Only appUser)
        -- TODO: Totally unsafe against SQL injections, but probably not a problem
        when (not dbExists) $ execvoid_ conn $ "CREATE DATABASE " <> unsafeDbName
        when (not userExists) $ do
            noErrors $ execvoid_ conn $ "CREATE USER " <> unsafeAppUser
            noErrors $ execvoid_ conn $ "GRANT CONNECT ON DATABASE " <> unsafeDbName <> " TO " <> unsafeAppUser
    
    let appDbSuperUserConnString = superUserConnString { DB.connectDatabase = dbName }
    connectAndDispose appDbSuperUserConnString $ \conn -> do
        -- Now we assume a failed transaction means some other app is creating the table and won the race
        noErrors $ DB.withTransaction conn $ do
            tblAlreadyExists <- isSingleTrue <$> DB.query conn "SELECT TRUE FROM pg_catalog.pg_tables WHERE tablename = ?" (DB.Only ("sql_migrations" :: String))
            when (not tblAlreadyExists) $ do
                execvoid_ conn $ "CREATE TABLE sql_migrations ( " <>
                    " applied_at timestamptz not null default now() " <>
                    ", name text not null " <>
                    ", unique (name))"
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT SELECT, INSERT, UPDATE, DELETE, REFERENCES, TRIGGER ON TABLES TO " <> unsafeAppUser
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT USAGE, SELECT, UPDATE ON SEQUENCES TO " <> unsafeAppUser
                execvoid_ conn $ "ALTER DEFAULT PRIVILEGES FOR USER postgres IN SCHEMA public GRANT EXECUTE ON ROUTINES TO " <> unsafeAppUser
        sqlMigrationFiles :: [(FilePath, FilePath)] <- fmap (sortOn fst) $ fmap concat $ forM sqlMigrationsDirs $ \dir -> do
            filesInDir <- listDirectory dir
            return $ fmap (\fn -> (fn, dir </> fn)) $ filter (".sql" `List.isSuffixOf`) filesInDir
        print sqlMigrationFiles
        sqlMigrationsContents <- sqlMigrationFiles `forM` \(fn, fp) -> (fn,) <$> readFile fp
        DB.withTransaction conn $ do
            execvoid_ conn "LOCK sql_migrations IN ACCESS EXCLUSIVE MODE"
            missing :: [FilePath] <- fmap DB.fromOnly <$> DB.returning conn "WITH allMigrations (name) AS (VALUES (?)) SELECT allMigrations.name FROM allMigrations LEFT JOIN sql_migrations applied USING (name) WHERE applied.name IS NULL" (fmap (DB.Only . fst) sqlMigrationFiles)
            let missingInOrder = filter ((`elem` missing) . fst) sqlMigrationsContents
            putStrLn $ "[ " <> show (length missingInOrder) <> " still unapplied ]"
            forM_ missingInOrder $ \(fn, sql) -> do
                putStr $ "Applying " <> show fn
                execvoid_ conn (DB.Query sql)
                void $ DB.execute conn "INSERT INTO sql_migrations (name) VALUES (?)" (DB.Only fn)
                putStrLn " [ OK ]"
    putStrLn "All migrations applied successfully"
    return appDbSuperUserConnString { DB.connectUser = appUser }

    where isSingleTrue v = v == [ DB.Only True ]
          -- execvoid conn q r = void $ DB.execute conn q r
          execvoid_ conn q = void $ DB.execute_ conn q
          noErrors f = catchAny f (const (pure ()))

-- | Brings a Database up to date just like `bringDbUpToDate`, executes the supplied action and DROPs the Database
-- afterwards. Useful for testing.
withDbAndDrop :: MonadUnliftIO m => DbVcsInfo -> (DB.ConnectInfo -> m a) -> m a
withDbAndDrop dbInfo f = bracket (bringDbUpToDate dbInfo) dropDb f
  where
      dropDb _ = do
          connectAndDispose (superUserConnString dbInfo) $ \conn -> void $ liftIO $ DB.execute_ conn $ "DROP DATABASE IF EXISTS " <> dbIdentifier (dbName dbInfo)

