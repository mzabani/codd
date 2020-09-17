module Codd.Environment (connStringParser, getAdminConnInfo, getDbVcsInfo, appUserInAppDatabaseConnInfo, superUserInAppDatabaseConnInfo) where

import Codd.Types (DbVcsInfo(..), DeploymentWorkflow(..))
import Codd.Parsing (parseMigrationTimestamp)
import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, parseOnly, peekChar, string)
import qualified Data.Attoparsec.Text as Parsec
import Data.Text (Text)
import qualified Data.Text as Text
import UnliftIO (MonadIO(..))
import UnliftIO.Environment (lookupEnv)

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Stops at and does not consume the first predicate-passing
-- char.
parseWithEscapeChar :: (Char -> Bool) -> Parser Text
parseWithEscapeChar untilc = do
    cs <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
    nextChar <- peekChar
    case nextChar of
        Nothing -> pure cs
        Just '\\' -> do
            void $ char '\\'
            c <- Parsec.take 1
            rest <- parseWithEscapeChar untilc
            pure $ cs <> c <> rest
        Just _ -> pure cs

-- | Parses a string in the format protocol://username[:password]@host:port/database_name
connStringParser :: Parser ConnectInfo
connStringParser = do
    void $ string "postgres://"
    usr <- idParser "username"
    pwd <- (char ':' *> idParser "password") <|> pure ""
    void $ char '@'
    host <- idParser "host" -- TODO: IPv6 addresses such as ::1 ??
    void $ char ':'
    port <- decimal <|> fail "Could not find a port in the connection string."
    void $ char '/'
    adminDb <- idParser "database"
    pure ConnectInfo {
        connectHost = host
        , connectPort = port
        , connectUser = usr
        , connectPassword = pwd
        , connectDatabase = adminDb
    }
    where idParser :: String -> Parser String
          idParser idName = do
              x <- Text.unpack <$> parseWithEscapeChar (\c -> c == ':' || c == '@')
              when (x == "") $ fail $ "Could not find a " <> idName <> " in the connection string."
              pure x

readEnv :: MonadIO m => String -> m Text
readEnv var = maybe (error $ "Could not find environment variable '" ++ var ++ "'") Text.pack <$> lookupEnv var

parseEnv :: MonadIO m => a -> (String -> Either Text a) -> String -> m a
parseEnv defaultValue parser var = do
    e <- lookupEnv var
    case e of
        Nothing -> pure defaultValue
        Just v ->
            case parser v of
                Left err -> error (Text.unpack err)
                Right x -> pure x

getAdminConnInfo :: MonadIO m => m ConnectInfo
getAdminConnInfo = do
    adminConnStr <- readEnv "ADMIN_DATABASE_URL"
    let connInfoE = parseOnly (connStringParser <* endOfInput) adminConnStr
    case connInfoE of
        Left err -> error $ "Error parsing the connection string in environment variable ADMIN_DATABASE_URL: " ++ err
        Right connInfo -> pure connInfo

getDbVcsInfo :: MonadIO m => m DbVcsInfo
getDbVcsInfo = do
    adminConnInfo <- getAdminConnInfo
    appDbName <- readEnv "APP_DATABASE"
    appUserName <- readEnv "APP_USERNAME"
    sqlMigrationPaths <- map Text.unpack . Text.splitOn ":" <$> readEnv "SQL_MIGRATION_PATHS" -- No escaping colons in PATH (really?) so no escaping here either
    -- TODO: Do we throw on empty sqlMigrationPaths?
    onDiskHashesDir <- Text.unpack <$> readEnv "DB_ONDISK_HASHES"
    destructiveUpTo <- parseEnv SimpleDeployment (fmap BlueGreenSafeDeploymentUpToAndIncluding . parseMigrationTimestamp) "CODD_DESTROY_UP_TO_AND_INCLUDING"
    pure DbVcsInfo { dbName = appDbName, appUser = appUserName, superUserConnString = adminConnInfo, sqlMigrations = Left sqlMigrationPaths, onDiskHashes = Left onDiskHashesDir, deploymentWorkflow = destructiveUpTo }

-- | Returns a `ConnectInfo` that will connect to the App's Database with the Super User's credentials.
superUserInAppDatabaseConnInfo :: DbVcsInfo -> ConnectInfo
superUserInAppDatabaseConnInfo DbVcsInfo { superUserConnString, dbName } = superUserConnString { connectDatabase = Text.unpack dbName }

-- | Returns a `ConnectInfo` that will connect to the App's Database with the App's User's credentials.
appUserInAppDatabaseConnInfo :: DbVcsInfo -> ConnectInfo
appUserInAppDatabaseConnInfo DbVcsInfo { superUserConnString, dbName, appUser } = superUserConnString { connectDatabase = Text.unpack dbName, connectUser = Text.unpack appUser }