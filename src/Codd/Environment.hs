module Codd.Environment (connStringParser, getAdminConnInfo, getCoddSettings, superUserInAppDatabaseConnInfo) where

import Codd.Types (CoddSettings(..), DeploymentWorkflow(..), Include(..), SqlRole(..), SqlSchema(..))
import Codd.Parsing (parseMigrationTimestamp)
import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Bifunctor (bimap)
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, parseOnly, peekChar, string, skipSpace)
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

-- | Considers backslash as an espace character for space.
spaceSeparatedObjNameParser :: Parser [Text]
spaceSeparatedObjNameParser = do
    v <- parseWithEscapeChar (== ' ')
    skipSpace
    (endOfInput *> pure [v]) <|> fmap (v :) spaceSeparatedObjNameParser

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
    adminConnStr <- readEnv "CODD_ADMIN_CONNECTION"
    let connInfoE = parseOnly (connStringParser <* endOfInput) adminConnStr
    case connInfoE of
        Left err -> error $ "Error parsing the connection string in environment variable CODD_ADMIN_CONNECTION: " ++ err
        Right connInfo -> pure connInfo

getCoddSettings :: MonadIO m => m CoddSettings
getCoddSettings = do
    adminConnInfo <- getAdminConnInfo
    appDbName <- readEnv "CODD_APPDB"
    sqlMigrationPaths <- map Text.unpack . Text.splitOn ":" <$> readEnv "CODD_MIGRATION_DIRS" -- No escaping colons in PATH (really?) so no escaping here either
    -- TODO: Do we throw on empty sqlMigrationPaths?
    onDiskHashesDir <- Text.unpack <$> readEnv "CODD_CHECKSUM_DIR"
    destructiveUpTo <- parseEnv SimpleDeployment (fmap BlueGreenSafeDeploymentUpToAndIncluding . parseMigrationTimestamp) "CODD_DESTROY_UP_TO_AND_INCLUDING"
    schemasToHash <- parseEnv (error "Please define the CODD_SCHEMAS environment variable with a space separated list of schema names")
                              (fmap (Include . map SqlSchema) . parseVar spaceSeparatedObjNameParser)
                              "CODD_SCHEMAS"
    extraRolesToHash <- parseEnv (error "Please define the CODD_EXTRA_ROLES environment variable with a space separated list of extra roles")
                              (fmap (Include . map SqlRole) . parseVar spaceSeparatedObjNameParser)
                              "CODD_EXTRA_ROLES"
    pure CoddSettings {
        dbName = appDbName
        , superUserConnString = adminConnInfo
        , sqlMigrations = Left sqlMigrationPaths
        , onDiskHashes = Left onDiskHashesDir
        , deploymentWorkflow = destructiveUpTo
        , schemasToHash = schemasToHash
        , extraRolesToHash = extraRolesToHash
        }

    where
        parseVar parser = bimap Text.pack id . parseOnly (parser <* endOfInput) . Text.pack

-- | Returns a `ConnectInfo` that will connect to the App's Database with the Super User's credentials.
superUserInAppDatabaseConnInfo :: CoddSettings -> ConnectInfo
superUserInAppDatabaseConnInfo CoddSettings { superUserConnString, dbName } = superUserConnString { connectDatabase = Text.unpack dbName }