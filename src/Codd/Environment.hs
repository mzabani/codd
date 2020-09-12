module Codd.Environment where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, parseOnly, string, takeWhile1)
import Data.Text (Text)
import qualified Data.Text as Text
import UnliftIO (MonadIO(..))
import UnliftIO.Environment (getEnv)

-- protocol://username[:password]@host:port/database_name

connStringParser :: Parser ConnectInfo
connStringParser = do
    void $ string "postgres://"
    usr <- idParser
    pwd <- char ':' *> idParser <|> pure ""
    void $ char '@'
    host <- idParser
    port <- decimal
    void $ char '/'
    adminDb <- idParser
    pure ConnectInfo {
        connectHost = host
        , connectPort = port
        , connectUser = usr
        , connectPassword = pwd
        , connectDatabase = adminDb
    }
    where idParser :: Parser String
          idParser = Text.unpack <$> takeWhile1 (\c -> c /= ':' && c /= '@')

getAdminConnInfo :: MonadIO m => m ConnectInfo
getAdminConnInfo = do
    adminConnStr <- Text.pack <$> getEnv "ADMIN_DATABASE_URL"
    let connInfoE = parseOnly (connStringParser <* endOfInput) adminConnStr
    case connInfoE of
        Left err -> error $ "Error parsing the connection string in environment variable DATABASE_URL: " ++ err
        Right connInfo -> pure connInfo

getAppDatabaseName :: MonadIO m => m Text
getAppDatabaseName = Text.pack <$> getEnv "APP_DATABASE_NAME"
    -- TODO: Throw informative exception if env var does not exist!