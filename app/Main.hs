module Main where

import qualified Codd.Environment as Codd
import qualified Codd.Hashing as Codd
import qualified Codd.Internal as Codd
import qualified Database.PostgreSQL.Simple as DB
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

newtype SqlFilePath = SqlFilePath { unSqlFilePath :: FilePath } deriving newtype (Eq, Ord, Show)
data Cmd = Up | Analyze SqlFilePath | Add Bool SqlFilePath | DbHashes FilePath

cmdParser :: Parser Cmd
cmdParser = hsubparser (
        command "up" (info (pure Up) (progDesc "Applies all pending migrations."))
     <> command "analyze" (info analyzeParser (progDesc "Checks that a SQL migration doesn't fail and checks some of its attributes such as destructiveness, amongst others."))
     <> command "add" (info addParser (progDesc "Adds a SQL migration to the list of to-be-applied migrations"))
     <> command "dbhashes" (info dbHashesParser (progDesc "Cleans up a directory and writes a file and folder structure to it that represents the DB's current schema"))
    )

analyzeParser :: Parser Cmd
analyzeParser = Analyze <$> argument sqlFilePathReader (metavar "SQL-MIGRATION-PATH" <> help "The complete path of the .sql file to be analyzed")

addParser :: Parser Cmd
addParser = Add <$> switch (long "apply" <> help "Also applies every pending migration, including the one being added.")
                <*> argument sqlFilePathReader (metavar "SQL-MIGRATION-PATH" <> help "The complete path of the .sql file to be added")

dbHashesParser :: Parser Cmd
dbHashesParser = DbHashes <$> strArgument (metavar "FOLDER-PATH" <> help "The path to a folder where all the files an directories representing the DB's schema will be persisted to")

sqlFilePathReader :: ReadM SqlFilePath
sqlFilePathReader = fmap SqlFilePath $ eitherReader $ \s -> if ".sql" `List.isSuffixOf` s then Right s else Left "A SQL file needs to have the '.sql' extension."

main :: IO ()
main = do
  parsedCmd <- execParser opts
  adminConnInfo <- Codd.getAdminConnInfo
  appDatabaseName <- Codd.getAppDatabaseName
  doWork adminConnInfo appDatabaseName parsedCmd
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
      )

doWork :: DB.ConnectInfo -> Text -> Cmd -> IO ()
doWork _ _ Up = error "up not implemented"
doWork _ _ (Analyze fp) = putStrLn $ "Analyzing " ++ show fp
doWork _ _ (Add alsoApply fp) = putStrLn $ "Adding " ++ show fp ++ ". Applying it: " ++ show alsoApply
doWork connInfo appDatabaseName (DbHashes dirToSave) = do
  let dbCfg = connInfo { DB.connectDatabase = Text.unpack appDatabaseName }
  hashes <- Codd.connectAndDispose dbCfg Codd.getDbHashes
  Codd.persistHashesToDisk hashes dirToSave
  putStrLn $ "DB Schema structures written to " ++ show dirToSave