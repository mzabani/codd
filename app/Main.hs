module Main where

import qualified Codd as Codd
import qualified Codd.Environment as Codd
import qualified Codd.Hashing as Codd
import qualified Codd.Internal as Codd
import Codd.Types (CoddSettings(..), SqlFilePath(..))
import Commands.AddMigration (addMigration)
import Commands.CheckMigration (checkMigrationFile)
import Commands.VerifyDb (verifyDb)
import Data.String (IsString)
import qualified Data.List as List
import Options.Applicative

data Cmd = UpDeploy | UpDev | Analyze SqlFilePath | Add Bool (Maybe FilePath) SqlFilePath | DbHashes FilePath | VerifyDb Bool

cmdParser :: Parser Cmd
cmdParser = hsubparser (
        command "up-dev" (info (pure UpDev) (progDesc "Applies all pending migrations and updates the on-disk hashes. Useful when developing."))
     <> command "up-deploy" (info (pure UpDeploy) (progDesc "Applies all pending migrations but does NOT update on-disk hashes. Instead, compares on-disk hashes to DB hashes after applying migrations and only commits if they match."))
     <> command "check" (info analyzeParser (progDesc "Checks that a SQL migration doesn't fail and checks some of its attributes such as destructiveness, amongst others."))
     <> command "add" (info addParser (progDesc "Adds and applies a SQL migration (and all pending migrations as well), then updates on-disk hashes."))
     <> command "dbhashes" (info dbHashesParser (progDesc "Cleans up a directory and writes a file and folder structure to it that represents the DB's current schema"))
     <> command "verifydb" (info verifyDbParser (progDesc "Verifies that the Database's current schema matches the current DB Hashes on Disk."))
    )

analyzeParser :: Parser Cmd
analyzeParser = Analyze <$> argument sqlFilePathReader (metavar "SQL-MIGRATION-PATH" <> help "The complete path of the .sql file to be analyzed")

addParser :: Parser Cmd
addParser = Add <$> switch (long "dont-apply" <> help "Do not apply any pending migrations, including the one being added.")
                <*> optionalStrOption (long "dest-folder" <> help "Specify the folder path where the .sql migration shall be put. If unspecified, the first folder in the 'CODD_MIGRATION_DIRS' environment variable will be used" <> metavar "DESTFOLDER")
                <*> argument sqlFilePathReader (metavar "SQL-MIGRATION-PATH" <> help "The complete path of the .sql file to be added")

dbHashesParser :: Parser Cmd
dbHashesParser = DbHashes <$> strArgument (metavar "FOLDER-PATH" <> help "The path to a folder where all the files an directories representing the DB's schema will be persisted to")

verifyDbParser :: Parser Cmd
verifyDbParser = VerifyDb <$> switch (long "verbose" <> short 'v' <> help "Prints to stdout and/or stderr with information concerning the comparison of hashes.") 

sqlFilePathReader :: ReadM SqlFilePath
sqlFilePathReader = fmap SqlFilePath $ eitherReader $ \s -> if ".sql" `List.isSuffixOf` s then Right s else Left "A SQL file needs to have the '.sql' extension."

optionalStrOption :: (Eq s, IsString s) => Mod OptionFields s -> Parser (Maybe s)
optionalStrOption optFields = fmap nothingOnEmpty $ strOption (optFields <> value "")
  where nothingOnEmpty "" = Nothing
        nothingOnEmpty v = Just v

main :: IO ()
main = do
  parsedCmd <- execParser opts
  dbVcsInfo <- Codd.getCoddSettings
  doWork dbVcsInfo parsedCmd
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
      )

doWork :: CoddSettings -> Cmd -> IO ()
doWork dbInfo UpDev = do
  Codd.applyMigrations dbInfo False
  hashes <- Codd.withConnection (Codd.superUserInAppDatabaseConnInfo dbInfo) (Codd.readHashesFromDatabaseWithSettings dbInfo)
  onDiskHashesDir <- either pure (error "This functionality needs a directory to write hashes to. Report this as a bug.") (onDiskHashes dbInfo)
  Codd.persistHashesToDisk hashes onDiskHashesDir
doWork dbInfo UpDeploy = Codd.applyMigrations dbInfo True
doWork dbInfo (Analyze fp) = checkMigrationFile dbInfo fp
doWork dbInfo (Add dontApply destFolder fp) = addMigration dbInfo dontApply destFolder fp
doWork dbInfo (VerifyDb verbose) = verifyDb dbInfo verbose
doWork dbInfo (DbHashes dirToSave) = do
  hashes <- Codd.withConnection (Codd.superUserInAppDatabaseConnInfo dbInfo) (Codd.readHashesFromDatabaseWithSettings dbInfo)
  Codd.persistHashesToDisk hashes dirToSave