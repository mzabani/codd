module Main where

import qualified Codd as Codd
import Codd.Environment (CoddSettings(..))
import qualified Codd.Environment as Codd
import qualified Codd.Hashing as Codd
import qualified Codd.Internal as Codd
import Codd.Types (SqlFilePath(..))
import Commands.AddMigration (AddMigrationOptions(..), addMigration)
import Commands.CheckMigration (checkMigrationFile)
import Commands.VerifyDb (verifyDb)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Functor ((<&>))
import Data.String (IsString)
import qualified Data.List as List
import Options.Applicative
import Types (Verbosity(..), runVerbosityLogger)

data Cmd = UpDeploy | UpDev | Analyze Verbosity SqlFilePath | Add AddMigrationOptions (Maybe FilePath) Verbosity SqlFilePath | WriteChecksum (Maybe FilePath) | VerifyDb Verbosity

cmdParser :: Parser Cmd
cmdParser = hsubparser (
        command "up-dev" (info (pure UpDev) (progDesc "Applies all pending migrations and updates the on-disk checksum. Useful when developing."))
     <> command "up-deploy" (info (pure UpDeploy) (progDesc "Applies all pending migrations but does NOT update on-disk checksum. Instead, compares on-disk checksum to DB checksum after applying migrations (and before commiting all changes when possible)."))
     <> command "check" (info analyzeParser (progDesc "Checks that a SQL migration doesn't fail and checks some of its attributes such as destructiveness, amongst others."))
     <> command "add" (info addParser (progDesc "Adds and applies a SQL migration (and all pending migrations as well), then updates on-disk checksum."))
     <> command "write-checksum" (info dbHashesParser (progDesc "Writes files and folders to the checksum's folder that represent the DB's current schema"))
     <> command "verifydb" (info verifyDbParser (progDesc "Verifies that the Database's current schema matches the current DB Hashes on Disk."))
    )

analyzeParser :: Parser Cmd
analyzeParser = Analyze <$> verbositySwitch
                        <*> argument sqlFilePathReader (metavar "SQL-MIGRATION-PATH" <> help "The complete path of the .sql file to be analyzed")

addParser :: Parser Cmd
addParser = Add <$>
                  (AddMigrationOptions <$> switch (long "dont-apply" <> help "Do not apply any pending migrations, including the one being added.")
                                       <*> switch (long "no-parse" <> help "Use only in case Codd's parsing fails for some reason but you're certain the SQL is valid. Disabling parsing means the migration will be treated as in-txn and COPY FROM STDIN will not be supported.")
                  )
                <*> optionalStrOption (long "dest-folder" <> help "Specify the folder path where the .sql migration shall be put. If unspecified, the first folder in the 'CODD_MIGRATION_DIRS' environment variable will be used" <> metavar "DESTFOLDER")
                <*> verbositySwitch
                <*> argument sqlFilePathReader (metavar "SQL-MIGRATION-PATH" <> help "The complete path of the .sql file to be added")

dbHashesParser :: Parser Cmd
dbHashesParser = WriteChecksum <$> optionalStrOption (long "dest-folder" <> help "The path to a folder where all the files an directories representing the DB's schema will be persisted to")

verifyDbParser :: Parser Cmd
verifyDbParser = VerifyDb <$> verbositySwitch

sqlFilePathReader :: ReadM SqlFilePath
sqlFilePathReader = fmap SqlFilePath $ eitherReader $ \s -> if ".sql" `List.isSuffixOf` s then Right s else Left "A SQL file needs to have the '.sql' extension."

optionalStrOption :: (Eq s, IsString s) => Mod OptionFields s -> Parser (Maybe s)
optionalStrOption optFields = fmap nothingOnEmpty $ strOption (optFields <> value "")
  where nothingOnEmpty "" = Nothing
        nothingOnEmpty v = Just v

verbositySwitch :: Parser Verbosity
verbositySwitch =
  switch (long "verbose" <> short 'v' <> help "Prints detailed execution information to stdout.") 
  <&> \case
        True -> Verbose
        False -> NonVerbose

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
doWork dbInfo UpDev = runStdoutLoggingT $ do
  Codd.applyMigrations dbInfo False
  checksum <- Codd.withConnection (Codd.superUserInAppDatabaseConnInfo dbInfo) (Codd.readHashesFromDatabaseWithSettings dbInfo)
  onDiskHashesDir <- either pure (error "This functionality needs a directory to write checksum to. Report this as a bug.") (onDiskHashes dbInfo)
  Codd.persistHashesToDisk checksum onDiskHashesDir
doWork dbInfo UpDeploy = runStdoutLoggingT $ Codd.applyMigrations dbInfo True
doWork dbInfo (Analyze verbosity fp) = runVerbosityLogger verbosity $ checkMigrationFile dbInfo fp
doWork dbInfo (Add dontApply destFolder verbosity fp) = runVerbosityLogger verbosity $ addMigration dbInfo dontApply destFolder fp
doWork dbInfo (VerifyDb verbose) = runVerbosityLogger verbose $ verifyDb dbInfo
doWork dbInfo (WriteChecksum mdest) = runStdoutLoggingT $ do
  checksum <- Codd.withConnection (Codd.superUserInAppDatabaseConnInfo dbInfo) (Codd.readHashesFromDatabaseWithSettings dbInfo)
  let dirToSave =
        case mdest of
          Just d -> d
          Nothing ->
            case Codd.onDiskHashes dbInfo of
              Right _ -> error "This functionality needs a directory to write checksum to. Report this as a bug."
              Left d -> d

  Codd.persistHashesToDisk checksum dirToSave