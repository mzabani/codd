module Main where

import qualified Codd
import           Codd.AppCommands.AddMigration  ( AddMigrationOptions(..)
                                                , addMigration
                                                )
import           Codd.AppCommands.CheckMigration
                                                ( checkMigrationFile )
import           Codd.AppCommands.VerifyChecksums
                                                ( verifyChecksums )
import           Codd.AppCommands.WriteChecksums
                                                ( WriteChecksumsOpts(..)
                                                , writeChecksums
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import qualified Codd.Environment              as Codd
import qualified Codd.Hashing                  as Codd
import           Codd.Logging                   ( Verbosity(..)
                                                , runVerbosityLogger
                                                )
import           Codd.Types                     ( SqlFilePath(..) )
import           Control.Monad                  ( void )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Data.Functor                   ( (<&>) )
import qualified Data.List                     as List
import           Data.String                    ( IsString )
import           Options.Applicative

data Cmd = UpDeploy Codd.CheckHashes | UpDev | Analyze Verbosity SqlFilePath | Add AddMigrationOptions (Maybe FilePath) Verbosity SqlFilePath | WriteChecksum WriteChecksumsOpts | VerifyChecksum Verbosity Bool

cmdParser :: Parser Cmd
cmdParser = hsubparser
  (  command
      "up-dev"
      (info
        (pure UpDev)
        (progDesc
          "Applies all pending migrations and updates the on-disk checksum. Useful when developing."
        )
      )
  <> command
       "up-deploy"
       (info
         upDeployParser
         (progDesc
           "Applies all pending migrations but does NOT update on-disk checksums. Instead, compares on-disk checksums to database checksums after applying migrations to check whether they match."
         )
       )
  <> command
       "check"
       (info
         analyzeParser
         (progDesc
           "Checks that a SQL migration doesn't fail and checks some of its attributes such as destructiveness, amongst others."
         )
       )
  <> command
       "add"
       (info
         addParser
         (progDesc
           "Adds and applies a SQL migration (and all pending migrations as well), then updates on-disk checksums."
         )
       )
  <> command
       "write-checksums"
       (info
         writeChecksumsParser
         (progDesc
           "Writes files and folders to the checksums's folder that represent the DB's current schema"
         )
       )
  <> command
       "verify-checksums"
       (info
         verifyChecksumParser
         (progDesc
           "Verifies that the Database's current schema matches on-disk checksums."
         )
       )
  )

upDeployParser :: Parser Cmd
upDeployParser =
  UpDeploy
    <$> (   flag'
            Codd.SoftCheck
            (  long "soft-check"
            <> help
                 "Applies and commits all pending migrations and only then compares database and expected checksums, returning an error in case they don't match."
            )

        <|> flag'
              Codd.HardCheck
              (  long "hard-check"
              <> help
                   "If and only if all pending migrations are in-txn, compares database and expected checksums before committing them, but aborts the transaction if they don't match. If there's even one pending no-txn migration, automatically falls back to soft checking."
              )
        )

analyzeParser :: Parser Cmd
analyzeParser = Analyze <$> verbositySwitch <*> argument
  sqlFilePathReader
  (  metavar "SQL-MIGRATION-PATH"
  <> help "The complete path of the .sql file to be analyzed"
  )

addParser :: Parser Cmd
addParser =
  Add
    <$> (   AddMigrationOptions
        <$> switch
              (  long "dont-apply"
              <> help
                   "Do not apply any pending migrations, including the one being added."
              )
        <*> switch
              (  long "no-parse"
              <> help
                   "Use only in case Codd's parsing fails for some reason but you're certain the SQL is valid. Disabling parsing means the migration will be treated as in-txn and COPY FROM STDIN will not be supported."
              )
        )
    <*> optionalStrOption
          (  long "dest-folder"
          <> help
               "Specify the folder path where the .sql migration shall be put. If unspecified, the first folder in the 'CODD_MIGRATION_DIRS' environment variable will be used"
          <> metavar "DESTFOLDER"
          )
    <*> verbositySwitch
    <*> argument
          sqlFilePathReader
          (  metavar "SQL-MIGRATION-PATH"
          <> help "The complete path of the .sql file to be added"
          )

writeChecksumsParser :: Parser Cmd
writeChecksumsParser =
  fmap WriteChecksum
    $   flag'
          WriteToStdout
          (  long "to-stdout"
          <> help
               "Writes a JSON representation of the DB schema to stdout. If this option is supplied, 'dest-folder' is ignored"
          )
    <|> (WriteToDisk <$> optionalStrOption
          (  long "dest-folder"
          <> help
               "The path to a folder where all the files an directories representing the DB's schema will be persisted to"
          )
        )

verifyChecksumParser :: Parser Cmd
verifyChecksumParser = VerifyChecksum <$> verbositySwitch <*> switch
  (  long "from-stdin"
  <> help
       "Reads a JSON representation of the expected checksums from stdin (also see 'codd write-checkums'), instead of using on-disk checksums."
  )

sqlFilePathReader :: ReadM SqlFilePath
sqlFilePathReader = fmap SqlFilePath $ eitherReader $ \s ->
  if ".sql" `List.isSuffixOf` s
    then Right s
    else Left "A SQL file needs to have the '.sql' extension."

optionalStrOption
  :: (Eq s, IsString s) => Mod OptionFields s -> Parser (Maybe s)
optionalStrOption optFields = nothingOnEmpty
  <$> strOption (optFields <> value "")
 where
  nothingOnEmpty "" = Nothing
  nothingOnEmpty v  = Just v

verbositySwitch :: Parser Verbosity
verbositySwitch =
  switch
      (long "verbose" <> short 'v' <> help
        "Prints detailed execution information to stdout."
      )
    <&> \case
          True  -> Verbose
          False -> NonVerbose

main :: IO ()
main = do
  parsedCmd <- execParser opts
  dbVcsInfo <- Codd.getCoddSettings
  doWork dbVcsInfo parsedCmd
  where opts = info (cmdParser <**> helper) fullDesc

doWork :: CoddSettings -> Cmd -> IO ()
doWork dbInfo UpDev = runStdoutLoggingT $ do
  -- Important, and we don't have a test for this:
  -- check hashes in the same transaction as migrations
  -- when possible, since that's what "up-deploy" does.
  checksum        <- Codd.applyMigrations dbInfo Codd.NoCheck
  onDiskHashesDir <- either
    pure
    (error
      "This functionality needs a directory to write checksums to. Please report this as a bug."
    )
    (onDiskHashes dbInfo)
  Codd.persistHashesToDisk checksum onDiskHashesDir
doWork dbInfo (UpDeploy checkHashes) =
  runStdoutLoggingT $ void $ Codd.applyMigrations dbInfo checkHashes
doWork dbInfo (Analyze verbosity fp) =
  runVerbosityLogger verbosity $ checkMigrationFile dbInfo fp
doWork dbInfo (Add dontApply destFolder verbosity fp) =
  runVerbosityLogger verbosity $ addMigration dbInfo dontApply destFolder fp
doWork dbInfo (VerifyChecksum verbosity fromStdin) =
  runVerbosityLogger verbosity $ verifyChecksums dbInfo fromStdin
doWork dbInfo (WriteChecksum opts) = writeChecksums dbInfo opts
