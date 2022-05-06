module Main where

import qualified Codd
import           Codd.AppCommands.AddMigration  ( AddMigrationOptions(..)
                                                , addMigration
                                                )
import           Codd.AppCommands.VerifyChecksums
                                                ( verifyChecksums )
import           Codd.AppCommands.WriteChecksums
                                                ( WriteChecksumsOpts(..)
                                                , writeChecksums
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import qualified Codd.Environment              as Codd
import           Codd.Logging                   ( Verbosity(..)
                                                , runVerbosityLogger
                                                )
import           Codd.Types                     ( SqlFilePath(..) )
import           Control.Monad                  ( void )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Data.Functor                   ( (<&>) )
import qualified Data.List                     as List
import           Data.String                    ( IsString )
import           Data.Time                      ( DiffTime )
import           Options.Applicative
import qualified System.IO                     as IO
import qualified Text.Read                     as Text

data Cmd = Up (Maybe Codd.CheckHashes) DiffTime | Add AddMigrationOptions (Maybe FilePath) Verbosity SqlFilePath | WriteChecksum WriteChecksumsOpts | VerifyChecksum Verbosity Bool

cmdParser :: Parser Cmd
cmdParser = hsubparser
  (  command
      "up"
      (info
        upParser
        (progDesc
          "Applies all pending migrations and possibly compares on-disk checksums to database checksums afterwards to check whether they match. The default mode of operation when none are provided is lax-checking."
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

upParser :: Parser Cmd
upParser =
  Up
    <$> (   flag'
            (Just Codd.LaxCheck)
            (  long "lax-check"
            <> short 'l'
            <> help
                 "Applies and commits all pending migrations and only then compares database and expected checksums, logging mismatches but returning a success status unless a migration fails."
            )
        <|> flag'
              (Just Codd.StrictCheck)
              (  long "strict-check"
              <> short 's'
              <> help
                   "If and only if all pending migrations are in-txn, compares database and expected checksums before committing them, but aborts the transaction if they don't match.\
                    \\nIf there's even one pending no-txn migration, this mode _will_ commit all migrations and verify checksums after that, exiting with an error code if they don't match."
              )
        <|> flag'
              Nothing
              (  long "no-check"
              <> short 'n'
              <> help
                   "Applies and commits all pending migrations and does not compare checksums. Returns a success status unless a migration fails."
              )
        <|> pure (Just Codd.LaxCheck)
        )
    <*> optionalSecondsOption
          5
          (  long "wait"
          <> short 'w'
          <> metavar "TIME_SECS"
          <> help
               "How long in seconds to wait for the database to be available before giving up. Defaults to 5 seconds if unspecified."
          )

addParser :: Parser Cmd
addParser =
  Add
    <$> (AddMigrationOptions <$> switch
          (  long "no-apply"
          <> help
               "Do not apply any pending migrations, including the one being added."
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
verifyChecksumParser = VerifyChecksum <$> quietSwitch <*> switch
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

optionalSecondsOption :: Int -> Mod OptionFields Int -> Parser DiffTime
optionalSecondsOption defaultValue optFields = realToFrac
  <$> option intParser (optFields <> value defaultValue)
  -- Watch out: DiffTime's Read instance reads value with an "s" suffixed!
  where intParser = maybeReader (Text.readMaybe @Int)

verbositySwitch :: Parser Verbosity
verbositySwitch =
  switch
      (long "verbose" <> short 'v' <> help
        "Prints detailed execution information to stdout."
      )
    <&> \case
          True  -> Verbose
          False -> NonVerbose

quietSwitch :: Parser Verbosity
quietSwitch =
  switch (long "quiet" <> short 'q' <> help "Hides some of the output.")
    <&> \case
          True  -> NonVerbose
          False -> Verbose

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stderr IO.NoBuffering

  parsedCmd <- execParser opts
  dbVcsInfo <- Codd.getCoddSettings
  doWork dbVcsInfo parsedCmd
  where opts = info (cmdParser <**> helper) fullDesc

doWork :: CoddSettings -> Cmd -> IO ()
doWork dbInfo (Up mCheckHashes connectTimeout) =
  runStdoutLoggingT $ case mCheckHashes of
    Nothing -> Codd.applyMigrationsNoCheck dbInfo
                                           Nothing
                                           connectTimeout
                                           (const $ pure ())
    Just checkHashes ->
      void $ Codd.applyMigrations dbInfo Nothing connectTimeout checkHashes
doWork dbInfo (Add addOpts destFolder verbosity fp) =
  runVerbosityLogger verbosity $ addMigration dbInfo addOpts destFolder fp
doWork dbInfo (VerifyChecksum verbosity fromStdin) =
  runVerbosityLogger verbosity $ verifyChecksums dbInfo fromStdin
doWork dbInfo (WriteChecksum opts) = writeChecksums dbInfo opts
