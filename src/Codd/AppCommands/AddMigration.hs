module Codd.AppCommands.AddMigration
  ( AddMigrationOptions(..)
  , addMigration
  ) where

import qualified Codd
import           Codd.Analysis                  ( MigrationCheckSimpleWorkflow(..)
                                                , checkMigration
                                                , checkMigrationSimpleWorkflow
                                                , migrationErrors
                                                )
import           Codd.AppCommands               ( timestampAndMoveMigrationFile
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( persistHashesToDisk
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Parsing                   ( ParsingOptions(..)
                                                , parseSqlMigration
                                                )
import           Codd.Types                     ( DeploymentWorkflow(..)
                                                , SqlFilePath(..)
                                                )
import           Control.Monad                  ( forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.Logger           ( MonadLoggerIO )
import           Data.Maybe                     ( maybeToList )
import qualified Data.Text.IO                  as Text
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.FilePath                ( takeFileName )
import           UnliftIO                       ( MonadUnliftIO
                                                , liftIO
                                                , stderr
                                                )
import           UnliftIO.Directory             ( copyFile
                                                , doesFileExist
                                                , removeFile
                                                )
import           UnliftIO.Exception             ( bracketOnError )

data AddMigrationOptions = AddMigrationOptions
  { dontApply :: Bool
  , noParse   :: Bool
  }

addMigration
  :: forall m
   . (MonadUnliftIO m, MonadLoggerIO m)
  => CoddSettings
  -> AddMigrationOptions
  -> Maybe FilePath
  -> SqlFilePath
  -> m ()
addMigration dbInfo@Codd.CoddSettings { sqlMigrations, onDiskHashes, deploymentWorkflow } AddMigrationOptions { dontApply, noParse } destFolder sqlFp@(SqlFilePath fp)
  = do
    finalDir <- case (destFolder, sqlMigrations) of
      (Just f, _) -> pure f
      (Nothing, Left []) ->
        error
          "Please specify '--dest-folder' or add at least one path to the CODD_MIGRATION_DIRS environment variable."
      (Nothing, Left (f : _)) -> pure f
      (Nothing, Right _) ->
        error "This is a bug in the add command. Please report it."
    onDiskHashesDir <- either
      pure
      (error
        "This functionality needs a directory to write the DB checksum to. Report this as a bug."
      )
      onDiskHashes
    exists <- doesFileExist fp
    unless exists $ error $ "Could not find file " ++ fp
    sqlMigContents <- liftIO $ Text.readFile fp
    let parsedSqlMigE = parseSqlMigration
          deploymentWorkflow
          (if noParse then NoParse else DoParse)
          (takeFileName fp)
          sqlMigContents
    case parsedSqlMigE of
      Left err ->
        error $ "There was an error parsing this SQL Migration: " ++ show err
      Right sqlMig -> do
        migErrors <- case deploymentWorkflow of
          SimpleDeployment ->
            either (pure . (: []))
                   (pure . maybeToList . transactionManagementProblem)
              $ checkMigrationSimpleWorkflow sqlMig
          BlueGreenSafeDeploymentUpToAndIncluding{} -> do
            migCheck <- checkMigration dbInfo sqlMig
            pure $ migrationErrors sqlMig migCheck

        when (migErrors /= []) $ liftIO $ do
          forM_ migErrors (Text.hPutStrLn stderr)
          exitWith (ExitFailure 1)

        bracketOnError (timestampAndMoveMigrationFile sqlFp finalDir)
                       moveMigrationBack
          $ \finalMigFile -> do
              unless dontApply $ do
                -- Important, and we don't have a test for this:
                -- fetch hashes in the same transaction as migrations
                -- when possible, since that's what "up" does.
                databaseChecksums <- Codd.applyMigrationsNoCheck
                  dbInfo
                  (readHashesFromDatabaseWithSettings dbInfo)
                persistHashesToDisk databaseChecksums onDiskHashesDir

                liftIO
                  $  putStrLn
                  $  "Migration applied and added to "
                  <> finalMigFile
              when dontApply
                $  liftIO
                $  putStrLn
                $  "Migration was NOT applied, but was added to "
                <> finalMigFile

 where
  moveMigrationBack :: FilePath -> m ()
  moveMigrationBack deleteFrom = do
    copyFile deleteFrom fp
    removeFile deleteFrom
