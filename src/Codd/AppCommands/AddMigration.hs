{-# OPTIONS_GHC -Wno-unused-matches #-}
module Codd.AppCommands.AddMigration
  ( AddMigrationOptions(..)
  , addMigration
  ) where

import qualified Codd
import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.AppCommands               ( timestampAndMoveMigrationFile
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( streamingReadFile )
import           Codd.Parsing                   ( EnvVars
                                                , parseSqlMigration
                                                )
import           Codd.Representations           ( persistRepsToDisk
                                                , readRepresentationsFromDbWithSettings
                                                )
import           Codd.Types                     ( SqlFilePath(..) )
import           Control.Monad                  ( forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.Logger           ( MonadLoggerIO )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import           Data.Maybe                     ( maybeToList )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( secondsToDiffTime )
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
import           UnliftIO.Resource              ( runResourceT )

newtype AddMigrationOptions = AddMigrationOptions
  { dontApply :: Bool
  }

addMigration
  :: forall m
   . (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m, EnvVars m)
  => CoddSettings
  -> AddMigrationOptions
  -> Maybe FilePath
  -> SqlFilePath
  -> m ()
addMigration dbInfo@Codd.CoddSettings { sqlMigrations, onDiskReps } AddMigrationOptions { dontApply } destFolder sqlFp@(SqlFilePath fp)
  = do
    finalDir <- case (destFolder, sqlMigrations) of
      (Just f, _) -> pure f
      (Nothing, []) ->
        error
          "Please specify '--dest-folder' or add at least one path to the CODD_MIGRATION_DIRS environment variable."
      (Nothing, f : _) -> pure f
    onDiskRepsDir <- either
      pure
      (error
        "This functionality needs a directory to write the expected representations to. Report this as a bug."
      )
      onDiskReps
    exists <- doesFileExist fp
    unless exists $ error $ "Could not find file " ++ fp
    runResourceT $ do
      migStream     <- streamingReadFile fp
      parsedSqlMigE <- parseSqlMigration (takeFileName fp) migStream
      case parsedSqlMigE of
        Left err -> do
          liftIO
            $  Text.hPutStrLn stderr
            $  "Could not add migration: "
            <> Text.pack err
          liftIO $ exitWith (ExitFailure 1)
        Right sqlMig -> do
          migCheck  <- checkMigration sqlMig
          migErrors <- either
            (pure . (: []))
            (pure . maybeToList . transactionManagementProblem)
            migCheck

          when (migErrors /= []) $ liftIO $ do
            Text.hPutStrLn stderr "Analysis of the migration detected errors."
            forM_ migErrors (Text.hPutStrLn stderr)
            exitWith (ExitFailure 1)

    bracketOnError (timestampAndMoveMigrationFile sqlFp finalDir)
                   moveMigrationBack
      $ \finalMigFile -> do
          unless dontApply $ do
            -- Important, and we don't have a test for this:
            -- fetch representations in the same transaction as migrations
            -- when possible, since that's what "up" does.
            databaseSchemas <- Codd.applyMigrationsNoCheck
              dbInfo
              Nothing
              (secondsToDiffTime 5)
              (readRepresentationsFromDbWithSettings dbInfo)
            persistRepsToDisk databaseSchemas onDiskRepsDir

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
