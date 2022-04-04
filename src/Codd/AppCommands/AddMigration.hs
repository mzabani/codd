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
import           Codd.Hashing                   ( persistHashesToDisk
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Internal                  ( streamingReadFile )
import           Codd.Parsing                   ( parseSqlMigration )
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
   . (MonadUnliftIO m, MonadLoggerIO m, MonadThrow m)
  => CoddSettings
  -> AddMigrationOptions
  -> Maybe FilePath
  -> SqlFilePath
  -> m ()
addMigration dbInfo@Codd.CoddSettings { sqlMigrations, onDiskHashes } AddMigrationOptions { dontApply } destFolder sqlFp@(SqlFilePath fp)
  = do
    finalDir <- case (destFolder, sqlMigrations) of
      (Just f, _) -> pure f
      (Nothing, []) ->
        error
          "Please specify '--dest-folder' or add at least one path to the CODD_MIGRATION_DIRS environment variable."
      (Nothing, f : _) -> pure f
    onDiskHashesDir <- either
      pure
      (error
        "This functionality needs a directory to write the DB checksum to. Report this as a bug."
      )
      onDiskHashes
    exists <- doesFileExist fp
    unless exists $ error $ "Could not find file " ++ fp
    runResourceT $ do
      (_, sqlMigContents) <- streamingReadFile fp
      parsedSqlMigE       <- parseSqlMigration (takeFileName fp) sqlMigContents
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
            -- fetch hashes in the same transaction as migrations
            -- when possible, since that's what "up" does.
            databaseChecksums <- Codd.applyMigrationsNoCheck
              dbInfo
              Nothing
              (secondsToDiffTime 5)
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
