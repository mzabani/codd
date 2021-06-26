module Codd.AppCommands.UpDeploy
  ( upDeploy
  ) where

import qualified Codd
import           Codd.Analysis                  ( canRunEverythingInASingleTransaction
                                                , checkMigration
                                                , migrationErrors
                                                )
import           Codd.AppCommands               ( timestampAndMoveMigrationFile
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( persistHashesToDisk )
import           Codd.Parsing                   ( ParsingOptions(..)
                                                , parseSqlMigration
                                                )
import           Codd.Types                     ( SqlFilePath(..) )
import           Control.Monad                  ( forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.Logger           ( MonadLoggerIO )
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

upDeploy
  :: forall m
   . (MonadUnliftIO m, MonadLoggerIO m)
  => CoddSettings
  -> Codd.CheckHashes
  -> m ()
addMigration dbInfo@Codd.CoddSettings { sqlMigrations, onDiskHashes, deploymentWorkflow } checkHashes
  = Codd.applyMigrations dbInfo checkHashes
