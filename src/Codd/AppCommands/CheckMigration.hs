module Codd.AppCommands.CheckMigration (checkMigrationFile) where

import Codd.Analysis (checkMigration, migrationErrors)
import Codd.Environment (CoddSettings(..))
import Codd.Parsing (ParsingOptions(..), parseSqlMigration)
import Codd.Types (SqlFilePath(..))
import Control.Monad (unless, forM_)
import Control.Monad.Logger (MonadLoggerIO)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath (takeFileName)
import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.Directory (doesFileExist)

checkMigrationFile :: (MonadUnliftIO m, MonadLoggerIO m) => CoddSettings -> SqlFilePath -> m ()
checkMigrationFile dbInfo (SqlFilePath fp) = do
  exists <- doesFileExist fp
  unless exists $ error $ "Could not find file " ++ fp
  sqlMigContents <- liftIO $ Text.readFile fp
  let parsedSqlMigE = parseSqlMigration (deploymentWorkflow dbInfo) NoParse (takeFileName fp) sqlMigContents
  case parsedSqlMigE of
    Left err -> error $ "There was an error parsing this SQL Migration: " ++ show err
    Right sqlMig -> do
      migCheck <- checkMigration dbInfo sqlMig
      liftIO $ putStrLn "--- Parsed SQL migration:"
      liftIO $ print sqlMig
      
      liftIO $ putStrLn "\n--- Check results:"
      liftIO $ print migCheck

      let migErrors = migrationErrors sqlMig migCheck
      if migErrors == [] then liftIO $ putStrLn "\n--- No errors. Can be added."
      else liftIO $ do
        putStrLn "\n--- Has errors:"
        forM_ migErrors (putStrLn . Text.unpack)