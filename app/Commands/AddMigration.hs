module Commands.AddMigration (addMigration) where

import qualified Codd as Codd
import Codd.AppCommands (timestampAndMoveMigrationFile)
import Codd.Analysis (checkMigration, migrationErrors)
import Codd.Environment (superUserInAppDatabaseConnInfo)
import Codd.Hashing (readHashesFromDatabaseWithSettings, persistHashesToDisk)
import Codd.Internal (connectAndDispose)
import Codd.Parsing (parseSqlMigration)
import Codd.Types (CoddSettings(..), SqlFilePath(..))
import Control.Monad (when, unless, forM_)
import qualified Data.Text.IO as Text
import System.FilePath (takeFileName)
import UnliftIO.Directory (doesFileExist)

addMigration :: CoddSettings -> Bool -> Maybe FilePath -> SqlFilePath -> IO ()
addMigration dbInfo@(Codd.CoddSettings { sqlMigrations, onDiskHashes, deploymentWorkflow }) alsoApply destFolder sqlFp@(SqlFilePath fp) = do
  finalDir <- case (destFolder, sqlMigrations) of
        (Just f, _) -> pure f
        (Nothing, Left []) -> error "Please specify '--dest-folder' or add at least one path to the SQL_MIGRATION_PATHS environment variable."
        (Nothing, Left (f:_)) -> pure f
        (Nothing, Right _) -> error "This is a bug in the add command. Please report it."
  onDiskHashesDir <- either pure (error "This functionality needs a directory to write hashes to. Report this as a bug.") onDiskHashes
  exists <- doesFileExist fp
  unless exists $ error $ "Could not find file " ++ fp
  sqlMigContents <- Text.readFile fp
  let parsedSqlMigE = parseSqlMigration deploymentWorkflow (takeFileName fp) sqlMigContents
  case parsedSqlMigE of
    Left err -> error $ "There was an error parsing this SQL Migration: " ++ show err
    Right sqlMig -> do
      migCheck <- checkMigration dbInfo sqlMig
      let migErrors = migrationErrors sqlMig migCheck

      -- TODO: Run in some MonadError to be able to return!
      if migErrors /= [] then forM_ migErrors putStrLn
      else do
        -- TODO: Don't allow adding a migration with the same timestamp as another!
        finalMigFile <- timestampAndMoveMigrationFile sqlFp finalDir
        putStrLn $ "Migration added to " ++ finalMigFile
        when alsoApply $ do
            Codd.applyMigrations dbInfo False
            hashes <- connectAndDispose (superUserInAppDatabaseConnInfo dbInfo) (readHashesFromDatabaseWithSettings dbInfo)
            persistHashesToDisk hashes onDiskHashesDir