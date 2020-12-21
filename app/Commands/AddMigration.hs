module Commands.AddMigration (addMigration) where

import qualified Codd as Codd
import Codd.AppCommands (timestampAndMoveMigrationFile)
import Codd.Analysis (checkMigration, migrationErrors, canRunEverythingInASingleTransaction)
import Codd.Environment (superUserInAppDatabaseConnInfo)
import Codd.Hashing (readHashesFromDatabaseWithSettings, persistHashesToDisk)
import Codd.Internal (withConnection)
import Codd.Parsing (parseSqlMigration)
import Codd.Types (CoddSettings(..), SqlFilePath(..))
import Control.Monad (when, unless, forM_)
import qualified Data.Text.IO as Text
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (takeFileName)
import UnliftIO.Directory (copyFile, doesFileExist, removeFile)
import UnliftIO.Exception (bracketOnError)

addMigration :: CoddSettings -> Bool -> Maybe FilePath -> SqlFilePath -> IO ()
addMigration dbInfo@(Codd.CoddSettings { sqlMigrations, onDiskHashes, deploymentWorkflow }) dontApply destFolder sqlFp@(SqlFilePath fp) = do
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
      -- If the User wants to apply pending migrations and the newly added one as well,
      -- there's a possible optimization: iff all pending migrations plus the newly added one are in-txn,
      -- we don't need to check the migration as a separate step; we can just add and apply.
      canRunInTxn <- canRunEverythingInASingleTransaction dbInfo sqlMig
      let skipCheck = not dontApply && canRunInTxn
      unless skipCheck $ do
        migCheck <- checkMigration dbInfo sqlMig
        let migErrors = migrationErrors sqlMig migCheck

        when (migErrors /= []) $ do
          forM_ migErrors putStrLn
          exitWith (ExitFailure 1)
      
      bracketOnError (timestampAndMoveMigrationFile sqlFp finalDir) moveMigrationBack $ \finalMigFile -> do
        unless dontApply $ do
          Codd.applyMigrations dbInfo False
          hashes <- withConnection (superUserInAppDatabaseConnInfo dbInfo) (readHashesFromDatabaseWithSettings dbInfo)
          persistHashesToDisk hashes onDiskHashesDir
        putStrLn $ "Migration added to " ++ finalMigFile

  where
    moveMigrationBack :: FilePath -> IO ()
    moveMigrationBack deleteFrom = do
      copyFile deleteFrom fp
      removeFile deleteFrom