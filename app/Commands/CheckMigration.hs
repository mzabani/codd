module Commands.CheckMigration (checkMigrationFile) where

import Codd.Analysis (checkMigration, migrationErrors)
import Codd.Parsing (parseSqlMigration)
import Codd.Types (DbVcsInfo, SqlFilePath(..))
import Control.Monad (unless, forM_)
import qualified Data.Text.IO as Text
import System.FilePath (takeFileName)
import UnliftIO.Directory (doesFileExist)

checkMigrationFile :: DbVcsInfo -> SqlFilePath -> IO ()
checkMigrationFile dbInfo (SqlFilePath fp) = do
  exists <- doesFileExist fp
  unless exists $ error $ "Could not find file " ++ fp
  sqlMigContents <- Text.readFile fp
  let parsedSqlMigE = parseSqlMigration (takeFileName fp) sqlMigContents
  case parsedSqlMigE of
    Left err -> error $ "There was an error parsing this SQL Migration: " ++ show err
    Right sqlMig -> do
      migCheck <- checkMigration dbInfo sqlMig
      putStrLn "--- Parsed SQL migration:"
      print sqlMig
      
      putStrLn "\n--- Check results:"
      print migCheck

      let migErrors = migrationErrors sqlMig migCheck
      if migErrors == [] then putStrLn "\n--- No errors. Can be added."
      else do
        putStrLn "\n--- Has errors:"
        forM_ migErrors putStrLn