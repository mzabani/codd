module Commands.VerifyDb (verifyDb) where

import Codd.Environment (superUserInAppDatabaseConnInfo)
import Codd.Types (CoddSettings(..))
import Codd.Hashing (readHashesFromDatabaseWithSettings, readHashesFromDisk)
import Codd.Internal (connectAndDispose)
import Control.Monad (when)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

verifyDb :: CoddSettings -> Bool -> IO ()
verifyDb dbInfoWithAllMigs@CoddSettings { onDiskHashes } verbose = do
  let dbInfoDontApplyAnything = dbInfoWithAllMigs {
    sqlMigrations = Right []
  }
  let adminConnInfo = superUserInAppDatabaseConnInfo dbInfoDontApplyAnything
  onDiskHashesDir <- either pure (error "This functionality needs a directory to write hashes to. Report this as a bug.") onDiskHashes
  dbHashes <- connectAndDispose adminConnInfo (readHashesFromDatabaseWithSettings dbInfoDontApplyAnything)
  diskHashes <- readHashesFromDisk onDiskHashesDir
  when (dbHashes /= diskHashes) $ do
    when verbose $ hPutStrLn stderr "DB and on-disk hashes do not match."
    exitWith (ExitFailure 1)
  when verbose $ putStrLn "DB and on-disk hashes match."