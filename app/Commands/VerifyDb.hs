module Commands.VerifyDb (verifyDb) where

import Codd.Environment (superUserInAppDatabaseConnInfo)
import Codd.Types (DbVcsInfo(..))
import Codd.Hashing (readHashesFromDatabase, readHashesFromDisk)
import Codd.Internal (connectAndDispose)
import Control.Monad (when)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

verifyDb :: DbVcsInfo -> Bool -> IO ()
verifyDb dbInfoWithAllMigs@DbVcsInfo { diskHashesDir } verbose = do
  let dbInfoDontApplyAnything = dbInfoWithAllMigs {
    sqlMigrations = Right []
  }
  let adminConnInfo = superUserInAppDatabaseConnInfo dbInfoDontApplyAnything
  dbHashes <- connectAndDispose adminConnInfo readHashesFromDatabase
  diskHashes <- readHashesFromDisk diskHashesDir
  when (dbHashes /= diskHashes) $ do
    when verbose $ hPutStrLn stderr "DB and on-disk hashes do not match."
    exitWith (ExitFailure 1)
  when verbose $ putStrLn "DB and on-disk hashes match."