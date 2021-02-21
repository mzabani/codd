module Commands.VerifyDb (verifyDb) where

import Codd.Environment (CoddSettings(..), superUserInAppDatabaseConnInfo)
import Codd.Hashing (readHashesFromDatabaseWithSettings, readHashesFromDisk)
import Codd.Internal (withConnection)
import Control.Monad (when)
import Control.Monad.Logger (MonadLoggerIO)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import UnliftIO (MonadUnliftIO, liftIO)

verifyDb :: (MonadUnliftIO m, MonadLoggerIO m) => CoddSettings -> m ()
verifyDb dbInfoWithAllMigs@CoddSettings { onDiskHashes } = do
  let dbInfoDontApplyAnything = dbInfoWithAllMigs {
    sqlMigrations = Right []
  }
  let adminConnInfo = superUserInAppDatabaseConnInfo dbInfoDontApplyAnything
  onDiskHashesDir <- either pure (error "This functionality needs a directory to write hashes to. Report this as a bug.") onDiskHashes
  dbHashes <- withConnection adminConnInfo (readHashesFromDatabaseWithSettings dbInfoDontApplyAnything)
  diskHashes <- readHashesFromDisk onDiskHashesDir
  when (dbHashes /= diskHashes) $ liftIO $ do
    hPutStrLn stderr "DB and on-disk hashes do not match."
    exitWith (ExitFailure 1)
  liftIO $ putStrLn "DB and on-disk hashes match."