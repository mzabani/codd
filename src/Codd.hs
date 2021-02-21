module Codd (CoddSettings(..), applyMigrations, withDbAndDrop) where

import Prelude hiding (readFile)
import Codd.Environment (CoddSettings(..), superUserInAppDatabaseConnInfo)
import Codd.Hashing (readHashesFromDisk)
import Codd.Internal (withConnection, applyMigrationsInternal, dbIdentifier, beginCommitTxnBracket, mainAppApplyMigsBlock)
import Control.Monad (void)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO.Exception (bracket)

-- | Creates the new Database if it doesn't yet exist and applies every single migration.
applyMigrations :: (MonadUnliftIO m, MonadIO m, MonadLogger m) => CoddSettings -> Bool -> m ()
applyMigrations dbInfo@CoddSettings { onDiskHashes } checkHashes = do
    expectedDbHash <- if checkHashes then Just <$> either readHashesFromDisk pure onDiskHashes else pure Nothing
    applyMigrationsInternal beginCommitTxnBracket (mainAppApplyMigsBlock dbInfo expectedDbHash) dbInfo

-- | Brings a Database up to date just like `applyMigrations`, executes the supplied action passing it a Connection String for the Super User and DROPs the Database
-- afterwards. Useful for testing.
withDbAndDrop :: (MonadUnliftIO m, MonadLogger m) => CoddSettings -> (DB.ConnectInfo -> m a) -> m a
withDbAndDrop dbInfo f = bracket (applyMigrations dbInfo False) dropDb (const $ f (superUserInAppDatabaseConnInfo dbInfo))
  where
      dropDb _ = do
          withConnection (superUserConnString dbInfo) $ \conn -> void $ liftIO $ DB.execute_ conn $ "DROP DATABASE IF EXISTS " <> dbIdentifier (dbName dbInfo)