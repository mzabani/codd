module Codd (DbVcsInfo(..), applyMigrations, withDbAndDrop) where

import Prelude hiding (readFile)
import Codd.Environment (appUserInAppDatabaseConnInfo)
import Codd.Hashing (readHashesFromDisk)
import Codd.Internal (connectAndDispose, applyMigrationsInternal, dbIdentifier, beginCommitTxnBracket, mainAppApplyMigsBlock)
import Codd.Types (DbVcsInfo(..), ApplyMigrations)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO.Exception (bracket)


-- | Creates the new Database if necessary and applies every single migration.
applyMigrations :: (MonadUnliftIO m, MonadIO m) => DbVcsInfo -> ApplyMigrations -> Bool -> m ()
applyMigrations dbInfo@DbVcsInfo { onDiskHashes } applyType checkHashes = do
    onDiskHashCheck <- if checkHashes then Just <$> either readHashesFromDisk pure onDiskHashes else pure Nothing
    applyMigrationsInternal beginCommitTxnBracket mainAppApplyMigsBlock dbInfo applyType onDiskHashCheck

-- | Brings a Database up to date just like `applyMigrations`, executes the supplied action in a Connection with the App's User and DROPs the Database
-- afterwards. Useful for testing.
withDbAndDrop :: MonadUnliftIO m => DbVcsInfo -> ApplyMigrations -> (DB.ConnectInfo -> m a) -> m a
withDbAndDrop dbInfo applyType f = bracket (applyMigrations dbInfo applyType False) dropDb (const $ f (appUserInAppDatabaseConnInfo dbInfo))
  where
      dropDb _ = do
          connectAndDispose (superUserConnString dbInfo) $ \conn -> void $ liftIO $ DB.execute_ conn $ "DROP DATABASE IF EXISTS " <> dbIdentifier (dbName dbInfo)