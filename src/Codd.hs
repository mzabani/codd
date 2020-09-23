module Codd (CoddSettings(..), applyMigrations, withDbAndDrop) where

import Prelude hiding (readFile)
import Codd.Environment (appUserInAppDatabaseConnInfo)
import Codd.Hashing (readHashesFromDisk)
import Codd.Internal (connectAndDispose, applyMigrationsInternal, dbIdentifier, beginCommitTxnBracket, mainAppApplyMigsBlock)
import Codd.Types (CoddSettings(..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO.Exception (bracket)


-- | Creates the new Database if necessary and applies every single migration.
applyMigrations :: (MonadUnliftIO m, MonadIO m) => CoddSettings -> Bool -> m ()
applyMigrations dbInfo@CoddSettings { onDiskHashes } checkHashes = do
    onDiskHashCheck <- if checkHashes then Just <$> either readHashesFromDisk pure onDiskHashes else pure Nothing
    applyMigrationsInternal beginCommitTxnBracket mainAppApplyMigsBlock dbInfo onDiskHashCheck

-- | Brings a Database up to date just like `applyMigrations`, executes the supplied action in a Connection with the App's User and DROPs the Database
-- afterwards. Useful for testing.
withDbAndDrop :: MonadUnliftIO m => CoddSettings -> (DB.ConnectInfo -> m a) -> m a
withDbAndDrop dbInfo f = bracket (applyMigrations dbInfo False) dropDb (const $ f (appUserInAppDatabaseConnInfo dbInfo))
  where
      dropDb _ = do
          connectAndDispose (superUserConnString dbInfo) $ \conn -> void $ liftIO $ DB.execute_ conn $ "DROP DATABASE IF EXISTS " <> dbIdentifier (dbName dbInfo)