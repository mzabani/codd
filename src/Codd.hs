module Codd (DbVcsInfo(..), applyMigrations, withDbAndDrop) where

import Prelude hiding (readFile)
import Codd.Internal (connectAndDispose, applyMigrationsInternal, dbIdentifier, beginCommitTxnBracket, mainAppApplyMigsBlock)
import Codd.Parsing (parseSqlMigration)
import Codd.Types (DbVcsInfo(..), ApplyMigrations)
import Control.Monad (void, when, forM, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ByteString (readFile)
import Data.String (fromString)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (listDirectory)
import UnliftIO.Exception (bracket, catchAny, throwIO, tryAny)

import qualified Data.List as List


-- | Creates the new Database if necessary, applies every single migration and returns a Connection String
--   that allows the Application User to connect (not the superuser).
applyMigrations :: (MonadUnliftIO m, MonadIO m) => DbVcsInfo -> ApplyMigrations -> m DB.ConnectInfo
applyMigrations dbInfo@(DbVcsInfo { superUserConnString, dbName, appUser, sqlMigrations }) applyType = do
    applyMigrationsInternal beginCommitTxnBracket mainAppApplyMigsBlock dbInfo applyType
    return superUserConnString { DB.connectDatabase = dbName, DB.connectUser = appUser }

-- | Brings a Database up to date just like `applyMigrations`, executes the supplied action and DROPs the Database
-- afterwards. Useful for testing.
withDbAndDrop :: MonadUnliftIO m => DbVcsInfo -> ApplyMigrations -> (DB.ConnectInfo -> m a) -> m a
withDbAndDrop dbInfo applyType f = bracket (applyMigrations dbInfo applyType) dropDb f
  where
      dropDb _ = do
          connectAndDispose (superUserConnString dbInfo) $ \conn -> void $ liftIO $ DB.execute_ conn $ "DROP DATABASE IF EXISTS " <> dbIdentifier (dbName dbInfo)