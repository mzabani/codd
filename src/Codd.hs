module Codd
    ( CoddSettings(..)
    , applyMigrations
    , withDbAndDrop
    ) where

import           Codd.Environment               ( CoddSettings(..)
                                                , superUserInAppDatabaseConnInfo
                                                )
import           Codd.Hashing                   ( readHashesFromDisk )
import           Codd.Internal                  ( CheckHashes(..)
                                                , applyMigrationsInternal
                                                , baseApplyMigsBlock
                                                , beginCommitTxnBracket
                                                , dbIdentifier
                                                , withConnection
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Control.Monad.Logger           ( MonadLogger )
import qualified Database.PostgreSQL.Simple    as DB
import           Prelude                 hiding ( readFile )
import           UnliftIO.Exception             ( bracket )

-- | Creates the new Database if it doesn't yet exist and applies every single migration.
applyMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> Bool
    -> m ()
applyMigrations dbInfo@CoddSettings { onDiskHashes } checkHashes = do
    if checkHashes
        then do
            eh <- either readHashesFromDisk pure onDiskHashes
            applyMigrationsInternal
                beginCommitTxnBracket
                (baseApplyMigsBlock (DoCheckHashes dbInfo eh) (const $ pure ()))
                dbInfo
        else applyMigrationsInternal
            beginCommitTxnBracket
            (baseApplyMigsBlock DontCheckHashes (const $ pure ()))
            dbInfo

-- | Brings a Database up to date just like `applyMigrations`, executes the supplied action passing it a Connection String for the Super User and DROPs the Database
-- afterwards. Useful for testing.
withDbAndDrop
    :: (MonadUnliftIO m, MonadLogger m)
    => CoddSettings
    -> (DB.ConnectInfo -> m a)
    -> m a
withDbAndDrop dbInfo f = bracket
    (applyMigrations dbInfo False)
    dropDb
    (const $ f (superUserInAppDatabaseConnInfo dbInfo))
  where
    dropDb _ = do
        withConnection (superUserConnString dbInfo) $ \conn ->
            void
                $  liftIO
                $  DB.execute_ conn
                $  "DROP DATABASE IF EXISTS "
                <> dbIdentifier (dbName dbInfo)
