module Codd
    ( CoddSettings(..)
    , CheckHashes(..)
    , applyMigrations
    , withDbAndDrop
    ) where

import           Codd.Environment               ( CoddSettings(..)
                                                , superUserInAppDatabaseConnInfo
                                                )
import           Codd.Hashing                   ( DbHashes
                                                , readHashesFromDatabaseWithSettings
                                                , readHashesFromDisk
                                                )
import           Codd.Internal                  ( applyMigrationsInternal
                                                , baseApplyMigsBlock
                                                , dbIdentifier
                                                , hardCheckLastAction
                                                , throwExceptionOnChecksumMismatch
                                                , withConnection
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Control.Monad.Logger           ( MonadLogger
                                                , logInfoN
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import           Prelude                 hiding ( readFile )
import           UnliftIO.Exception             ( bracket )

data CheckHashes = NoCheck | SoftCheck | HardCheck

-- | Creates the new Database if it doesn't yet exist and applies every single migration, returning the Database's checksums after having
-- migrations applied.
applyMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> CheckHashes
    -> m DbHashes
applyMigrations dbInfo@CoddSettings { onDiskHashes, retryPolicy, txnIsolationLvl } checkHashes
    = case checkHashes of
        HardCheck -> do
            eh <- either readHashesFromDisk pure onDiskHashes
            applyMigrationsInternal
                (baseApplyMigsBlock retryPolicy
                                    (hardCheckLastAction dbInfo eh)
                                    txnIsolationLvl
                )
                dbInfo
            pure eh
        SoftCheck -> do
            eh       <- either readHashesFromDisk pure onDiskHashes
            dbCksums <- applyMigrationsInternal
                (baseApplyMigsBlock
                    retryPolicy
                    (\_migBlocks conn ->
                        readHashesFromDatabaseWithSettings dbInfo conn
                    )
                    txnIsolationLvl
                )
                dbInfo
            logInfoN
                "Soft-checking enabled. Comparing Database and expected checksums."
            throwExceptionOnChecksumMismatch dbCksums eh
            pure eh
        NoCheck -> applyMigrationsInternal
            (baseApplyMigsBlock
                retryPolicy
                (\_migBlocks conn ->
                    readHashesFromDatabaseWithSettings dbInfo conn
                )
                txnIsolationLvl
            )
            dbInfo

-- | Brings a Database up to date just like `applyMigrations`, executes the supplied action passing it a Connection String for the Super User and DROPs the Database
-- afterwards. Useful for testing.
withDbAndDrop
    :: (MonadUnliftIO m, MonadLogger m)
    => CoddSettings
    -> (DB.ConnectInfo -> m a)
    -> m a
withDbAndDrop dbInfo f = bracket
    (applyMigrations dbInfo NoCheck)
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
