module Codd
    ( ApplyResult(..)
    , ChecksumsPair(..)
    , CoddSettings(..)
    , CheckHashes(..)
    , applyMigrations
    , applyMigrationsNoCheck
    , withDbAndDrop
    ) where

import           Codd.Environment               ( CoddSettings(..)
                                                , superUserInAppDatabaseConnInfo
                                                )
import           Codd.Hashing                   ( DbHashes
                                                , readHashesFromDisk
                                                )
import           Codd.Internal                  ( applyMigrationsInternal
                                                , baseApplyMigsBlock
                                                , dbIdentifier
                                                , hardCheckLastAction
                                                , softCheckLastAction
                                                , withConnection
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Control.Monad.Logger           ( MonadLogger )
import qualified Database.PostgreSQL.Simple    as DB
import           Prelude                 hiding ( readFile )
import           UnliftIO.Exception             ( bracket )

data CheckHashes = SoftCheck | HardCheck
data ChecksumsPair = ChecksumsPair
    { expectedChecksums :: DbHashes
    , databaseChecksums :: DbHashes
    }
data ApplyResult = ChecksumsDiffer ChecksumsPair | ChecksumsMatch DbHashes | ChecksumsNotVerified

-- | Creates the new Database if it doesn't yet exist and applies every single migration, returning
-- the Database's checksums if they're not the ones expected or a success result otherwise.
-- Throws an exception if a migration fails or if checksums mismatch and hard-checking is enabled.
applyMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> CheckHashes
    -> m ApplyResult
applyMigrations dbInfo@CoddSettings { superUserConnString, onDiskHashes, retryPolicy, txnIsolationLvl } checkHashes
    = case checkHashes of
        HardCheck -> do
            eh <- either readHashesFromDisk pure onDiskHashes
            applyMigrationsInternal
                (baseApplyMigsBlock superUserConnString
                                    retryPolicy
                                    (hardCheckLastAction dbInfo eh)
                                    txnIsolationLvl
                )
                dbInfo
            pure $ ChecksumsMatch eh
        SoftCheck -> do
            eh       <- either readHashesFromDisk pure onDiskHashes
            dbCksums <- applyMigrationsInternal
                (baseApplyMigsBlock superUserConnString
                                    retryPolicy
                                    (softCheckLastAction dbInfo eh)
                                    txnIsolationLvl
                )
                dbInfo

            if dbCksums /= eh
                then pure $ ChecksumsDiffer $ ChecksumsPair
                    { expectedChecksums = eh
                    , databaseChecksums = dbCksums
                    }
                else pure $ ChecksumsMatch eh

-- | Creates the new Database if it doesn't yet exist and applies every single migration.
-- Does not verify checksums but allows a function that runs in the same transaction as the last migrations
-- iff all migrations are in-txn or separately after the last migration and not in an explicit transaction
-- otherwise.
-- Throws an exception if a migration fails.
applyMigrationsNoCheck
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> (DB.Connection -> m a)
    -> m a
applyMigrationsNoCheck dbInfo@CoddSettings { superUserConnString, retryPolicy, txnIsolationLvl } finalFunc
    = applyMigrationsInternal
        (baseApplyMigsBlock superUserConnString
                            retryPolicy
                            (\_migBlocks conn -> finalFunc conn)
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
    (applyMigrationsNoCheck dbInfo (const $ pure ()))
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
