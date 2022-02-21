module Codd
    ( ApplyResult(..)
    , ChecksumsPair(..)
    , CoddSettings(..)
    , CheckHashes(..)
    , applyMigrations
    , applyMigrationsNoCheck
    ) where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( DbHashes
                                                , readHashesFromDisk
                                                )
import           Codd.Internal                  ( applyMigrationsInternal
                                                , baseApplyMigsBlock
                                                , laxCheckLastAction
                                                , strictCheckLastAction
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Control.Monad.Logger           ( MonadLogger )
import qualified Database.PostgreSQL.Simple    as DB
import           Prelude                 hiding ( readFile )

data CheckHashes = LaxCheck | StrictCheck
data ChecksumsPair = ChecksumsPair
    { expectedChecksums :: DbHashes
    , databaseChecksums :: DbHashes
    }
data ApplyResult = ChecksumsDiffer ChecksumsPair | ChecksumsMatch DbHashes | ChecksumsNotVerified

-- | Creates the new Database if it doesn't yet exist and applies every single migration, returning
-- the Database's checksums if they're not the ones expected or a success result otherwise.
-- Throws an exception if a migration fails or if checksums mismatch and strict-checking is enabled.
applyMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> CheckHashes
    -> m ApplyResult
applyMigrations dbInfo@CoddSettings { migsConnString, onDiskHashes, retryPolicy, txnIsolationLvl } checkHashes
    = case checkHashes of
        StrictCheck -> do
            eh <- either readHashesFromDisk pure onDiskHashes
            applyMigrationsInternal
                (baseApplyMigsBlock migsConnString
                                    retryPolicy
                                    (strictCheckLastAction dbInfo eh)
                                    txnIsolationLvl
                )
                dbInfo
            pure $ ChecksumsMatch eh
        LaxCheck -> do
            eh       <- either readHashesFromDisk pure onDiskHashes
            dbCksums <- applyMigrationsInternal
                (baseApplyMigsBlock migsConnString
                                    retryPolicy
                                    (laxCheckLastAction dbInfo eh)
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
applyMigrationsNoCheck dbInfo@CoddSettings { migsConnString, retryPolicy, txnIsolationLvl } finalFunc
    = applyMigrationsInternal
        (baseApplyMigsBlock migsConnString
                            retryPolicy
                            (\_migBlocks conn -> finalFunc conn)
                            txnIsolationLvl
        )
        dbInfo

