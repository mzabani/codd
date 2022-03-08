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
import           Codd.Internal                  ( collectAndApplyMigrations
                                                , laxCheckLastAction
                                                , strictCheckLastAction
                                                )
import           Codd.Parsing                   ( AddedSqlMigration
                                                , hoistAddedSqlMigration
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Control.Monad.Logger           ( MonadLogger )
import           Control.Monad.Trans            ( lift )
import           Data.Time                      ( DiffTime )
import qualified Database.PostgreSQL.Simple    as DB
import           Prelude                 hiding ( readFile )
import           UnliftIO.Resource              ( runResourceT )

data CheckHashes = LaxCheck | StrictCheck
    deriving stock (Show)

data ChecksumsPair = ChecksumsPair
    { expectedChecksums :: DbHashes
    , databaseChecksums :: DbHashes
    }
data ApplyResult = ChecksumsDiffer ChecksumsPair | ChecksumsMatch DbHashes | ChecksumsNotVerified

-- | Collects pending migrations from disk and applies them all, returning
-- the Database's checksums if they're not the ones expected or a success result otherwise.
-- Throws an exception if a migration fails or if checksums mismatch and strict-checking is enabled.
applyMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> Maybe [AddedSqlMigration m]
    -- ^ Instead of collecting migrations from disk according to codd settings, use these if they're defined.
    -> DiffTime
    -> CheckHashes
    -> m ApplyResult
applyMigrations dbInfo@CoddSettings { onDiskHashes } mOverrideMigs connectTimeout checkHashes
    = case checkHashes of
        StrictCheck -> do
            eh <- either readHashesFromDisk pure onDiskHashes
            runResourceT $ collectAndApplyMigrations
                (strictCheckLastAction dbInfo eh)
                dbInfo
                (map (hoistAddedSqlMigration lift) <$> mOverrideMigs)
                connectTimeout
            pure $ ChecksumsMatch eh
        LaxCheck -> do
            eh       <- either readHashesFromDisk pure onDiskHashes
            dbCksums <- runResourceT $ collectAndApplyMigrations
                (laxCheckLastAction dbInfo eh)
                dbInfo
                (map (hoistAddedSqlMigration lift) <$> mOverrideMigs)
                connectTimeout

            if dbCksums /= eh
                then pure $ ChecksumsDiffer $ ChecksumsPair
                    { expectedChecksums = eh
                    , databaseChecksums = dbCksums
                    }
                else pure $ ChecksumsMatch eh

-- | Collects pending migrations from disk and applies them all.
-- Does not verify checksums but allows a function that runs in the same transaction as the last migrations
-- iff all migrations are in-txn or separately after the last migration and not in an explicit transaction
-- otherwise.
-- Throws an exception if a migration fails.
applyMigrationsNoCheck
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> Maybe [AddedSqlMigration m]
    -- ^ Instead of collecting migrations from disk according to codd settings, use these if they're defined.
    -> DiffTime
    -> (DB.Connection -> m a)
    -> m a
applyMigrationsNoCheck dbInfo mOverrideMigs connectTimeout finalFunc =
    runResourceT $ collectAndApplyMigrations
        (\_migBlocks conn -> lift $ finalFunc conn)
        dbInfo
        (map (hoistAddedSqlMigration lift) <$> mOverrideMigs)
        connectTimeout
