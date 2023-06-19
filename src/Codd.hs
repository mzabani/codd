module Codd
  ( ApplyResult(..)
  , SchemasPair(..)
  , CoddSettings(..)
  , VerifySchemas(..)
  , applyMigrations
  , applyMigrationsNoCheck
  ) where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( collectAndApplyMigrations
                                                , laxCheckLastAction
                                                , strictCheckLastAction
                                                )
import           Codd.Parsing                   ( AddedSqlMigration
                                                , EnvVars
                                                , hoistAddedSqlMigration
                                                )
import           Codd.Representations           ( DbRep
                                                , readRepsFromDisk
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Control.Monad.Logger           ( MonadLogger )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import           Data.Time                      ( DiffTime )
import qualified Database.PostgreSQL.Simple    as DB
import           Prelude                 hiding ( readFile )
import           UnliftIO.Resource              ( runResourceT )

data VerifySchemas = LaxCheck | StrictCheck
    deriving stock (Show)

data SchemasPair = SchemasPair
  { expectedSchemas :: DbRep
  , databaseSchemas :: DbRep
  }
data ApplyResult = SchemasDiffer SchemasPair | SchemasMatch DbRep | SchemasNotVerified

-- | Collects pending migrations from disk and applies them all, returning
-- the Database's schema if they're not the ones expected or a success result otherwise.
-- Throws an exception if a migration fails or if schemas mismatch and strict-checking is enabled.
applyMigrations
  :: (MonadUnliftIO m, MonadIO m, MonadLogger m, MonadThrow m, EnvVars m)
  => CoddSettings
  -> Maybe [AddedSqlMigration m]
    -- ^ Instead of collecting migrations from disk according to codd settings, use these if they're defined.
  -> DiffTime
  -> VerifySchemas
  -> m ApplyResult
applyMigrations dbInfo@CoddSettings { onDiskReps } mOverrideMigs connectTimeout checkSchemas
  = case checkSchemas of
    StrictCheck -> do
      eh <- either readRepsFromDisk pure onDiskReps
      runResourceT $ collectAndApplyMigrations
        (strictCheckLastAction dbInfo eh)
        dbInfo
        (map (hoistAddedSqlMigration lift) <$> mOverrideMigs)
        connectTimeout
      pure $ SchemasMatch eh
    LaxCheck -> do
      eh       <- either readRepsFromDisk pure onDiskReps
      dbCksums <- runResourceT $ collectAndApplyMigrations
        (laxCheckLastAction dbInfo eh)
        dbInfo
        (map (hoistAddedSqlMigration lift) <$> mOverrideMigs)
        connectTimeout

      if dbCksums /= eh
        then pure $ SchemasDiffer $ SchemasPair { expectedSchemas = eh
                                                , databaseSchemas = dbCksums
                                                }
        else pure $ SchemasMatch eh

-- | Collects pending migrations from disk and applies them all.
-- Does not verify schemas but allows a function that runs in the same transaction as the last migrations
-- iff all migrations are in-txn or separately after the last migration and not in an explicit transaction
-- otherwise.
-- Throws an exception if a migration fails.
applyMigrationsNoCheck
  :: (MonadUnliftIO m, MonadIO m, MonadLogger m, MonadThrow m, EnvVars m)
  => CoddSettings
  -> Maybe [AddedSqlMigration m]
    -- ^ Instead of collecting migrations from disk according to codd settings, use these if they're defined.
  -> DiffTime
  -> (  forall t
      . (MonadLogger t, MonadUnliftIO t)
     => DB.Connection
     -> t a
     )
  -> m a
applyMigrationsNoCheck dbInfo mOverrideMigs connectTimeout finalFunc =
  runResourceT $ collectAndApplyMigrations
    (\_migBlocks conn -> finalFunc conn)
    dbInfo
    (map (hoistAddedSqlMigration lift) <$> mOverrideMigs)
    connectTimeout
