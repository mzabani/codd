module Codd
  ( ApplyResult (..),
    SchemasPair (..),
    CoddSettings (..),
    VerifySchemas (..),
    applyMigrations,
    applyMigrationsNoCheck,
  )
where

import Codd.Environment (CoddSettings (..))
import Codd.Internal
  ( collectAndApplyMigrations,
    laxCheckLastAction,
    strictCheckLastAction,
  )
import Codd.Logging (CoddLogger)
import Codd.Parsing
  ( AddedSqlMigration,
    EnvVars,
    hoistAddedSqlMigration,
  )
import Codd.Query
  ( InTxnT,
    NotInTxn,
  )
import Codd.Representations
  ( DbRep,
  )
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.Time (DiffTime)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO.Resource
  ( ResourceT,
    runResourceT,
  )
import Prelude hiding (readFile)

data VerifySchemas = LaxCheck | StrictCheck
  deriving stock (Show)

data SchemasPair = SchemasPair
  { expectedSchemas :: DbRep,
    databaseSchemas :: DbRep
  }

data ApplyResult = SchemasDiffer SchemasPair | SchemasMatch DbRep | SchemasNotVerified

-- | Collects pending migrations from disk and applies them all, returning
-- the Database's schema if they're not the ones expected or a success result otherwise.
-- Throws an exception if a migration fails or if schemas mismatch and strict-checking is enabled.
applyMigrations ::
  (MonadUnliftIO m, CoddLogger m, MonadThrow m, EnvVars m, NotInTxn m) =>
  CoddSettings ->
  -- | Instead of collecting migrations from disk according to codd settings, use these if they're defined.
  Maybe [AddedSqlMigration m] ->
  DiffTime ->
  VerifySchemas ->
  m ApplyResult
applyMigrations dbInfo mOverrideMigs connectTimeout checkSchemas =
  case checkSchemas of
    StrictCheck -> do
      expectedReps <-
        runResourceT $
          collectAndApplyMigrations
            (strictCheckLastAction dbInfo)
            dbInfo
            (map (hoistAddedSqlMigration lift) <$> mOverrideMigs)
            connectTimeout
      pure $ SchemasMatch expectedReps
    LaxCheck -> do
      (dbReps, expectedReps) <-
        runResourceT $
          collectAndApplyMigrations
            (laxCheckLastAction dbInfo)
            dbInfo
            (map (hoistAddedSqlMigration lift) <$> mOverrideMigs)
            connectTimeout

      if dbReps /= expectedReps
        then
          pure $
            SchemasDiffer $
              SchemasPair
                { expectedSchemas = expectedReps,
                  databaseSchemas = dbReps
                }
        else pure $ SchemasMatch expectedReps

-- | Collects pending migrations from disk and applies them all.
-- Does not verify schemas but allows a function that runs in the same transaction as the last migrations
-- iff all migrations are in-txn or separately after the last migration otherwise.
-- Throws an exception if a migration fails.
applyMigrationsNoCheck ::
  ( MonadUnliftIO m,
    CoddLogger m,
    MonadThrow m,
    EnvVars m,
    NotInTxn m,
    txn ~ InTxnT (ResourceT m)
  ) =>
  CoddSettings ->
  -- | Instead of collecting migrations from disk according to codd settings, use these if they're defined.
  Maybe [AddedSqlMigration m] ->
  DiffTime ->
  (DB.Connection -> txn a) ->
  m a
applyMigrationsNoCheck dbInfo mOverrideMigs connectTimeout finalFunc =
  collectAndApplyMigrations
    (\_migBlocks conn -> finalFunc conn)
    dbInfo
    mOverrideMigs
    connectTimeout
