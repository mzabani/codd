module Codd.InternalSchema.V2 (migrateInternalSchemaV1ToV2) where

import Codd.Query (InTxn, execvoid_)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO (MonadIO)

migrateInternalSchemaV1ToV2 :: (InTxn m, MonadIO m) => DB.Connection -> m ()
migrateInternalSchemaV1ToV2 conn = do
  execvoid_
    conn
    "ALTER TABLE codd_schema.sql_migrations ADD COLUMN application_duration INTERVAL"
