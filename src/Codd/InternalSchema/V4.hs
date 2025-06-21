module Codd.InternalSchema.V4 (migrateInternalSchemaV3ToV4) where

import Codd.Query (InTxn, execvoid_)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO (MonadIO)

migrateInternalSchemaV3ToV4 :: (InTxn m, MonadIO m) => DB.Connection -> m ()
migrateInternalSchemaV3ToV4 conn =
  execvoid_
    conn
    "ALTER TABLE codd_schema.sql_migrations ADD COLUMN txnid BIGINT DEFAULT txid_current(), ADD COLUMN connid INT DEFAULT pg_backend_pid()"
