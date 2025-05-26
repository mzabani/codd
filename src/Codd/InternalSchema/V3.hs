module Codd.InternalSchema.V3 (migrateInternalSchemaV2ToV3) where

import Codd.Query (InTxn, execvoid_)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO (MonadIO)

migrateInternalSchemaV2ToV3 :: (InTxn m, MonadIO m) => DB.Connection -> m ()
migrateInternalSchemaV2ToV3 conn =
  execvoid_
    conn
    "ALTER TABLE codd_schema.sql_migrations ADD COLUMN num_applied_statements INT, ADD COLUMN no_txn_failed_at timestamptz, ALTER COLUMN applied_at DROP NOT NULL, ADD CONSTRAINT no_txn_mig_applied_or_failed CHECK ((applied_at IS NULL) <> (no_txn_failed_at IS NULL)); \n\
    \ -- Grant UPDATE so in-txn migrations running under different users can register themselves atomically \n\
    \GRANT UPDATE ON TABLE codd_schema.sql_migrations TO PUBLIC;"
