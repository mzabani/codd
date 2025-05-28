module Codd.InternalSchema.V1 (createInternalSchemaV1) where
import qualified Database.PostgreSQL.Simple as DB
import Codd.Query (execvoid_, InTxn)
import UnliftIO (MonadIO)

createInternalSchemaV1 :: (InTxn m, MonadIO m) => DB.Connection -> m ()
createInternalSchemaV1 conn = do
  execvoid_
      conn
      "CREATE SCHEMA codd_schema; GRANT USAGE ON SCHEMA codd_schema TO PUBLIC;"
  execvoid_ conn
      $ "CREATE TABLE codd_schema.sql_migrations ( \
      \  id SERIAL PRIMARY KEY\
      \, migration_timestamp timestamptz not null\
      \, applied_at timestamptz not null \
      \, name text not null \
      \, unique (name), unique (migration_timestamp));"
      <> -- It is not necessary to grant SELECT on the table, but it helps _a lot_ with a test and shouldn't hurt.
         -- SELECT on the sequence enables dumps by unprivileged users
         "GRANT INSERT,SELECT ON TABLE codd_schema.sql_migrations TO PUBLIC;\
         \GRANT USAGE ,SELECT ON SEQUENCE codd_schema.sql_migrations_id_seq TO PUBLIC;"
