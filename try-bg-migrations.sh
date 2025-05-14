#!/usr/bin/env bash

export CODD_RETRY_POLICY="max 0 backoff exponential 1s"
psql -U postgres -c 'select pg_terminate_backend(pid) from pg_stat_activity where pid <> pg_backend_pid()'
dropdb "$PGDATABASE"
codd up
psql -1 -f codd-background-schema.sql
# psql -1 -f replace-one-column-restrict-enum.sql
# psql -1 -f replace-two-bools-with-one-new-column.sql
psql -1 -f emulate-error-in-scheduled-job.sql
