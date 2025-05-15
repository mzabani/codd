#!/usr/bin/env bash

export CODD_RETRY_POLICY="max 0 backoff exponential 1s"
psql -U postgres -c 'select pg_terminate_backend(pid) from pg_stat_activity where pid <> pg_backend_pid()'
dropdb "$PGDATABASE"
codd up
psql -1 -f codd-background-schema.sql

################################
# Now choose a test
# #############################


########### Replacing a column with a new one
# psql -1 -f replace-one-column-restrict-enum.sql
# psql -1 -c "SELECT codd_schema.synchronously_finish_background_job('change-experience', 100)"

########## A job that errors after a few successful runs
psql -1 -f emulate-error-in-scheduled-job.sql
psql -1 -c "SELECT codd_schema.synchronously_finish_background_job('change-experience', 100)"

########## Replacing two columns with a new one
# psql -1 -f replace-two-bools-with-one-new-column.sql
