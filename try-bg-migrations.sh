#!/usr/bin/env bash

export CODD_RETRY_POLICY="max 0 backoff exponential 1s"
psql -U postgres -c 'select pg_terminate_backend(pid) from pg_stat_activity where pid <> pg_backend_pid()'
dropdb "$PGDATABASE"
codd up
psql -1 -v ON_ERROR_STOP=on -f codd-background-schema.sql

################################
# Now choose a test
# #############################


########### Replacing a column with a new one
# psql -1 -v ON_ERROR_STOP=on -f replace-one-column-restrict-enum.sql
# psql -1 -v ON_ERROR_STOP=on -c "SELECT codd_schema.synchronously_finalize_background_job('change-experience', '100 seconds')"

########## A job that errors after a few successful runs
psql -1 -v ON_ERROR_STOP=on -f emulate-error-in-scheduled-job.sql
psql -1 -v ON_ERROR_STOP=on -c "SELECT codd_schema.synchronously_finalize_background_job('change-experience', '100 seconds')"

########## Creating an index concurrently
# psql -1 -v ON_ERROR_STOP=on -f create-index-concurrently.sql

########## Replacing two columns with a new one
# psql -1 -v ON_ERROR_STOP=on -f replace-two-bools-with-one-new-column.sql
