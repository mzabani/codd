#!/usr/bin/env bash

export CODD_RETRY_POLICY="max 0 backoff exponential 1s"
psql -U postgres -c 'select pg_terminate_backend(pid) from pg_stat_activity where pid <> pg_backend_pid()'
dropdb "$PGDATABASE"
codd up || true # postgres is not superuser error
codd up # second time works
psql -1 -f codd-background-schema.sql
psql -1 -f 1st-migration.sql
