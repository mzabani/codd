#!/usr/bin/env bash

set -eou pipefail

dropdb --if-exists codd-experiments
createdb -T template0 -E UTF8 -l en_US.UTF8 codd-experiments
FIRST_SQL=$(cat <<"EOF"
DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd_admin') THEN
      CREATE USER "codd_admin";
   END IF;
END
$do$;

ALTER DATABASE "codd-experiments" OWNER TO codd_admin;
EOF
)

psql -U postgres -d postgres -c "$FIRST_SQL"