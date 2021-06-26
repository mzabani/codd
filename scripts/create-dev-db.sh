#!/usr/bin/env bash

set -eou pipefail

dropdb -U postgres --if-exists codd-experiments
createdb -U postgres -T template0 -E UTF8 -l en_US.UTF8 codd-experiments

FIRST_SQL=$(cat <<"EOF"
DO
$do$
BEGIN
   IF EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd_admin') THEN
      DROP OWNED BY codd_admin;
      DROP ROLE codd_admin;
   END IF;
END
$do$;
CREATE USER codd_admin WITH CREATEROLE;
ALTER DATABASE "codd-experiments" OWNER TO codd_admin;
EOF
)
psql -U postgres -d postgres -c "$FIRST_SQL"

SECOND_SQL=$(cat <<"EOF"
ALTER SCHEMA public OWNER TO codd_admin;
EOF
)
psql -U postgres -d codd-experiments -c "$SECOND_SQL"