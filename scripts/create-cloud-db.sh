#!/usr/bin/env bash

# This is nearly identical to create-dev-db.sh, with the
# difference that it uses $PGPASSWORD to create $PGUSER
# with it. This is necessary because apparently AWS RDS
# doesn't allow passwordless logins.

set -e

psql -U "$PGSUPERUSER" -d postgres <<EOF
DO
\$do$
BEGIN
   IF EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = '$PGUSER') THEN
      SET ROLE "$PGUSER";
      DROP OWNED BY "$PGUSER";
   END IF;
END
\$do$;
EOF

(echo "DROP DATABASE IF EXISTS \"$PGDATABASE\"" | psql -U "$PGSUPERUSER" -d postgres) 2>/dev/null || true
psql -U "$PGSUPERUSER" -d postgres <<EOF
SET ROLE "$PGUSER"; DROP DATABASE IF EXISTS "$PGDATABASE"
EOF

createdb -U "$PGSUPERUSER" -T template0 -E UTF8 -l en_US.UTF8 "$PGDATABASE"

psql -U "$PGSUPERUSER" -d postgres <<EOF
DROP ROLE IF EXISTS "$PGUSER";
CREATE USER "$PGUSER" WITH CREATEROLE PASSWORD '$PGPASSWORD';
ALTER DATABASE "$PGDATABASE" OWNER TO "$PGUSER";
GRANT CONNECT ON DATABASE "$PGDATABASE" TO "$PGUSER";
EOF

# The next SQL runs in the $PGDATABASE database
psql -U "$PGSUPERUSER" -d "$PGDATABASE" <<EOF
ALTER SCHEMA public OWNER TO "$PGUSER";
EOF