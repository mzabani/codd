-- codd: no-txn
-- codd-env-vars: PGSUPERUSER ,PGHOST,PGUSER, PGPORT , PGDATABASE
-- codd-connection: postgres://${PGSUPERUSER}@${PGHOST}:${PGPORT}/postgres

DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = '${PGUSER}') THEN
      CREATE USER ${PGUSER} WITH CREATEROLE;
   END IF;
END
$do$;

CREATE DATABASE "${PGDATABASE}" TEMPLATE template0 OWNER ${PGUSER} ENCODING UTF8 LC_COLLATE "en_GB.UTF8" LC_CTYPE "en_GB.UTF8";