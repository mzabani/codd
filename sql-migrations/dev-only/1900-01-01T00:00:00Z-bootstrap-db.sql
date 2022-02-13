-- codd: no-txn
-- codd-connection: postgres://postgres@localhost:5433/postgres

DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd_admin') THEN
      CREATE USER codd_admin WITH CREATEROLE;
   END IF;
END
$do$;

CREATE DATABASE "codd-experiments" WITH OWNER=codd_admin;