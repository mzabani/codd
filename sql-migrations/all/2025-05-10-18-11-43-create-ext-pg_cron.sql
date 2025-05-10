-- codd-connection: dbname=codd-experiments host=127.0.0.1 user=postgres
CREATE EXTENSION pg_cron;

GRANT USAGE ON SCHEMA cron TO codd_admin;
