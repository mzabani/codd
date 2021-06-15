DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'dbmasteruser') THEN
      CREATE USER dbmasteruser NOSUPERUSER INHERIT CREATEROLE;
   END IF;
END
$do$;

GRANT CONNECT ON DATABASE "codd-experiments" TO dbmasteruser;