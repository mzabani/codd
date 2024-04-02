-- codd: no-txn
-- codd-connection: dbname=postgres host=127.0.0.1 user=postgres port=5434
-- This migration is used in our retry policy tests

-- The CREATE TABLE statements ensure we're rolling back and retrying properly, or else they will fail due to
-- tables already existing.

CREATE TABLE some_table();
DO
$do$
BEGIN
   IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-test-user') THEN
      CREATE USER "codd-test-user";
   END IF;
END
$do$;
CREATE DATABASE "codd-test-db" WITH OWNER="codd-test-user";
GRANT CONNECT ON DATABASE "codd-test-db" TO "codd-test-user";

CREATE TABLE other_table();
-- The statement below will fail, but the default connection string will be accessible after that happens!
SELECT 1/0;
SELECT 42;
