-- codd: in-txn
-- codd-connection: postgres://postgres:crazy_-passwor'd@some-server:5433/codd-experiments_db

-- We just dump a lot of things here to make sure our streaming parser
-- is capable of reading the contents of the file without leaving anything behind.
-- We have this on-disk instead of in-memory because pure streams behave differently,
-- so it's good to have this test.

-- Copy and past from codd's existing migration as of 2022-04-09:

DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN

      CREATE USER "codd-user";
   END IF;
END
$do$;

GRANT CONNECT ON DATABASE "codd-experiments" TO "codd-user";


CREATE TABLE employee (
    employee_id SERIAL PRIMARY KEY
    , employee_name TEXT NOT NULL
);

INSERT INTO employee (employee_name) VALUES ('Marcelo');-- codd: no-txn
ALTER TABLE employee RENAME COLUMN employee_name TO name;-- codd: no-txn

-- README:
-- This is an example of a Migration that renames a column in a Blue-Green-Safe way. Both Old and New Apps
-- need not be concerned of the new/old column names here. We recommend caution and testing when using this.

-- 1. Add the column and set its values, initially
ALTER TABLE employee ADD COLUMN employee_name TEXT; -- TODO: Remember to set a good DEFAULT if you need one.
UPDATE employee SET employee_name=name WHERE name IS DISTINCT FROM employee_name;
ALTER TABLE employee ADD CONSTRAINT employee_rename_equal_ck CHECK (employee_name IS NOT DISTINCT FROM name);

-- 2. Both old and new Apps need to update the two columns consistently until one of them is removed.
CREATE FUNCTION employee_name_rename_set_new() RETURNS TRIGGER AS $$
    BEGIN
        NEW.employee_name = NEW.name;
        RETURN NEW;
    END
$$ LANGUAGE plpgsql;
CREATE FUNCTION employee_name_rename_set_old() RETURNS TRIGGER AS $$
    BEGIN
        NEW.name = NEW.employee_name;
        RETURN NEW;
    END
$$ LANGUAGE plpgsql;

-- 3. Triggers to set the new column name when the old App does something
CREATE TRIGGER employee_old_app_update_column_name
    BEFORE UPDATE ON employee
    FOR EACH ROW
    WHEN (OLD.name IS DISTINCT FROM NEW.name)
    EXECUTE FUNCTION employee_name_rename_set_new();
CREATE TRIGGER employee_old_app_insert_column_name
    BEFORE INSERT ON employee
    FOR EACH ROW
    WHEN (NEW.employee_name IS NULL)
    EXECUTE FUNCTION employee_name_rename_set_new();

-- 4. Triggers to set the old column name when the new App does something
CREATE TRIGGER employee_new_app_update_column_name
    BEFORE UPDATE ON employee
    FOR EACH ROW
    WHEN (OLD.employee_name IS DISTINCT FROM NEW.employee_name)
    EXECUTE FUNCTION employee_name_rename_set_old();
CREATE TRIGGER employee_new_app_insert_column_name
    BEFORE INSERT ON employee
    FOR EACH ROW
    WHEN (NEW.name IS NULL)
    EXECUTE FUNCTION employee_name_rename_set_old();

-- 5. You might want to create indices to speed up your queries here

-- codd: destructive
DROP TRIGGER employee_old_app_update_column_name ON employee;
DROP TRIGGER employee_old_app_insert_column_name ON employee;
DROP TRIGGER employee_new_app_update_column_name ON employee;
DROP TRIGGER employee_new_app_insert_column_name ON employee;
DROP FUNCTION employee_name_rename_set_new();
DROP FUNCTION employee_name_rename_set_old();

-- We drop the new and rename because in the non-destructive section we didn't add constraints that might exist
-- to the new column, but we still want them.
ALTER TABLE employee DROP COLUMN employee_name;
ALTER TABLE employee RENAME COLUMN name TO employee_name;CREATE TABLE bonus_pay (
    employee_id INT NOT NULL REFERENCES employee(employee_id)
    , value NUMERIC(10,2) CHECK (value > 0)
    , value_copy NUMERIC(10,2) CHECK (value_copy > 0)
);

-- Just something destructive, which should not matter for simple deployment workflow
ALTER TABLE bonus_pay DROP COLUMN value_copy;DROP TABLE bonus_pay;-- codd: no-txn
CREATE TABLE some_table ();DROP TABLE some_table;-- codd:no-txn
CREATE TABLE other_table();DROP TABLE other_table;-- codd: no-txn

-- This changes the DB default just to ensure that read-only as a default
-- works with codd
BEGIN TRANSACTION READ WRITE;

ALTER DATABASE "codd-experiments" SET default_transaction_isolation TO 'serializable';
ALTER DATABASE "codd-experiments" SET default_transaction_deferrable TO TRUE;
ALTER DATABASE "codd-experiments" SET default_transaction_read_only TO TRUE;

SET default_transaction_isolation TO 'serializable';
SET default_transaction_deferrable TO TRUE;
SET default_transaction_read_only TO TRUE;

COMMIT;-- codd: no-txn

BEGIN TRANSACTION READ WRITE;
CREATE TYPE experience AS ENUM ('junior', 'senior');
ALTER TABLE employee ADD COLUMN experience experience;
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
COMMIT;

BEGIN TRANSACTION READ WRITE;
UPDATE employee SET experience='intern';
COMMIT;-- codd: no-txn

-- This reverts the default to read-write.
BEGIN TRANSACTION READ WRITE;

ALTER DATABASE "codd-experiments" SET default_transaction_read_only TO FALSE;

SET default_transaction_read_only TO FALSE;

COMMIT;CREATE TABLE ahaha();-- codd: no-txn
drop table ahaha;-- codd: no-txn
ALTER TYPE experience ADD VALUE 'master' AFTER 'senior';
UPDATE employee SET experience='master';-- CREATE VIEW vista AS SELECT 'Hello World';
CREATE OR REPLACE VIEW all_employee_names (employee_name) AS (SELECT employee_name FROM employee);CREATE UNIQUE INDEX any_unique_employee_idx_will_do ON employee (employee_name);-- COPY employee FROM STDIN WITH (FORMAT CSV);
-- "5","Dracula","master"
-- "6","The Grinch","master"
-- \.
SELECT 'just for fun';

COPY employee FROM STDIN WITH (FORMAT CSV);
5,Dracula,master
6,The Grinch,master
\.

COPY public.employee FROM STDIN WITH (format csv);
7,Frankenstein,junior
8,Medusa,senior
\.

COPY public . employee FROM STDIN WITH (FORMAT CSV);
9,Werewolf,intern
\.

-- One empty COPY
COPY "public"."employee" FROM STDIN WITH (FORMAT CSV);
\.

SELECT setval('employee_employee_id_seq', (SELECT MAX(employee_id) FROM employee));CREATE TABLE no_cols_table (x INT);
INSERT INTO no_cols_table (x) VALUES (1), (2), (3), (4), (5);
ALTER TABLE no_cols_table DROP COLUMN x;-- Repopulate the table without columns with COPY just to test an edge case!!
DELETE FROM no_cols_table;
COPY no_cols_table FROM STDIN WITH (FORMAT CSV);





\.
DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd_low_privilege_user') THEN

      CREATE USER "codd_low_privilege_user";
   END IF;
END
$do$;

GRANT CONNECT ON DATABASE "codd-experiments" TO "codd_low_privilege_user";

REVOKE ALL ON TABLE employee FROM codd_low_privilege_user;ALTER DATABASE "codd-experiments" SET default_transaction_isolation TO 'serializable';
SET default_transaction_isolation TO 'serializable';GRANT ALL ON TABLE employee TO "codd-user";
GRANT ALL ON TABLE no_cols_table TO "codd-user";CREATE TABLE transactions (id BIGINT GENERATED BY DEFAULT AS IDENTITY, payer TEXT NOT NULL, receiver TEXT NOT NULL, value NUMERIC(10,2) CHECK (value > 0));SELECT 1;
SELECT 2;-- codd: no-txn
ALTER TABLE transactions ADD COLUMN anonymous BOOLEAN NOT NULL DEFAULT FALSE;CREATE SCHEMA collations_1;
CREATE SCHEMA collations_2;
CREATE COLLATION collations_1.pt_br_test_coll (provider = icu, locale = 'pt-u-co-phonebk');
CREATE COLLATION collations_2.pt_br_test_coll (provider = libc, locale = 'pt_BR.utf8');

ALTER TABLE employee ADD COLUMN surname TEXT COLLATE collations_1.pt_br_test_coll;CREATE DOMAIN non_empty_text TEXT NOT NULL CHECK (VALUE != '');
CREATE DOMAIN non_whitespace_text TEXT NOT NULL CHECK (TRIM(VALUE) != '');

ALTER DOMAIN non_empty_text ADD CONSTRAINT new_constraint CHECK(VALUE != 'empty') NOT VALID;
GRANT USAGE ON DOMAIN non_empty_text TO codd_low_privilege_user;

CREATE TYPE complex AS (
    a       double precision,
    b       double precision
);

CREATE TYPE floatrange AS RANGE (
    subtype = float8,
    subtype_diff = float8mi
);
-- Range constructor functions are created as owned by the database admin,
-- which is not what we want. However, users may not want to run migrations
-- as the SQL admin, and that's the scenario we want to emulate here
-- for development purposes, so that we can discover these restrictions.
-- ALTER ROUTINE floatrange(float8, float8) OWNER TO codd_admin;
-- ALTER ROUTINE floatrange(float8, float8, text) OWNER TO codd_admin;

SELECT '[1.234, 5.678]'::floatrange;

CREATE FUNCTION time_subtype_diff(x time, y time) RETURNS float8 AS
'SELECT EXTRACT(EPOCH FROM (x - y))' LANGUAGE sql STRICT IMMUTABLE;

CREATE TYPE timerange AS RANGE (
    subtype = time,
    subtype_diff = time_subtype_diff
);
-- ALTER ROUTINE timerange(time, time) OWNER TO codd_admin;
-- ALTER ROUTINE timerange(time, time, text) OWNER TO codd_admin;

SELECT '[11:10, 23:00]'::timerange;
-- codd-connection: postgres://postgres@localhost:5433/postgres

-- This tests that inserting to the codd_schema works.

SELECT 1;-- codd-connection: postgres://codd_admin@[::1]:5433/codd-experiments

-- Test an IPv6 connection string

SELECT 1;CREATE TABLE animals (id SERIAL PRIMARY KEY, popular_name TEXT NOT NULL);
INSERT INTO animals (popular_name) VALUES ('Dog'), ('Cat');
CREATE TABLE people();SELECT 3+25;-- codd: no-parse
SELECT 1;
CREATE TABLE table_created_with_no_parse_mig();
SELECT 2;