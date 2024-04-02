-- codd: no-txn
-- This migration is used in our retry policy tests
-- See why so many "SELECT 1" in the other "divide-by-0-migration.sql" file.

-- The CREATE TABLE statements ensure we're retrying no-txn migrations from the right statement, or else they will fail due to
-- tables already existing.

CREATE TABLE any_table();
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 1;
CREATE TABLE other_table();
SELECT 7/0
