-- codd: no-txn
SELECT 4;

-- Some comment
COPY somedata FROM STDIN WITH (FORMAT csv);
4
5
6
\.

-- Another comment

SELECT 7;
BEGIN;
SELECT 3;
CREATE TABLE othertablenotexists();
-- Yet another one
COPY somedata FROM STDIN WITH (FORMAT csv);
1
2
3
\.

-- Last one

COMMIT;
