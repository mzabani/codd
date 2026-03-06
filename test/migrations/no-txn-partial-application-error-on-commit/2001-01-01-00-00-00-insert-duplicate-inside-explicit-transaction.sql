-- codd: no-txn
SELECT 4;

-- Some comment
COPY somedata FROM STDIN WITH (FORMAT csv);
4
5
6
\.

\restrict TAXaYefQ7OaPsbhTIwM0eA6r8S102Jqiy0mRQfQXQQmIdA9fqI7q4LFmKpchNqQ
-- Another comment

SELECT 7;
BEGIN;
SELECT 3;
CREATE TABLE othertablenotexists();
\unrestrict TAXaYefQ7OaPsbhTIwM0eA6r8S102Jqiy0mRQfQXQQmIdA9fqI7q4LFmKpchNqQ
-- Yet another one
COPY somedata FROM STDIN WITH (FORMAT csv);
1
2
3
\.

-- Last one

COMMIT;
