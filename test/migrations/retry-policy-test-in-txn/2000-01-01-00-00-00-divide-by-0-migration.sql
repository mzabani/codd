-- codd: in-txn
-- This migration is used in our retry policy tests.
-- This is a bit sad, but the behaviour of a Stream depends
-- on its sources, and we do build our Streams combining in-memory
-- lists mappended to file/disk streams. When consuming these streams
-- more than once, pure streams will naturally not store an "offset"
-- while IO-based streams will. This leads to very confusing behavior.
-- That means, if "SELECT 7/0" is in the beginning of the file, that
-- the retry test _will_ pass even with a bad implementation.
-- So we currently rely on an implementation detail that codd will never
-- have more than the first few SQL statements (should only be the
-- statements in the first line, really) in memory.

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
SELECT 1;
SELECT 1;
SELECT 1;
SELECT 7/0