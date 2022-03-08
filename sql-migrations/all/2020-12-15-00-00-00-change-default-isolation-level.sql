-- codd: no-txn

-- This changes the DB default just to ensure that read-only as a default
-- works with codd
BEGIN TRANSACTION READ WRITE;

ALTER DATABASE "codd-experiments" SET default_transaction_isolation TO 'serializable';
ALTER DATABASE "codd-experiments" SET default_transaction_deferrable TO TRUE;
ALTER DATABASE "codd-experiments" SET default_transaction_read_only TO TRUE;

SET default_transaction_isolation TO 'serializable';
SET default_transaction_deferrable TO TRUE;
SET default_transaction_read_only TO TRUE;

COMMIT;