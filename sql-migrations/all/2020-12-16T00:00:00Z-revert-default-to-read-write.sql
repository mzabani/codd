-- codd: no-txn

-- This reverts the default to read-write.
BEGIN TRANSACTION READ WRITE;

ALTER DATABASE "codd-experiments" SET default_transaction_read_only TO FALSE;

SET default_transaction_read_only TO FALSE;

COMMIT;