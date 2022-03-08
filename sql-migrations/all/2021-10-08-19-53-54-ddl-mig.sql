-- codd: no-txn
ALTER TABLE transactions ADD COLUMN anonymous BOOLEAN NOT NULL DEFAULT FALSE;