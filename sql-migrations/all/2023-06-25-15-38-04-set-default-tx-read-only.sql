-- codd-env-vars: PGDATABASE

alter database "${PGDATABASE}" set default_transaction_read_only to true;