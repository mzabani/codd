# SQL Migrations and Codd

Most of the time, you'll be able to simply add migrations and things should work well. However, there are cases where things get tricky, so this guide should help if you're in such a situation.  

<!--toc:start-->
- [SQL Migrations and Codd](#sql-migrations-and-codd)
  - [Configurability](#configurability)
    - [Migrations that can't run in transactions](#migrations-that-cant-run-in-transactions)
    - [Migrations that need to run in a custom database connection](#migrations-that-need-to-run-in-a-custom-database-connection)
    - [Templating environment variables into migrations](#templating-environment-variables-into-migrations)
  - [Special care with no-txn or custom-connection migrations](#special-care-with-no-txn-or-custom-connection-migrations)
  - [Unsupported SQL inside migrations](#unsupported-sql-inside-migrations)
  - [Retry Policies](#retry-policies)
    - [Examples:](#examples)
      - [Set of consecutive in-txn migrations](#set-of-consecutive-in-txn-migrations)
      - [No-txn migration failing in the middle](#no-txn-migration-failing-in-the-middle)
      - [No-txn migration with statement failing inside an explicit BEGIN..COMMIT section of the migration](#no-txn-migration-with-statement-failing-inside-an-explicit-begincommit-section-of-the-migration)
<!--toc:end-->

## Configurability

Codd handles plain SQL migrations, but allows some level of configurability per
migration by parsing comments at the top of migrations, i.e. comments with special
syntax before any statements.
There are currently three different special comments that are meaningful to codd,
which we describe in the next three subsections.

### Migrations that can't run in transactions

Just add `-- codd: no-txn` before any statements to your SQL file and codd will run
the statements in your migration outside a transaction.

Example:

````sql
-- codd: no-txn
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
UPDATE employee SET employee_experience='intern';
````

Avoid these migrations as much as possible because they are dangerous. Read
[Special care with no-txn or custom-connection migrations](#special-care-with-no-txn-or-custom-connection-migrations)
for more information why.

### Migrations that need to run in a custom database connection

Just add `-- codd-connection: postgres://...` (or in keyword value pairs format) before
any statements to your SQL file and codd will use that connection string to apply that
migration. The format is the same as the one used for the `CODD_CONNECTION` environment
variable.

This is useful when writing migrations that [bootstrap](BOOTSTRAPPING.md) your database,
because the database to be created doesn't yet exist, so the default connection string
can't work.

Example:

````sql
-- codd: no-txn
-- codd-connection: postgres://dbsuperuser@localhost:5432/postgres
CREATE DATABASE app_database;
````

### Templating environment variables into migrations

Also usually for bootstrapping or in more complex development workflows, the name of the
database to be created is in an environment variable. Or perhaps other things are in
environment variables. In any case you might need access to them from inside your migrations.

To do that, you can write a `-- codd-env-vars: VAR1, VAR2, ...` comment before your statements
and interpolate their values by referencing them through `${VAR1}`, `${VAR2}` etc.

You can reference environment variables from anywhere in the migration, including other
`-- codd` directives in the header.

Please be aware that codd's environment variable templating is extremely primitive. Do not
put entire statements inside your environment variables. Also, if one variable contains
`${OTHERVAR}` inside, then there is a chance it can be doubly replaced depending on the internal
order codd applies its replacements. This order is undefined, so don't rely on that behaviour.
No escaping is performed, that is on you as well.

Example:

````sql
-- codd: no-txn
-- codd-env-vars: PGSUPERUSER, PGHOST, PGUSER, PGPORT, PGDATABASE
-- codd-connection: postgres://${PGSUPERUSER}@${PGHOST}:${PGPORT}/postgres

CREATE USER ${PGUSER} WITH CREATEROLE;
CREATE DATABASE "${PGDATABASE}" TEMPLATE template0 OWNER ${PGUSER} ENCODING UTF8 LC_COLLATE "en_GB.UTF8" LC_CTYPE "en_GB.UTF8";
````

## Special care with no-txn or custom-connection migrations

By using `no-txn` migrations or migrations with a custom connection string, you're taking great risk with the possibility of a migration failing when deploying and leaving the database in an intermediary state that is not compatible with the previously deployed application nor the to-be-deployed one. Codd can resume application from the precise statement that last failed (see [Retry Policies](#retry-policies) for an example) but it is recommended that you avoid these at great costs and plan carefully when adding even one of them.  

Codd will always run each block of consecutive `in-txn` migrations with the same connection string in a single transaction. If there are `in-txn` migrations intertwined with `no-txn` migrations or migrations with custom connection strings, every block of consecutive `in-txn` and `same-connection-string` migrations runs in the same transaction, but other migrations run separately. Also, if even one `no-txn` migration or one migration with a custom connection string exists, codd will apply and commit every pending migration and verify schemas only after that.  

## Unsupported SQL inside migrations

Codd does not support all SQL statements inside a migration. This is an incomplete list of things that we know codd does not support.  

1. `COPY FROM STDIN` is supported but other forms of `COPY` are not.  
2. psql's meta commands, including `\COPY`, are not supported.  

If you find a problem, please let us know. 

## Retry Policies

A migration can fail for a variety of reasons, including unhandled data and serializability errors when using Serializable transactions. For this reason, codd comes with a default Retry Policy of 3 tries (at most 2 retries), the first retry attempt 1 second after the first fails, and the second retry attempt 2 seconds after the second one fails. This can be configured with the `CODD_RETRY_POLICY` environment variable as in [CONFIGURATION.md](CONFIGURATION.md). Important observations are:

- When faced with a set of consecutive `in-txn`  migrations, codd retries all the pending migrations in the block.
  - For these, the retry count and intervals are "reset" for each block that is retried.
- For `no-txn` migrations, codd retries individual statements, not even entire migrations.
  - Otherwise retries would lead to possibly inconsistent data.
  - The retry count and intervals are also "reset" for each statement.
	- Codd stores how many statements it applied for a no-txn migration and is able to resume application from the exact statement that failed in the no-txn migration.

### Examples:

#### Set of consecutive in-txn migrations

This shows how codd will retry all consecutive in-txn migrations atomically.

````
Checking if database codd-test-db is accessible with the configured connection string... (waiting up to 5sec)
Looking for pending migrations... [2 found]
BEGINning transaction
Applying 2000-01-01-00-00-00-create-table-with-unique-id.sql (0.94ms)
Applying 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
ROLLBACKed transaction
Warn: Waiting 1ms before next try
BEGINning transaction
Applying 2000-01-01-00-00-00-create-table-with-unique-id.sql (0.56ms)
Applying 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
ROLLBACKed transaction
Warn: Waiting 2ms before next try
BEGINning transaction
Applying 2000-01-01-00-00-00-create-table-with-unique-id.sql (0.60ms)
Applying 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
ROLLBACKed transaction
Error: Failed after all configured retries. Giving up.
````

#### No-txn migration failing in the middle

This is how codd will retry if a migration has a `COPY` statement in a no-txn migration that fails after another statement in that same migration:

````
Checking if database codd-test-db is accessible with the configured connection string... (waiting up to 5sec)
Looking for pending migrations... [2 found]
Applying 2000-01-01-00-00-00-create-table-with-unique-id.sql (0.95ms)
Applying 2001-01-01-00-00-00-insert-duplicate-not-in-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
Error: After applying 1 statements from no-txn migration 2001-01-01-00-00-00-insert-duplicate-not-in-explicit-transaction.sql, the 2nd failed to be applied. Codd will resume the next retry or codd up from it
Warn: Waiting 1ms before next try
Warn: Skipping the first 1 SQL statements, which have already been applied, and starting application from the 2nd statement
Applying 2001-01-01-00-00-00-insert-duplicate-not-in-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
Error: After applying 1 statements from no-txn migration 2001-01-01-00-00-00-insert-duplicate-not-in-explicit-transaction.sql, the 2nd failed to be applied. Codd will resume the next retry or codd up from it
Warn: Waiting 2ms before next try
Warn: Skipping the first 1 SQL statements, which have already been applied, and starting application from the 2nd statement
Applying 2001-01-01-00-00-00-insert-duplicate-not-in-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
Error: After applying 1 statements from no-txn migration 2001-01-01-00-00-00-insert-duplicate-not-in-explicit-transaction.sql, the 2nd failed to be applied. Codd will resume the next retry or codd up from it
Error: Failed after all configured retries. Giving up.
````

#### No-txn migration with statement failing inside an explicit BEGIN..COMMIT section of the migration

You are free to add your explicit `BEGIN..COMMIT` sections inside your no-txn migrations to minimize the number of possible intermediary states should application fail, and codd is smart enough to retry them appropriately:

````
Checking if database codd-test-db is accessible with the configured connection string... (waiting up to 5sec)
Looking for pending migrations... [2 found]
Applying 2000-01-01-00-00-00-create-table-with-unique-id.sql (0.87ms)
Applying 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
Error: After applying 2 statements from no-txn migration 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql, the 3rd failed to be applied. Since this failed statement is inside an explicitly started transaction in the migration, codd will resume the next retry or codd up from the last BEGIN-like statement, which is the 2nd statement in this migration
ROLLBACKed last explicitly started transaction
Warn: Waiting 1ms before next try
Warn: Skipping the first 1 SQL statements, which have already been applied, and starting application from the 2nd statement
Applying 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
Error: After applying 2 statements from no-txn migration 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql, the 3rd failed to be applied. Since this failed statement is inside an explicitly started transaction in the migration, codd will resume the next retry or codd up from the last BEGIN-like statement, which is the 2nd statement in this migration
ROLLBACKed last explicitly started transaction
Warn: Waiting 2ms before next try
Warn: Skipping the first 1 SQL statements, which have already been applied, and starting application from the 2nd statement
Applying 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql [failed]
Error: SqlStatementException {sqlStatement = "", psimpleError = SqlError {sqlState = "", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"somedata_id_key\"\nDETAIL:  Key (id)=(1) already exists.\nCONTEXT:  COPY somedata, line 1\n)", sqlErrorDetail = "", sqlErrorHint = ""}}
Error: After applying 2 statements from no-txn migration 2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql, the 3rd failed to be applied. Since this failed statement is inside an explicitly started transaction in the migration, codd will resume the next retry or codd up from the last BEGIN-like statement, which is the 2nd statement in this migration
ROLLBACKed last explicitly started transaction
Error: Failed after all configured retries. Giving up.
````
