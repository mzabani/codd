# SQL Migrations and Codd

Most of the time, you'll be able to simply add migrations and things should work well. However, there are cases where things get tricky, so this guide should help if you're in such a situation.  

<!-- vscode-markdown-toc -->
- [SQL Migrations and Codd](#sql-migrations-and-codd)
	- [Configurability](#configurability)
		- [Migrations that can't run in transactions](#migrations-that-cant-run-in-transactions)
		- [Migrations that need to run in a custom database connection](#migrations-that-need-to-run-in-a-custom-database-connection)
		- [Templating environment variables into migrations](#templating-environment-variables-into-migrations)
	- [Special care with no-txn or custom-connection migrations](#special-care-with-no-txn-or-custom-connection-migrations)
	- [Unsupported SQL inside migrations](#unsupported-sql-inside-migrations)
	- [Retry Policies](#retry-policies)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## Configurability

_Codd_ handles plain SQL migrations, but allows some level of configurability per
migration by parsing comments at the top of migrations, i.e. comments with special
syntax before any statements.
There are currently three different special comments that are meaningful to _codd_,
which we describe in the next three subsections.

### Migrations that can't run in transactions

Just add `-- codd: no-txn` before any statements to your SQL file and _codd_ will run
the statements in your migration outside a transaction.

Example:

````sql
-- codd: no-txn
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
UPDATE employee SET employee_experience='intern';
````

### Migrations that need to run in a custom database connection

Just add `-- codd-connection: postgres://...` (or in keyword value pairs format) before
any statements to your SQL file and _codd_ will use that connection string to apply that
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

Please be aware that _codd_'s environment variable templating is extremely primitive. Do not
put entire statements inside your environment variables. Also, if one variable contains
`${OTHERVAR}` inside, then there is a chance it can be doubly replaced depending on the internal
order _codd_ applies its replacements. This order is undefined, so don't rely on that behaviour.
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

By using `no-txn` migrations or migrations with a custom connection string, you're taking great risk with the possibility of a migration failing when deploying and leaving the database in an intermediary state that is not compatible with the previously deployed application nor the to-be-deployed one. It is recommended that you avoid these at great costs and plan carefully when adding even one of them.  

_Codd_ will always run each block of consecutive `in-txn` migrations with the same connection string in a single transaction. If there are `in-txn` migrations intertwined with `no-txn` migrations or migrations with custom connection strings, every block of consecutive `in-txn` and `same-connection-string` migrations runs in the same transaction, but other migrations run separately. Also, if even one `no-txn` migration or one migration with a custom connection string exists, _codd_ will apply and commit every pending migration and verify checksums only after that.  

## Unsupported SQL inside migrations

_Codd_ does not support all SQL statements inside a migration. This is an incomplete list of things that we know _codd_ does not support.  

1. `COPY FROM STDIN` is supported but other forms of `COPY` are not.  
2. psql's meta commands, including `\COPY`, are not supported.  

If you find a problem, please let us know. 

## Retry Policies

A migration can fail for a variety of reasons, including unhandled data and serializability errors when using Serializable transactions. For this reason, _codd_ comes with a default Retry Policy of 3 tries (at most 2 retries), the first retry attempt 1 second after the first fails, and the second retry attempt 2 seconds after the second one fails. This can be configured with the `CODD_RETRY_POLICY` environment variable as in [CONFIGURATION.md](CONFIGURATION.md). Important observations are:

- When faced with a block of consecutive `in-txn`  migrations, _codd_ retries the blocks whole.
  - For these, the retry count and intervals are "reset" for each block.
- For `no-txn` migrations, _codd_ retries individual statements, not even entire migrations.
  - Otherwise retries would lead to possibly inconsistent data.
  - The retry count and intervals are also "reset" for each statement.
