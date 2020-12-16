# What is Codd?

Codd is a tool to help teams of Developers version-control their Databases locally and for Deployment. It provides a few main features:

- A way to apply pending SQL migrations in a single DB Transaction when possible.  
- A one-file-per-database-object approach to checksum your Database, helping minimize chances that your Production Database differs from your Development Database and ensuring that Merge Conflicts only arise when two developers alter the same Database object.  
- **It is only compatible with PostgreSQL >= 10. No other databases are currently supported.**

## Installing Codd

Currently we provide Codd through Docker. Run codd for the first time with `docker run --rm mzabani/codd codd --help`.

## Configuring Codd

Codd will hash DB Objects to ensure Database-equality between different environments such as Development and Production. But we have to first set it up to let it know which top-level objects - such as schemas and roles - it will consider, and connection strings for it to connect.

Let's take a look at an example `.env` file for Codd. These environment variables must be defined when running the `codd` executable.

````.env
# A connection string in the format postgres://username[:password]@host:port/database_name
# This connection string must be for an ADMIN user, the database must exist and the user must have access to it
ADMIN_DATABASE_URL=postgres://postgres@127.0.0.1:5432/postgres

# The name of the Database the App uses. It does not need to exist and will be created automatically by Codd if necessary
APP_DATABASE=codd-experiments

# A list of directories where SQL migration files will be found/added to. Do note that you can have e.g. a testing environment with an extra folder
# for itself to hold data migrations you don't want on Production.
# It's recommended to always have your "all migrations" folder first.
SQL_MIGRATION_PATHS=sql-migrations/all:sql-migrations/dev

# Folder where files will be created with checksums of DB objects. This folder will be wiped clean by codd every time it's necessary
DB_ONDISK_HASHES=sql-migrations/on-disk-hashes

# Space separated schemas to hash
CODD_SCHEMAS=public

# Space separated roles other than the ones specified above that must also be considered
CODD_EXTRA_ROLES=codd-user
````

## Starting out

After having configured your .env file and making sure Postgres is reachable run:

````bash
docker run --rm -it --env-file .env --network=host -v "$(pwd)/sql-migrations:/sql-migrations" mzabani/codd codd up-dev
````

*Note: If you have a `codd` executable available, there's no need for this long Docker command line; just make sure your variables in `.env` are exported and run `codd up-dev`.*

After this, you should be able to connect to your newly created Database, "codd-experiments".

## Adding a SQL Migration

Here's an example of a first migration which creates a non-admin User and a table of employees:

````sql
DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN
      CREATE USER "codd-user";
   END IF;
END
$do$;

GRANT CONNECT ON DATABASE "codd-experiments" TO "codd-user";

CREATE TABLE employee (
    employee_id SERIAL PRIMARY KEY
    , employee_name TEXT NOT NULL
);
````

1. Save this file anywhere with a name such as `create-user-and-employee-table.sql`.
2. In the older where your migration file is, run `docker run --rm -it --env-file .env --network=host -v "$(pwd)/sql-migrations:/sql-migrations" -v "$(pwd):/new-migrations" mzabani/codd codd add --apply new-migrations/create-user-and-employee-table.sql`.
3. The file will be renamed and moved to the first folder in `SQL_MIGRATION_PATHS`, it'll also run against your database.

After doing this, I recommend exploring your `DB_ONDISK_HASHES` folder. Everything in that folder should be put under version control; that's what will enable git to detect conflicts when developers make changes to the same database objects (e.g. same columns, indices, constraints etc.).

## More about SQL Migrations

Codd will always run every pending migration in a single transaction. Even if there's more than one pending migrations (you can add them without the `--apply` flag and then call `codd up-dev` to run them all) they will all run in the same transaction.

However, not any SQL can run inside a transaction. In Postgres, altering `enum` types and using the newly created `enum` values cannot run in the same transaction.
Because of that, there's a way to specify that a SQL migration cannot run in a transaction, as is exemplified below:


````sql
-- codd: no-txn
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
````

Codd will parse the comment in the first line and figure that this migration can't run in a Transaction.

**IMPORTANT:** By using `no-txn` migrations, you're taking great risk with the possibility of a migration failing when deploying and leaving the Database state in an intermediary state that is not compatible with the previously deployed application nor the to-be-deployed one. It is recommended that you avoid these at great costs, and if you do need them make sure that they won't make the previously deployed application fail if they run partially.
**IMPORTANT 2:** Codd will run blocks of consecutive `in-txn` migrations (that can run in transactions) in a single transaction. If there are blocks of `in-txn` migrations intertwined with `no-txn` migrations, each consecutive block runs either in a transaction or outside a transaction, accordingly.


[![asciicast](https://asciinema.org/a/wTdnsKvPV6rl9LTGC8B2pICuC.svg)](https://asciinema.org/a/wTdnsKvPV6rl9LTGC8B2pICuC)