# What is Codd?

_Codd_ is a tool to help teams of Developers version-control their Databases locally and for Deployment. It provides a few main features:

- A checksum of your entire Database to help minimize chances that your Production Database differs from your Development Database. This checksum is done in a way such that different developers writing SQL migrations that affect the same Database objects become merge conflicts, but touching distinct DB objects does not lead to conflicts.  
- A way to apply pending SQL migrations in a single DB Transaction when possible, rolling back in case checksums mismatch before committing.  
- **It is only compatible with PostgreSQL version 10, 11 and 12 (probably with 13 as well but hasn't been tested yet). No other databases are currently supported.**

## Installing Codd

Currently we provide _Codd_ through Docker. Run codd for the first time with `docker run --rm mzabani/codd codd --help`.

## Configuring Codd

_Codd_ will hash DB Objects to ensure Database-equality between different environments such as Development and Production. But we have to first set it up to let it know which top-level objects - such as schemas and roles - it will consider, and connection strings for it to connect.

Let's take a look at an example `.env` file for _Codd_. These environment variables must be defined when running the `codd` executable.

````.env
# A connection string in the format postgres://username[:password]@host:port/database_name
# This connection string must be for an ADMIN user, the database must exist
# and the user must have access to it
CODD_ADMIN_CONNECTION=postgres://postgres@127.0.0.1:5432/postgres

# The name of the Database the App uses. It does not need to exist and will be created
# automatically by Codd if necessary
CODD_APPDB=codd-experiments

# A list of directories where SQL migration files will be found/added to. Do note that you
# can have e.g. a testing environment with an extra folder for itself to hold data migrations
# you don't want on Production. It's recommended to always have your "all migrations" folder first.
CODD_MIGRATION_DIRS=sql-migrations/all:sql-migrations/dev-only

# Folder where files will be created with checksums of DB objects. This folder will be
# wiped clean by codd every time it's necessary
CODD_CHECKSUM_DIR=sql-migrations/on-disk-hashes

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
2. In the older where your migration file is, run `docker run --rm -it --env-file .env --network=host -v "$(pwd)/sql-migrations:/sql-migrations" -v "$(pwd):/new-migrations" mzabani/codd codd add new-migrations/create-user-and-employee-table.sql`.
3. The file will be renamed and moved to the first folder in `CODD_MIGRATION_DIRS`, it'll also run against your database.

After doing this, I recommend exploring your `CODD_CHECKSUM_DIR` folder. Everything in that folder should be put under version control; that's what will enable git to detect conflicts when developers make changes to the same database objects (e.g. same columns, indices, constraints etc.).

## More about SQL Migrations

_Codd_ will - when possible - run every pending migration in a single transaction. Even if there's more than one pending migration, such as what typically happens when deploying and running migrations in Production, they will all run in the same transaction.

However, not all SQL can run inside a transaction. In Postgres, altering `enum` types and using the newly created `enum` values cannot run in the same transaction.
Because of that, there's a way to specify that a SQL migration cannot run in a transaction, as is exemplified below:


````sql
-- codd: no-txn
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
UPDATE employee SET employee_experience='intern';
````

_Codd_ will parse the comment in the first line and figure that this migration can't run in a Transaction.


## Important notes about SQL migrations

1. By using `no-txn` migrations, you're taking great risk with the possibility of a migration failing when deploying and leaving the Database state in an intermediary state that is not compatible with the previously deployed application nor the to-be-deployed one. It is recommended that you avoid these at great costs and plan carefully when adding even one of them.  
2. `COPY` is not supported.  
3. _Codd_ will run blocks of consecutive `in-txn` migrations (that can run in transactions) in a single transaction. If there are blocks of `in-txn` migrations intertwined with `no-txn` migrations, each consecutive block runs either in a transaction or outside a transaction, accordingly. 

## Start using Codd in an existing Database

If you already have a Database and would like to start using _Codd_, here's a suggestion on how to approach the problem:

1. Configure your `.env` file as explained in this guide.
2. In that configuration make sure you have that extra `dev-only` folder to hold SQL migrations that will only run in developers' machines.
3. Run `pg_dump --column-inserts -N codd_schema your_database > bootstrap-migration.sql`
4. Edit `bootstrap-migration.sql` and add `-- codd: no-txn` as its very first line.
5. Run `dropdb your_database; codd add bootstrap-migration.sql --dest-folder your-dev-only-folder`
6. You should now have your Database back and managed through _Codd_.
7. Make sure your Production `.env` does not contain your `dev-only` folder. Add any future SQL migrations to your `all-migrations` folder.