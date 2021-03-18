# What is Codd?

_Codd_ is a tool to help teams of Developers version-control their Databases locally and for Deployment. It provides a few main features:

- Checksums of your Database to help minimize chances that your Production Database differs from your Development Database. This checksum is done in a way such that different developers writing SQL migrations that affect the same Database objects become merge conflicts, but touching distinct DB objects does not lead to conflicts. Currently, codd **can't guarantee complete schema equality**, so beware of that.  
- A way to apply pending SQL migrations in a single DB Transaction when possible, rolling back in case checksums mismatch before committing.  

**It is only compatible with PostgreSQL version 10, 11 and 12 (should also work with 13 but only supporting v12's features). No other databases are currently supported.**

## Installing Codd

We currently provide two installation methods.

### 1. Docker

We keep up-to-date images of _Codd_ in DockerHub. To run _Codd_ through docker just run `docker run --rm mzabani/codd --help`.
Invoking _Codd_ this way will require mounting volumes and is certainly more bureaucratic than other installation methods.

### 2. Nix

This method will install an executable named `codd` and make it available in your PATH just like installing from a package manager would.

1. Install Nix if you don't have it yet by using your package manager or running `sh <(curl -L https://nixos.org/nix/install) --daemon` and following its instructions.
2. Run `sh <(curl -L https://raw.githubusercontent.com/mzabani/codd/master/nix/install-codd.sh)` to install _Codd_. Now just run `codd --help` to invoke it for the first time. To uninstall it, run `nix-env --uninstall codd-exe-codd`.

## Configuring Codd

_Codd_ will hash DB Objects to ensure Database-equality between different environments such as Development and Production. But we have to first set it up to let it know which top-level objects - such as schemas and roles - it will consider, and connection strings for it to connect.

Let's take a look at an example `.env` file for _Codd_. These environment variables must be defined when running the `codd` executable. We suggest you add this file to you project's root folder.

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

After having configured your .env file and making sure Postgres is reachable run one of these:

````bash
# With docker
$ docker run --rm -it --env-file .env --network=host --user `id -u`:`id -g` -v "$(pwd):/working-dir" mzabani/codd up-dev

# .. or with Nix
$ codd up-dev
````

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

1. Save this file in your project's root folder with a name such as `create-user-and-employee-table.sql`.
2. Run 
   
   ````bash
   # With docker
   $ docker run --rm -it --env-file .env --network=host --user `id -u`:`id -g` -v "$(pwd):/working-dir" mzabani/codd add create-user-and-employee-table.sql

   # .. or with Nix
   $ codd add create-user-and-employee-table.sql
   ````
3. The file will be renamed and moved to the first folder in `CODD_MIGRATION_DIRS`, it'll also run against your database.

After doing this, I recommend exploring your `CODD_CHECKSUM_DIR` folder. Everything in that folder should be put under version control; that's what will enable git to detect conflicts when developers make changes to the same database objects (e.g. same columns, indices, constraints etc.).

## More about SQL Migrations

_Codd_ will — when possible — run every pending migration in a single transaction. Even if there's more than one pending migration, such as what typically happens when deploying and running migrations in Production, they will all run in the same transaction.

However, not all SQL can run inside a transaction. One example is altering `enum` types and using the newly created `enum` values.
Because of that, you can tell *codd* not to run a migration in a transaction, as is exemplified below:

````sql
-- codd: no-txn
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
UPDATE employee SET employee_experience='intern';
````

_Codd_ will parse the comment in the first line and understand that this migration can't run in a transaction. There are caveats of doing this, so keep on reading to know more.

## Important notes about SQL migrations

1. By using `no-txn` migrations, you're taking great risk with the possibility of a migration failing when deploying and leaving the Database in an intermediary state that is not compatible with the previously deployed application nor the to-be-deployed one. It is recommended that you avoid these at great costs and plan carefully when adding even one of them.  
2. _Codd_ will run blocks of consecutive `in-txn` migrations (that can run in transactions) in a single transaction. If there are blocks of `in-txn` migrations intertwined with `no-txn` migrations, each consecutive block runs either in a transaction or outside a transaction, accordingly.  
3. `COPY FROM STDIN` is supported but other forms of `COPY` or psql's meta commands, including `\COPY`, _are not_.  

## Schema equality checks

*Codd*'s Database checksum process does not yet checksum every DB object or every DB object's attributes. For a more thorough - but a bit drafty and _not necessarily up to date_ - description of what is checksummed, see [SCHEMA-MAPPINGS.md](docs/SCHEMA-MAPPINGS.md). What follows is an incomplete list of what currently is checksummed, but be aware that not all pertinent attributes are necessarily included:

- Tables, columns, CHECK constraints, FKs, indexes and other constraints
- Indexes
- Sequences (although their _RESTART_ value are not currently checked)
- Functions, operators and VIEWs
- Triggers
- Row Level Security
- Roles, including their config attributes such as `search_path`, which other roles they belong to and database-related permissions.
- Database encoding and its `default_transaction_isolation`

In contrast, an **incomplete** list of things that are **not currently checksummed:**

- Collations
- Extensions
- Partitioning
- Foreign Servers
- Others

Checksumming every possible object is a top priority of *codd*, so if something's missing or not behaving as intended, please file a bug report.

## Start using Codd in an existing Database

If you already have a Database and would like to start using _Codd_, here's a suggestion on how to approach the problem:

1. Configure your `.env` file as explained in this guide.
2. In that configuration make sure you have that extra `dev-only` folder to hold SQL migrations that will only run in developers' machines.
3. Run `pg_dump -N codd_schema your_database > bootstrap-migration.sql`
4. Edit `bootstrap-migration.sql` and add `-- codd: no-txn` as its very first line.
5. Run `dropdb your_database; codd add bootstrap-migration.sql --dest-folder your-dev-only-folder`
6. You should now have your Database back and managed through _Codd_.
7. Make sure your Production `.env` does not contain your `dev-only` folder. Add any future SQL migrations to your `all-migrations` folder.