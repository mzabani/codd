# What is Codd?

_Codd_ is a tool to help teams of developers version-control their PostgreSQL databases locally and for deployment. It provides a few main features:

- Plain SQL migrations that you can add by simply running `codd add migration-file.sql`
- Schema equality checks to reduce the chances your production database's schema differs from your development schema. This check includes roles and permissions, among other things.
- Running every pending deployed migration in a single transaction when possible, with the option to rollback if the expected schema does not match the schema during development.

**It is only compatible with PostgreSQL version 10 to 13. No other databases are currently supported.**

<!-- vscode-markdown-toc -->
* [Installing Codd](#installing-codd)
	* [1. Nix (preferred)](#1-nix-preferred)
	* [2. Docker](#2-docker)
* [Configuring Codd](#configuring-codd)
* [Starting out](#starting-out)
* [Adding a SQL Migration](#adding-a-sql-migration)
	* [no-txn migrations and more](#no-txn-migrations-and-more)
* [Start using Codd in an existing database](#start-using-codd-in-an-existing-database)
* [Safety considerations](#safety-considerations)
* [Frequently Asked Questions](#frequently-asked-questions)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## Installing Codd

We currently provide two installation methods.

### 1. Nix (preferred)

This method will install an executable named `codd` and make it available in your PATH just like installing from a package manager would.

1. Install Nix if you don't have it yet by using your package manager or running `sh <(curl -L https://nixos.org/nix/install) --daemon` and following its instructions.
2. Run `sh <(curl -L https://raw.githubusercontent.com/mzabani/codd/master/nix/install-codd.sh)` to install _Codd_. Now just run `codd --help` to invoke it for the first time. To uninstall it, run `nix-env --uninstall codd-exe-codd`.

### 2. Docker

We keep up-to-date images of _Codd_ in DockerHub. To run _Codd_ through docker just run `docker run --rm mzabani/codd --help`.
Invoking _Codd_ this way will require mounting volumes and is certainly more bureaucratic than other installation methods.

## Configuring Codd

_Codd_ will checksum DB objects to ensure database-equality between different environments such as Development and Production. But we have to first set it up to let it know which top-level objects — such as schemas and roles — it will consider, and connection strings for it to connect.

Let's take a look at an example `.env` file for _Codd_. These environment variables must be defined when running the `codd` executable.  

````.env
# A connection string in the format postgres://username[:password]@host:port/database_name
# This connection string will be used to apply migrations. You can specify
# custom connection strings on a per-migration basis too.
CODD_CONNECTION=postgres://postgres@127.0.0.1:5432/postgres

# A list of directories where SQL migration files will be found/added to. Do note that you
# can have e.g. a testing environment with an extra folder for itself to hold data migrations
# you don't want on Production. It's recommended to always have your "all migrations" folder first.
CODD_MIGRATION_DIRS=sql-migrations/all:sql-migrations/dev-only

# Folder where files will be created with checksums of DB objects. This folder will be
# wiped clean by codd every time it's necessary
CODD_CHECKSUM_DIR=sql-migrations/on-disk-cksums

# Space separated schemas to checksum
CODD_SCHEMAS=public

# Space separated roles other than the ones specified above that must also be considered
CODD_EXTRA_ROLES=codd-user

# Codd uses the default isolation level in READ WRITE mode, but you can override
# that with the (optional) environment below.
# Choose "db-default|serializable|repeatable read|read committed|read uncommitted"
CODD_TXN_ISOLATION=db-default

# Migrations can fail due to temporary errors, so Codd retries up to 2 times by default
# when migrations fail, but you can control that with this variable.
# Its format is "max MAXRETRIES backoff (constant|exponential) TIME(s|ms)"
CODD_RETRY_POLICY=max 2 backoff exponential 1.5s
````

## Starting out

After having configured your environment variables and making sure Postgres is reachable run one of these:

````bash
# With docker
$ docker run --rm -it --env-file .env --network=host --user `id -u`:`id -g` -v "$(pwd):/working-dir" mzabani/codd up

# .. or with Nix (make sure env vars are all exported)
$ codd up
````

After this, you should be able to connect to your newly created Database, "codd_experiments".

## Adding a SQL Migration

Here's a super quick way to experiment with _codd_. Let's create a table of employees with one employee inside by writing the following SQL to a file:

````sql
CREATE TABLE employee (
    employee_id SERIAL PRIMARY KEY
    , employee_name TEXT NOT NULL
);
INSERT INTO employee (employee_name) VALUES ('John Doe');
````

1. Now save this file in your project's root folder with a name such as `create-user-and-employee-table.sql`.
2. Make sure the connection string configured in `CODD_CONNECTION` works¹.
3. Run 
   
   ````bash
   # With docker
   $ docker run --rm -it --env-file .env --network=host --user `id -u`:`id -g` -v "$(pwd):/working-dir" mzabani/codd add create-user-and-employee-table.sql

   # .. or with Nix
   $ codd add create-user-and-employee-table.sql
   ````
4. The file will be renamed and moved to the first folder in `CODD_MIGRATION_DIRS`, it'll also run against your database.

After doing this, I recommend exploring your `CODD_CHECKSUM_DIR` folder. Everything in that folder should be put under version control; that's what will enable git to detect conflicts when developers make changes to the same database objects (e.g. same columns, indices, constraints etc.).

¹ _Codd_ can create your database for you through a process called [bootstrapping](docs/BOOTSTRAPPING.md).

### no-txn migrations and more

Not all SQL can run inside a transaction. One example is altering `enum` types and using the newly created `enum` values.
Because of that, you can tell *codd* not to run a migration in a transaction, as is exemplified below:

````sql
-- codd: no-txn
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
UPDATE employee SET employee_experience='intern';
````

_Codd_ will parse the comment in the first line and understand that this migration can't run in a transaction.  

Using `no-txn` migrations adds great risk by allowing your database to be left in a state that is undesirable. It is highly recommended reading [SQL-migrations.md](docs/SQL-MIGRATIONS.md) if you plan to add them, or if you just want to learn more.

## Start using codd

If you already have a database and want to start using _codd_ without losing it, read [START-USING.md](docs/START-USING.md).
If you want to have a process where `codd up` will even create your database if necessary, read [BOOTSTRAPPING.md](docs/BOOTSTRAPPING.md).

## Safety considerations

We recommend following these instructions closely to avoid several problems. Even then, they do not guarantee everything will work smoothly.

- Read about what _codd_ **cannot do** in [DATABASE-EQUALITY.md](docs/DATABASE-EQUALITY.md#Delayedeffectinpg_catalog).  
- Never merge code that has been tested without `master` merged into it.
  - There are non-conflicting changes which can break your App. One example is one developer removes a column and another developer writes a new query using that column. Only a test could catch this.  
- Always run `codd up --strict-check` on CI because it's a good place to be strict.
- After running `codd up --strict-check` on CI, make sure `codd verify-checksums` doesn't error. It might seem redundant because `codd up --strict-check` verifies checksums, but there are corner cases. Read more about this in [DATABASE-EQUALITY.md](docs/DATABASE-EQUALITY.md#Delayedeffectinpg_catalog).

## Frequently Asked Questions

1. ### Why does taking and restoring a database dump affect my checksums?
   `pg_dump` does not dump all of the schema state that _codd_ checks. A few examples include (at least with PG 13) role related state, the database's default transaction isolation level and deferredness, among possibly others. So check that it isn't the case that you get different schemas when that happens. We recommend using `pg_dumpall` to preserve more, but it still seems to lose schema permissions in some cases, for instance. If you've checked with `psql` and everything looks to be the same please report a bug in _codd_.
2. ### How do I specify custom connection strings on a per-migration basis?
   Add a `-- codd-connection` comment to the first lines of your migration. You can see an example at [BOOTSTRAPPING.md](docs/BOOTSTRAPPING.md).