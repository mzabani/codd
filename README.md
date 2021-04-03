# What is Codd?

_Codd_ is a tool to help teams of developers version-control their PostgreSQL databases locally and for deployment. It provides a few main features:

- Plain SQL migrations that you can add by simply running `codd add migration-file.sql`
- Schema equality checks to reduce the chances your production database's schema differs from your development schema. This check includes roles and permissions, among other things.
- Running every pending deployed migration in a single transaction when possible, with the option to rollback if the expected schema does not match the schema during development.

**It is only compatible with PostgreSQL version 10 to 13. No other databases are currently supported.**

<!-- vscode-markdown-toc -->
* [Installing Codd](#InstallingCodd)
	* [1. Docker](#Docker)
	* [2. Nix](#Nix)
* [Configuring Codd](#ConfiguringCodd)
* [Starting out](#Startingout)
* [Adding a SQL Migration](#AddingaSQLMigration)
	* [no-txn migrations and more](#no-txnmigrationsandmore)
* [Start using Codd in an existing Database](#StartusingCoddinanexistingDatabase)
* [Safety considerations](#Safetyconsiderations)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## <a name='InstallingCodd'></a>Installing Codd

We currently provide two installation methods.

### <a name='Docker'></a>1. Docker

We keep up-to-date images of _Codd_ in DockerHub. To run _Codd_ through docker just run `docker run --rm mzabani/codd --help`.
Invoking _Codd_ this way will require mounting volumes and is certainly more bureaucratic than other installation methods.

### <a name='Nix'></a>2. Nix

This method will install an executable named `codd` and make it available in your PATH just like installing from a package manager would.

1. Install Nix if you don't have it yet by using your package manager or running `sh <(curl -L https://nixos.org/nix/install) --daemon` and following its instructions.
2. Run `sh <(curl -L https://raw.githubusercontent.com/mzabani/codd/master/nix/install-codd.sh)` to install _Codd_. Now just run `codd --help` to invoke it for the first time. To uninstall it, run `nix-env --uninstall codd-exe-codd`.

## <a name='ConfiguringCodd'></a>Configuring Codd

_Codd_ will checksum DB objects to ensure database-equality between different environments such as Development and Production. But we have to first set it up to let it know which top-level objects — such as schemas and roles — it will consider, and connection strings for it to connect.

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
CODD_CHECKSUM_DIR=sql-migrations/on-disk-cksums

# Space separated schemas to checksum
CODD_SCHEMAS=public

# Space separated roles other than the ones specified above that must also be considered
CODD_EXTRA_ROLES=codd-user
````

## <a name='Startingout'></a>Starting out

After having configured your .env file and making sure Postgres is reachable run one of these:

````bash
# With docker
$ docker run --rm -it --env-file .env --network=host --user `id -u`:`id -g` -v "$(pwd):/working-dir" mzabani/codd up-dev

# .. or with Nix
$ codd up-dev
````

After this, you should be able to connect to your newly created Database, "codd-experiments".

## <a name='AddingaSQLMigration'></a>Adding a SQL Migration

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

### <a name='no-txnmigrationsandmore'></a>no-txn migrations and more

Not all SQL can run inside a transaction. One example is altering `enum` types and using the newly created `enum` values.
Because of that, you can tell *codd* not to run a migration in a transaction, as is exemplified below:

````sql
-- codd: no-txn
ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';
UPDATE employee SET employee_experience='intern';
````

_Codd_ will parse the comment in the first line and understand that this migration can't run in a transaction.  

Using `no-txn` migrations adds great risk by allowing your database to b left in a state that is undesirable. It is highly recommended reading [SQL-migrations.md](docs/SQL-MIGRATIONS.md) if you plan to add them, or if you just want to learn more.

## <a name='StartusingCoddinanexistingDatabase'></a>Start using Codd in an existing Database

If you already have a Database and would like to start using _Codd_, here's a suggestion on how to approach the problem:

1. Configure your `.env` file as explained in this guide.
2. In that configuration make sure you have that extra `dev-only` folder to hold SQL migrations that will only run in developers' machines.
3. Run `pg_dump -N codd_schema your_database > bootstrap-migration.sql`
4. Edit `bootstrap-migration.sql` and add `-- codd: no-txn` as its very first line.
5. Run `dropdb your_database; codd add bootstrap-migration.sql --dest-folder your-dev-only-folder`
6. You should now have your Database back and managed through _Codd_.
7. Make sure your Production `.env` does not contain your `dev-only` folder. Add any future SQL migrations to your `all-migrations` folder.

## <a name='Safetyconsiderations'></a>Safety considerations

We recommend following these instructions closely to avoid several problems. Even then, they do not guarantee everything will work smoothly.

- Read about what _codd_ **cannot do** in [DATABASE-EQUALITY.md](docs/DATABASE-EQUALITY.md#Delayedeffectinpg_catalog).  
- Never merge code that has been tested without `master` merged into it.
  - There are non-conflicting changes which can break your App. One example is one developer removes a column and another developer writes a new query using that column. Only a test could catch this.  
- Always run `codd up-deploy` on CI because that's what will be used in your Production environments.
- After running `codd up-deploy` on CI, make sure `codd verify-checksums` doesn't error. It might seem redundant because `codd up-deploy` verifies checksums, but there are corner cases. Read more about this in [DATABASE-EQUALITY.md](docs/DATABASE-EQUALITY.md#Delayedeffectinpg_catalog).