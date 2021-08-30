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
* [Frequently Asked Questions](#FrequentlyAskedQuestions)

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
# This connection string must be for a user with the CREATE and CONNECT permissions. The
# database must already exist.
CODD_ADMIN_CONNECTION=postgres://postgres@127.0.0.1:5432/postgres

# The name of the Database the App uses. It does not need to exist and will be created
# automatically by Codd if necessary.
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

# Codd uses the default isolation level in READ WRITE mode, but you can override
# that with the (optional) environment below.
# Choose "db-default|serializable|repeatable read|read committed|read uncommitted"
CODD_TXN_ISOLATION=db-default

# Migrations can fail due to temporary errors, so Codd retries up to 2 times by default
# when migrations fail, but you can control that with this variable.
# Its format is "max MAXRETRIES backoff (constant|exponential) TIME(s|ms)"
CODD_RETRY_POLICY=max 2 backoff exponential 1.5s
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
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd_user') THEN
      CREATE USER codd_user;
   END IF;
END
$do$;

GRANT CONNECT ON DATABASE "codd-experiments" TO codd_user;

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

Using `no-txn` migrations adds great risk by allowing your database to be left in a state that is undesirable. It is highly recommended reading [SQL-migrations.md](docs/SQL-MIGRATIONS.md) if you plan to add them, or if you just want to learn more.

## <a name='StartusingCoddinanexistingDatabase'></a>Start using Codd in an existing Database

If you already have a Database and would like to start using _Codd_, here's a guideline to approach the problem. Remember to be very careful if/when making any changes to your Prod DB:

1. Configure your `.env` file as explained in this guide.
2. In that configuration make sure you have that extra `dev-only` folder to hold SQL migrations that will only run in developers' machines.
3. Run `pg_dump your_database > bootstrap-migration.sql`. Do not use `pg_dumpall` because it includes _psql_'s meta-commands that _codd_ doesn't support.
4. Run `dropdb your_database` to drop your DB.
5. Run something like `createdb -T template0 -E UTF8 -l en_US.UTF8 your_database`, but with encoding and locale equal to your Production DB's. Database and the _public_'s Schema ownership might need some manual intervention to match in different environments.
   - **What do we mean?** Cloud services such as Amazon's RDS will create Schemas and DBs owned by users managed by them - such as the `rdsadmin` user -, that we don't usually replicate locally. We can either replicate these locally so we don't need to touch our Prod DB or change our Prod DB so only users managed by us are ever referenced in any environment.
   - Use _psql_'s `\l` to check DB ownership and permissions of your Prod DB.
   - Use _psql_'s `\dn+` to check the _public_ schema's ownership and permissions in your Prod DB.
   - **Note:** Because _codd_ runs migrations with the user in the supplied connection string, that user must already have proper permissions and must be the same across environments so newly created objects have the same ownership. It also helps a lot if that user is the DB owner because some SQL statements are only allowed for DB owners.
6. The user that you supply in your connection string can't be created in a migration because it needs to exist before _codd_ runs. That also applies to DB ownership, locale and encoding, among a few other things, so it is helpful to keep a script that creates your DB and sets up those bits in case you want to recreate it. See [scripts/create-dev-db.sh](scripts/create-dev-db.sh) for an example, but make sure to use the `createdb` statement you came up with in step 5. After creating one such script, **run it**.
7. Add `CREATE USER`-like migrations for any non-admin users you need (`pg_dumpall --roles-only` before step 4 can help you with that) and run `codd add create-users-migration.sql --dest-folder your-dev-only-folder`.
8. Edit `bootstrap-migration.sql` (created in step 3) and add `-- codd: no-txn` as its very first line.
9.  Run `codd add bootstrap-migration.sql --dest-folder your-dev-only-folder`
10. You should now have your Database back and managed through _Codd_.
11. Make sure your separate Production `.env` file does not contain your `dev-only` folder. Add any future SQL migrations to your `all-migrations` folder.
12. Before deploying with _codd_, we strongly recommend you run `codd verify-checksums -v` with your environment variables connected to your Production database and make sure checksums match.
13. In Production, we strongly recommend running `codd up-deploy --soft-check` to start with until you get acquainted enough to consider hard-checking. Make sure you read `codd up-deploy --help` to better understand your options.

## <a name='Safetyconsiderations'></a>Safety considerations

We recommend following these instructions closely to avoid several problems. Even then, they do not guarantee everything will work smoothly.

- Read about what _codd_ **cannot do** in [DATABASE-EQUALITY.md](docs/DATABASE-EQUALITY.md#Delayedeffectinpg_catalog).  
- Never merge code that has been tested without `master` merged into it.
  - There are non-conflicting changes which can break your App. One example is one developer removes a column and another developer writes a new query using that column. Only a test could catch this.  
- Always run `codd up-deploy` on CI because that's what will be used in your Production environments.
- After running `codd up-deploy` on CI, make sure `codd verify-checksums` doesn't error. It might seem redundant because `codd up-deploy` verifies checksums, but there are corner cases. Read more about this in [DATABASE-EQUALITY.md](docs/DATABASE-EQUALITY.md#Delayedeffectinpg_catalog).

## <a name='FrequentlyAskedQuestions'></a>Frequently Asked Questions

1. ### Why does taking and restoring a database dump affect my checksums?
   `pg_dump` does not dump all of the schema state that _codd_ checks. A few examples include (at least with PG 13) role related state, the database's default transaction isolation level and deferredness, among possibly others. So check that it isn't the case that you get different schemas when that happens. We recommend using `pg_dumpall` to preserve If you've checked and they are the same please report a bug.