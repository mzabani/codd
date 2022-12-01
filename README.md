# What is Codd?

_Codd_ is a tool to help teams of developers version-control their PostgreSQL databases locally and for deployment. It provides a few main features:

<table>
<tr>
   <td>Plain SQL migrations</td>
   <td>

````shell
$ cat create-animals-table.sql
CREATE TABLE animals (id SERIAL PRIMARY KEY, popular_name TEXT NOT NULL);
INSERT INTO animals (popular_name) VALUES ('Dog'), ('Cat');
$ codd add create-animals-table.sql
Migration applied and added to sql-migrations/all/2022-02-27-23-14-50-create-animals-table.sql
$ psql -c "SELECT popular_name FROM animals"
 popular_name
--------------
 Dog
 Cat
(2 rows)
````

</td>
</tr>

<tr>
<td>Extensive schema equality checks</td>
<td>

````shell
$ psql -c "ALTER TABLE animals ALTER COLUMN popular_name TYPE VARCHAR(30)"
ALTER TABLE
$ codd verify-schema
[Error] DB and expected checksums do not match. Differences are (Left is Database, Right is expected): {"schemas/public/tables/animals/cols/popular_name":"different-checksums"}
````

</td>
</tr>

<tr>
<td>Applies pending migrations in a single transaction, optionally rolls back on schema mismatch before committing¹</td>
<td>

````shell
$ codd up
[Info] Checking if database 'codd-experiments' is accessible with the configured connection string... (waiting up to 5sec)
[Info] Checking which SQL migrations have already been applied...
[Info] Parse-checking headers of all pending SQL Migrations...
[Info] BEGINning transaction
[Info] Applying 2022-02-27-23-14-50-create-animals-table.sql
[Info] Applying 2022-02-27-23-30-41-create-people-table.sql
[Info] Database and expected schemas match.
[Info] COMMITed transaction
[Info] All migrations applied to codd-experiments successfully
````

</td>
</tr>

<tr>
<td>Meaningful merge conflicts²</td>
<td>

````shell
$ git merge branch-with-conflicting-db-migration
Auto-merging sql-migrations/db-checksum/schemas/public/tables/animals/cols/popular_name
CONFLICT (content): Merge conflict in sql-migrations/db-checksum/schemas/public/tables/animals/cols/popular_name
Automatic merge failed; fix conflicts and then commit the result.
````

</td>
</tr>

</table>

¹ Some SQL must run without explicit transactions; single-transaction application only works when none of that is present.  
² There can be false positives and false negatives in some cases.  

<!-- vscode-markdown-toc -->
- [What is Codd?](#what-is-codd)
  - [Installing Codd](#installing-codd)
    - [1. Nix](#1-nix)
    - [2. Docker](#2-docker)
  - [Get codd up and running in 15 minutes](#get-codd-up-and-running-in-15-minutes)
  - [Start using codd with an existing database](#start-using-codd-with-an-existing-database)
  - [Safety considerations](#safety-considerations)
  - [Frequently Asked Questions](#frequently-asked-questions)

<!-- vscode-markdown-toc-config
	numbering=false
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

## Installing Codd

We currently provide two installation methods.

### 1. Nix

This method will install an executable named `codd` and make it available in your PATH just like installing from a package manager would. It is a bit more cumbersome to install than with docker but easier to use once installed.

1. Install Nix if you don't have it yet by using your package manager or following instructions from https://nixos.org/download.html.
2. Add a Nix cache with precompiled packages so you don't have to compile a ton of things. This step is optional but highly recommended, otherwise you may have to compile things for hours. Add these to your `/etc/nix/nix.conf` file (or `~/.config/nix/nix.conf` if you know what you're doing your user is in Nix's trust list). If you already have other substituters and trusted public keys, just append these new values to them instead of adding new lines.
   ```
   substituters = https://mzabani.cachix.org
   trusted-public-keys = mzabani.cachix.org-1:wnkKakfl+rbT7zTtV1P1tAtjBTuh6GQVX7LfSd9sMbA=
   ```
3. Run `sh <(curl -L https://raw.githubusercontent.com/mzabani/codd/master/nix/install-codd.sh)` to install _codd_. If things are building and taking too long, you may want to check you did step 2 correctly. After installed, just run `codd --help` to invoke it for the first time. To uninstall it, run `nix-env --uninstall codd-exe-codd`.

### 2. Docker

You can find up-to-date images of _codd_ in DockerHub. To run _codd_ through docker just run `docker run --rm mzabani/codd --help`.
Invoking _codd_ this way will often require mounting volumes, specifying UIDs and is certainly more bureaucratic than installing with Nix.

## Get codd up and running in 15 minutes

Here's a super quick way to get a taste of _codd_ if you have postgres running. Let's first define three required environment variables:

````shell
$ export CODD_CONNECTION=postgres://postgres@localhost/codd_experiments
$ export CODD_MIGRATION_DIRS=sql-migrations
$ export CODD_CHECKSUM_DIR=codd-checksums
````

Make sure you create the `sql-migrations` folder. If you're using docker, it helps to have these environment variables in a _.env_ file.

But the database `codd_experiments` doesn't exist yet, so this connection string will not work. That is not a problem, and we can make _codd_ [create this database](docs/BOOTSTRAPPING.md) for us with a migration that overrides the connection string just for itself.

Create this file and save it as `bootstrap-db.sql`:

````sql
-- codd: no-txn
-- codd-connection: postgres://postgres@localhost/postgres

CREATE DATABASE codd_experiments;
````

That's a lot to take in. _Codd_ handles pure SQL migrations but also has some special header comments defined that can make it do special things.

- The `-- codd: no-txn` header comment specifies that this migration can't run inside a transaction. Postgres doesn't allow us to create databases (plus a few other statements) inside transactions, after all.
- The `-- codd-connection` header comment specifies that this specific migration will run with its own connection string, not with the default one.

You can find more about the special migration directives that _codd_ understands [here](docs/SQL-MIGRATIONS.md#configurability).

Now add this migration by running one of the two commands below:

````shell
$ # If you installed codd with Nix
$ codd add bootstrap-db.sql

$ # If you're using the docker image with a .env file:
$ docker run --rm -it --env-file .env --network=host --user `id -u`:`id -g` -v "$(pwd):/working-dir" mzabani/codd add bootstrap-db.sql
````

The file should now have been timestamped and moved to the `sql-migrations` folder. The migration ran and so the `codd_experiments` database was created, and checksums were written to the `codd-checksums` folder.

Optionally, explore the `codd-checksums` folder. You won't find much yet, but all the files in there reflect existing database objects. That's how _codd_ knows if schemas in different environments match and also how multiple developers can add migrations and get warned by merge conflicts if any two people modify the same database object.

Just for completeness, let's now create a table. Write the following to a `create-employees-table.sql`:

````sql
CREATE TABLE employee (
    employee_id SERIAL PRIMARY KEY
    , employee_name TEXT NOT NULL
);
INSERT INTO employee (employee_name) VALUES ('John Doe');
````

Add this migration with `codd add` just like you did to the previous one and it will be added and applied.

Before we finish this tutorial, some things you might want to do:
- psql into your database and manually create a table there, without a migration. Then run `codd verify-schema`.
- Run `dropdb codd_experiments` and then `codd up` to get a fresh database from the start.
- Read all the knobs you can configure codd with in [CONFIGURATION.md](docs/CONFIGURATION.md).
- Read [safety considerations](#safety-considerations).

## Start using codd with an existing database

If you already have a database and want to start using _codd_ without losing it, read [START-USING.md](docs/START-USING.md).
If you're running _codd_ in multiple environments where connection strings can differ between them, [environment variable templating](docs/SQL-MIGRATIONS.md#templating-environment-variables-into-migrations) might be of assistance.

## Safety considerations

We recommend following these instructions closely to catch as many possible issues with your database setup/management as possible.

- Never merge code that has been tested without `master` merged into it.
  - There are non-conflicting changes which can break your App. One example is one developer removes a column and another developer writes a new query using that column. Only a test could catch this.  
- Always run `codd up --strict-check` on CI because it's a good place to be strict.
- After running `codd up --strict-check` on CI, make sure `codd verify-schema` doesn't error. It might seem redundant because `codd up --strict-check` verifies checksums, but [there are edge cases](docs/DATABASE-EQUALITY.md#Delayedeffectinpg_catalog).
- Read about what _codd_ **cannot do** in [DATABASE-EQUALITY.md](docs/DATABASE-EQUALITY.md#Delayedeffectinpg_catalog). This will also give you another idea about how far _codd_ is willing to go to ensure your schema is the same across environments.  

## Frequently Asked Questions

1. ### Why does taking and restoring a database dump affect my checksums?
   `pg_dump` does not dump all of the schema state that _codd_ checks. A few examples include (at least with PG 13) role related state, the database's default transaction isolation level and deferredness, among possibly others. So check that it isn't the case that you get different schemas when that happens. We recommend using `pg_dumpall` to preserve more when possible instead. If you've checked with `psql` and everything looks to be the same please report a bug in _codd_.
