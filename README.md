[![CI](https://github.com/mzabani/codd/actions/workflows/main.yml/badge.svg)](https://github.com/mzabani/codd/actions/workflows/main.yml)

# What is Codd?

<!--toc:start-->
- [What is Codd?](#what-is-codd)
  - [Installing Codd](#installing-codd)
    - [1. Self-contained executable](#1-self-contained-executable)
    - [2. Nix](#2-nix)
    - [3. Docker](#3-docker)
  - [Get codd up and running in 15 minutes](#get-codd-up-and-running-in-15-minutes)
  - [Guides](#guides)
    - [Start using codd with an existing database](#start-using-codd-with-an-existing-database)
    - [Add background migrations to update large datasets without downtime](#add-background-migrations-to-update-large-datasets-without-downtime)
    - [Codd's limits and robustness](#codds-limits-and-robustness)
  - [Frequently Asked Questions](#frequently-asked-questions)
    - [Why does taking and restoring a database dump affect my expected codd schema?](#why-does-taking-and-restoring-a-database-dump-affect-my-expected-codd-schema)
<!--toc:end-->

Codd is a CLI tool that applies plain SQL migrations atomically (when PostgreSQL allows it) and includes schema equality checks that practically ensure your development database's schema matches the database schema in every other environment,
checking table columns' names, types, order, available functions, roles, table privileges, object ownership, row security policies, database encoding [and much more](/docs/DATABASE-EQUALITY.md).
These schema equality checks happen automatically; you only need to write .sql files and `codd add migration-file.sql` them. No configuration files, JSON, or YAML; just 3 environment variables and .sql files and you can use codd.

It's also meant to be really simple to use: codd reads SQL files from folders you choose and applies migrations in order. Any special features for these migrations are typically special top-level comments in those SQL files, but you won't need them most of the time. Setting your environment up to use codd from scratch [takes 15 minutes](#get-codd-up-and-running-in-15-minutes).

In day to day usage, you will typically run `codd add new-migration.sql` and/or `codd up`, and very likely no other commands.

Compared to other DB tools, codd aims for simplicity, strong automatic schema equality checks, and robustness/reliability. This means it might not have all the features other tools do. It also only supports PostgreSQL.

Here you can see its main features in more detail:

<table>
<tr>
   <td>Plain SQL migrations</td>
   <td>

````shell
$ cat create-animals-table.sql
CREATE TABLE animals (id SERIAL PRIMARY KEY, popular_name TEXT NOT NULL);
INSERT INTO animals (popular_name) VALUES ('Dog'), ('Cat');
$ codd add --quiet create-animals-table.sql
New migration applied and added to sql-migrations/all/2024-03-05-19-27-43-create-animals-table.sql
Updated expected DB schema representations in the expected-schema folder
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
Error: DB and expected schemas do not match. Differing objects and their current DB schemas are: {"schemas/public/tables/animals/cols/popular_name":["different-schemas",{"collation":"default","collation_nsp":"pg_catalog","default":null,"generated":"","hasdefault":false,"identity":"","inhcount":0,"local":true,"notnull":true,"privileges":null,"type":"character varying(30)"}],"schemas/public/tables/animals/statistics/test_stat_with_expr":["different-schemas",{"definition":"id, popular_name, lower(popular_name::text), (id * 42)","kind":["d","e","f","m"],"owner":"codd_admin"}]}
````

</td>
</tr>

<tr>
<td>Applies pending migrations in a single transaction, optionally rolls back on schema mismatch before committing¹</td>
<td>

````shell
$ codd up
Checking if database codd-experiments is accessible with the configured connection string... (waiting up to 5sec)
Looking for pending migrations... [2 found]
BEGINning transaction
Applying 2022-02-27-23-14-50-create-animals-table.sql (0.08ms)
Applying 2022-02-27-23-30-41-create-people-table.sql (0.13ms)
Comparing actual and expected schemas... [match]
COMMITed transaction
Successfully applied all migrations to codd-experiments
````

</td>
</tr>

<tr>
<td>Meaningful merge conflicts²</td>
<td>

````shell
$ git merge branch-with-conflicting-db-migration
Auto-merging expected-schema/schemas/public/tables/animals/cols/popular_name
CONFLICT (content): Merge conflict in expected-schema/schemas/public/tables/animals/cols/popular_name
Automatic merge failed; fix conflicts and then commit the result.
````

</td>
</tr>

<tr>
<td>Useful diffs</td>
<td>

````diff
- "definition": "CHECK (value >= 0::numeric)",
+ "definition": "CHECK (value > 0::numeric)",
````

</td>
</tr>

<tr>
<td>Background/gradual migrations for large datasets</td>
<td>

````sql
ALTER TABLE employee ADD COLUMN new_employee_id BIGINT;
SELECT codd.populate_table_gradually('make-employee_id-a-bigint', '10 seconds', 'employee',
-- Next is the job that will run every 10 seconds.
$$
UPDATE employee SET new_employee_id=employee_id::bigint
WHERE employee_id IN (SELECT employee_id FROM employee WHERE new_employee_id IS NULL LIMIT 1000);
$$
-- Next is the trigger expression which will be used for INSERTs and UPDATEs
'NEW.new_employee_id=NEW.employee_id::bigint'
);
````

</td>
</tr>

</table>

¹ Postgresql itself requires some SQL to run without explicit transactions; single-transaction application only works when that is not present.  
² There can be false positives and false negatives in some cases.   

## Installing Codd

### 1. Self-contained executable

If you are on x86_64-linux or on aarch64-macOS, the easiest thing is to download our self-contained binaries from [Github Releases](https://github.com/mzabani/codd/releases). If you can't use that, there are two other installation methods, described below.

### 2. Nix

This method will install an executable named `codd` and make it available in your PATH just like installing from a package manager would. It is a bit more cumbersome to install than with docker but easier to use once installed.

1. Install Nix if you don't have it yet by using your package manager or following instructions from https://nixos.org/download.html.
2. Run `sh <(curl -L https://raw.githubusercontent.com/mzabani/codd/master/nix/install-codd.sh)` to install codd. If things are compiling and taking too long, you may want to check if you're a privileged Nix user (otherwise it means our Nix cache is not being used). After installed, just run `codd --help` to invoke it for the first time. To uninstall it, run `nix-env --uninstall codd`.

### 3. Docker

You can find up-to-date images of codd in DockerHub. To run codd through docker just run `docker run --rm mzabani/codd --help`.
Invoking codd this way will often require mounting volumes, specifying UIDs and thus is more bureaucratic than other installation methods.

## Get codd up and running in 15 minutes

Here's a super quick way to get a taste of codd if you have postgres running. Let's first define three required environment variables:

````shell
$ # codd understands URI or keyword value pairs, e.g. dbname=codd_experiments user=postgres host=localhost
$ export CODD_CONNECTION=postgres://postgres@localhost/codd_experiments
$ export CODD_MIGRATION_DIRS=sql-migrations
$ export CODD_EXPECTED_SCHEMA_DIR=expected-schema
````

Make sure you create the `sql-migrations` folder. If you're using docker, it helps to have these environment variables in a _.env_ file.

But the database `codd_experiments` doesn't exist yet, so this connection string will not work. That is not a problem, and we can make codd [create this database](docs/BOOTSTRAPPING.md) for us with a migration that overrides the connection string just for itself.

Create this file and save it as `bootstrap-db.sql`:

````sql
-- codd: no-txn
-- codd-connection: postgres://postgres@localhost/postgres

CREATE DATABASE codd_experiments;
````

That's a lot to take in. Codd handles pure SQL migrations but also has some special header comments defined that can make it do special things.

- The `-- codd: no-txn` header comment specifies that this migration can't run inside a transaction. Postgres doesn't allow us to create databases (plus a few other statements) inside transactions, after all.
- The `-- codd-connection` header comment specifies that this specific migration will run with its own connection string, not with the default one.

You can find more about the special migration directives that codd understands [here](docs/SQL-MIGRATIONS.md#configurability).

Now add this migration by running one of the two commands below:

````shell
$ # If you are using the binary releases or if you installed codd with Nix
$ codd add bootstrap-db.sql

$ # If you're using the docker image with a .env file:
$ docker run --rm -it --env-file .env --network=host --user `id -u`:`id -g` -v "$(pwd):/working-dir" mzabani/codd add bootstrap-db.sql
````

The file should now have been timestamped and moved to the `sql-migrations` folder. The migration ran and so the `codd_experiments` database was created, and schema representation files were written to the `expected-schema` folder.

Optionally, explore the `expected-schema` folder. You won't find much yet, but all the files in there reflect existing database objects. That's how codd knows if schemas in different environments match and also how multiple developers can add migrations and get warned by merge conflicts if any two people modify the same database object.

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
- Read the guides below.

## Guides

### Start using codd with an existing database

If you already have a database and want to start using codd without losing it, read [START-USING.md](docs/START-USING.md).
If you're running codd in multiple environments where connection strings can differ between them, [environment variable templating](docs/SQL-MIGRATIONS.md#templating-environment-variables-into-migrations) might be of assistance.

### Add background migrations to update large datasets without downtime

Codd provides helper functions, and with the help of the [pg_cron](https://github.com/citusdata/pg_cron) extension (or a background job/SQL runner of your own implementation) it can be much simpler to apply such migration. [Read the guide](docs/BACKGROUND-MIGRATIONS.md).

### Codd's limits and robustness

- You can have arbitrarily large migrations. Even pg_dump files can be migrations. Codd parses migrations from disk in streaming fashion and keeps in memory only a constant number of SQL statements at a time. For `COPY` statements, codd uses a constant-size buffer to stream-read the contents and achieve bounded memory usage while staying fast; [nearly as fast as psql](https://mzabani.github.io/posts/2025-02-12-concurrently-forcing-haskell-streams.html). Codd's CI includes regression tests and benchmarks both for wall-clock performance and maximum memory usage.
- You should be able to have thousands of migration files. Codd keeps only file names and other migration metadata in memory (but write a feature request if you'd like that to be improved!) and only opens one file at a time to to stay well below typical file handle limits imposed by the shell or operating system (MacOS shells have particularly low limits). Codd's CI includes an strace-based test to ensure this stays that way.
- Codd will apply all pending migrations in the same transaction unless you add `-- codd: no-txn` to some of your migrations.
- Codd has a default retry policy with a backoff mechanism. Check [CONFIGURATION.md](docs/CONFIGURATION.md) to configure it yourself if you prefer.
- Codd cannot apply psql's meta commands (those starting with a backslash), but you should otherwise be able to write arbitrary SQL in your migrations. You can e.g. write your own `BEGIN; ... COMMIT;` sections in migrations with `-- codd: no-txn`, and codd will be smart enough to retry those migrations if they fail inside those transaction from the `BEGIN` statement (and as you'd expect, we test this in our CI pipeline). See the [retry examples](/docs/SQL-MIGRATIONS.md#examples) if you're interested.
- Read about the kinds of schema equality that codd **cannot verify** in [DATABASE-EQUALITY.md](docs/DATABASE-EQUALITY.md). This will also give you another idea about how far codd is willing to go to ensure your schema is the same across environments.  

The author's intention is that you should be able to write SQL fearlessly and count on codd to be reliable and to behave reasonably even in edge cases. Please file a report if you think it's not keeping that promise.

## Frequently Asked Questions

1. ### Why does taking and restoring a database dump affect my expected codd schema?
   `pg_dump` does not dump all of the schema state that codd checks. A few examples include role related state, the database's default transaction isolation level and deferredness, among other settings. So check that it isn't the case that you get different schemas when that happens. PostgreSQL can also some times have its internal parsing+deparsing process not be an identity. If you've checked with `psql` and everything looks to be the same please report a bug in codd.
