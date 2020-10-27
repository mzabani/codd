# What is Codd?

Codd is a tool to help teams of Developers version-control their Databases locally and for Deployment. It provides a few main features:

- A way to apply pending SQL migrations in a single DB Transaction when possible.  
- A one-file-per-database-object approach to checksum your Database, helping minimize chances that your Production Database differs from your Development Database and ensuring that Merge Conflicts only arise when two developers alter the same Database object.  
- Other nice features, such as SQL destructiveness detection and destructive/non-destructive section separation to help those who are willing to pay development/effort costs to have near-zero downtime during deployments.  
- **It is only compatible with PostgreSQL >= 10**

## Installing Codd

TODO

## Configuring Codd

Codd will hash DB Objects to ensure Database-equality between different environments such as Development and Production. But we have to first set it up to let it know which top-level objects - such as schemas and roles - it will consider, and connection strings for it to connect.

Let's take a look at an example `.env` file for Codd. These environment variables must be defined when running the `codd` executable.

````.env
# A connection string in the format postgres://username[:password]@host:port/database_name
# This connection string must be for an ADMIN user
ADMIN_DATABASE_URL=postgres://postgres@127.0.0.1:5433/postgres

# The name of the Database the App uses
APP_DATABASE=codd-experiments

# The DB Username the App uses
APP_USERNAME=codd-user

# A list of directories where SQL migration files will be found/added to. Do note that you can have e.g. a testing environment with an extra folder
# for itself to hold data migrations you don't want on Production
SQL_MIGRATION_PATHS=sql-migrations/all:sql-migrations/dev

# Folder where files will be created with checksums of DB objects. This folder will be wiped clean by codd every time it's necessary
DB_ONDISK_HASHES=sql-migrations/on-disk-hashes

# Space separated schemas to hash
CODD_SCHEMAS=public

# Space separated roles other than the ones specified above that must also be considered
CODD_EXTRA_ROLES=pg_monitor pg_stat_scan_tables
````

## Adding a Migration

[![asciicast](https://asciinema.org/a/wTdnsKvPV6rl9LTGC8B2pICuC.svg)](https://asciinema.org/a/wTdnsKvPV6rl9LTGC8B2pICuC)