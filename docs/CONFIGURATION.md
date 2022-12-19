## Configuring Codd

This is a complete list of environment variables _codd_ understands. The only mandatory ones are `CODD_CONNECTION`, `CODD_MIGRATION_DIRS` and `CODD_EXPECTED_SCHEMA_DIR`.

````bash
# A connection string in the format postgres://username[:password]@host:port/database_name
# or as keyword value pairs, e.g. dbname=database_name user=postgres host=localhost
# This is a libpq/psql compatible connection string that codd will use to apply migrations.
CODD_CONNECTION=postgres://postgres@127.0.0.1:5432/postgres

# A list of directories where SQL migration files will be found/added to. Do note that you
# can have e.g. a testing environment with an extra folder for itself to hold data migrations
# you don't want on Production. It's recommended to always have your "all migrations" folder first.
CODD_MIGRATION_DIRS=sql-migrations/all:sql-migrations/dev-only

# Path to a folder where files will be created with representations of DB objects. This folder will be
# wiped clean by codd every time it writes schema files. It can be a relative folder.
CODD_EXPECTED_SCHEMA_DIR=expected-schema

# Space separated schemas for codd to check. If this variable is not set all schemas except for
# postgres's internal ones will be included. If defined and empty no schemas will be included.
CODD_SCHEMAS=public

# Optional, space separated roles for codd to check.
# Note that the role in CODD_CONNECTION is always checked regardless of this.
CODD_EXTRA_ROLES=codd-user

# Optional transaction isolation level for codd. By default, codd uses the
# default isolation level in the DB, but always in READ WRITE mode so you can
# set the default to be read only and still use codd.
# Choose "db-default|serializable|repeatable read|read committed|read uncommitted"
# or don't set it to get 'db-default'
CODD_TXN_ISOLATION=db-default

# Optional retry policy.
# Migrations can fail due to temporary errors, so Codd retries up to 2 times by default
# when migrations fail (to a total of 3 tries), but you can control that with this variable.
# Its format is "max MAXRETRIES backoff (constant|exponential) TIME(s|ms)"
CODD_RETRY_POLICY=max 2 backoff exponential 1.5s
````
