# Bootstrapping

_Codd_ is really good at applying migrations but it needs an existing database to apply them on.
Custom scripts can help with that initial phase, but _codd_ has its own solution to that, helping save
a few keystrokes and making the entire process more streamlined; it's called bootstrapping.

## How does it work?

Creating a database usually involves first connecting to the database and issuing a `CREATE DATABASE my_database` statement, followed by many other statements that only privileged users can run.
After that migrations can usually be applied by either the same privileged user on the target database, but the very first statements must at least run on a non-target database such as `postgres`, or others that already existed.

With that in mind, with _codd_ you just have to add a migration like almost any other, except you want it to have a custom connection string, e.g.:


````sql
-- codd: no-txn
-- codd-connection: postgres://postgres@localhost:5433/postgres

DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd_admin') THEN
      CREATE USER codd_admin WITH CREATEROLE;
   END IF;
END
$do$;

CREATE DATABASE codd_experiments TEMPLATE template0 OWNER codd_admin ENCODING UTF8 LC_COLLATE "en_GB.UTF8" LC_CTYPE "en_GB.UTF8";
````

Now just make sure this is the very first migration to run - you can move it to your "dev-only" folder of migrations (the one you'd run in a Production server would likely be very different) with a very early date. One suitable named would be something like `1900-01-01T00:00:00Z-bootstrap-db.sql`, for example.

This can help you recreate your local DB as easily as `dropdb codd_experiments && codd up`, and keeps all necessary statements to create it in folders familiar to you.

## Tips
- The long `CREATE DATABASE` statement that specifies so many things might seem overkill, but it helps ensure the exact same database is created even in different installations of postgres. _Codd_ will checksum these attributes so it may accuse differences if you're not very specific here.
- You can specify a custom `-- codd-connection` for _any_ migration. Of course, different connection strings break transactional guarantees created by only having single-connection in-txn migrations, so be use this carefully.