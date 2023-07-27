<!-- This file is referenced from error messages codd produces, so don't move it or rename it. -->
# Bootstrapping

_Codd_ can apply migrations but it needs an existing database to apply them on.
Custom scripts can help with that initial phase, but _codd_ has its own solution to that, helping save
a few keystrokes and making the entire process more streamlined; it's called bootstrapping.

## How does it work?

Creating a database usually involves first connecting to an existing database and issuing a `CREATE DATABASE my_database` statement, followed by many other statements that only privileged users can run.
After that migrations can usually be applied by either the same privileged user or some other role on the target database, but the very first statements must at least run on a non-target database such as `postgres`, or others that already existed.

With that in mind, with _codd_ you just have to add a migration like almost any other, except you want it to have a custom connection string, e.g.:


````sql
-- codd: no-txn
-- codd-connection: postgres://postgres@localhost:5432/postgres

DO
$do$
BEGIN
   IF NOT EXISTS (
      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'my_admin_user') THEN
      CREATE USER my_admin_user WITH CREATEROLE;
   END IF;
END
$do$;

CREATE DATABASE my_database TEMPLATE template1 OWNER my_admin_user;
````

Now just make sure this is the very first migration to run - you can move it to your "dev-only" folder of migrations (the one you'd run in a Production server would likely be very different) with a very early date. One suitable named would be something like `1900-01-01-00-00-00-bootstrap-db.sql`, for example.

This can help you recreate your local DB as easily as `dropdb my_database && codd up`, and keeps all necessary statements to create it in folders familiar to you.

## Tips
- Check that ownership, encoding and locale of the created database matches across different environments. Especially if you're using Cloud SQL, this part can be tricky to get right.
- You can specify a custom `-- codd-connection` for _any_ migration. Of course, different connection strings break transactional guarantees created by only having single-connection in-txn migrations, so use this carefully.