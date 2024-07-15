## Start using Codd in an existing database

If you already have a Database and would like to start using codd, here's a guideline to approach the problem. Remember to be very careful if/when making any changes to your Prod DB:

1. Configure your environment variables as explained in the [README](../README.md) and in [CONFIGURATION.md](CONFIGURATION.md).
2. In that configuration make sure you have that extra `dev-only` folder to hold SQL migrations that will only run in developers' machines.
3. Run `pg_dump your_database > dump-migration.sql` **locally**. Do not use `pg_dumpall` because it includes _psql_'s meta-commands that codd doesn't support.
4. Run `dropdb your_database` to drop your DB **locally**.
5. Add a bootstrap migration similar to the one exemplified in [BOOTSTRAPPING.md](BOOTSTRAPPING.md), but with ownership, encoding and locale equal to your Production DB's. The database's and the _public_'s Schema ownership might need some manual intervention to match in different environments.
   - **What do we mean?** Cloud services such as Amazon's RDS will create Schemas and DBs owned by users managed by them - such as the `rdsadmin` user -, that we don't usually replicate locally. We can either replicate these locally so we don't need to touch our Prod DB or change our Prod DB so only users managed by us are ever referenced in any environment.
6. Make sure the bootstrap migrations added in the previous step create the database, roles and ownership match what you get in Production.
   - Use _psql_'s `\dg` to view roles in your Prod DB.
   - Use _psql_'s `\l` to check DB ownership and permissions of your Prod DB.
   - Use _psql_'s `\dn+` to check the _public_ schema's ownership and permissions in your Prod DB.
Once your bootstrapping migration is ready, run `codd add bootstrap-migration.sql --dest-folder your-dev-only-folder`. This will create your database with no tables or data in it.
7. Run `codd add dump-migration.sql --dest-folder your-dev-only-folder`. Dumps can some times fail to be applied due to privileges being enforced by postgresql itself, so make sure to edit and change the dump file accordingly so that it can be applied. This often means adding a custom `-- codd-connection` comment on top to make it run as a privileged enough user, like the `postgres` user.
8. You should now have your database back and managed through codd.
9. Make sure your Production environment variable `CODD_MIGRATION_DIRS` does not contain your `dev-only` folder. Add any future SQL migrations to your `all-migrations` folder.
10. Before deploying with codd, we strongly recommend you run `codd verify-schema` with your environment variables connected to your Production database and make sure schemas match.
11. In Production, we strongly recommend running `codd up --lax-check` (the default) to start with until you get acquainted enough to consider strict-checking. Make sure you read `codd up --help` to better understand your options.
