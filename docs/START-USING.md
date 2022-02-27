## Start using Codd in an existing database

If you already have a Database and would like to start using _codd_, here's a guideline to approach the problem. Remember to be very careful if/when making any changes to your Prod DB:

1. Configure your `.env` file as explained in the [README](../README.md).
2. In that configuration make sure you have that extra `dev-only` folder to hold SQL migrations that will only run in developers' machines.
3. Run `pg_dump your_database > dump-migration.sql`. Do not use `pg_dumpall` because it includes _psql_'s meta-commands that _codd_ doesn't support.
4. Run `dropdb your_database` to drop your DB **locally**.
5. Add a bootstrap migration similar to the one exemplified in [BOOTSTRAPPING.md](BOOTSTRAPPING.md), but with encoding and locale equal to your Production DB's. The database's and the _public_'s Schema ownership might need some manual intervention to match in different environments.
   - **What do we mean?** Cloud services such as Amazon's RDS will create Schemas and DBs owned by users managed by them - such as the `rdsadmin` user -, that we don't usually replicate locally. We can either replicate these locally so we don't need to touch our Prod DB or change our Prod DB so only users managed by us are ever referenced in any environment.
6. Make sure the bootstrap migrations added in the previous step create the database, roles and ownership match what you get in Production.
   - Use _psql_'s `\dg` to view roles in your Prod DB.
   - Use _psql_'s `\l` to check DB ownership and permissions of your Prod DB.
   - Use _psql_'s `\dn+` to check the _public_ schema's ownership and permissions in your Prod DB.
7. Edit `dump-migration.sql` (created in step 3) and add `-- codd: no-txn` as its very first line.
8.  Run `codd add dump-migration.sql --dest-folder your-dev-only-folder`
9.  You should now have your database back and managed through _codd_.
10. Make sure your separate Production `.env` file does not contain your `dev-only` folder. Add any future SQL migrations to your `all-migrations` folder.
11. Before deploying with _codd_, we strongly recommend you run `codd verify-checksums` with your environment variables connected to your Production database and make sure checksums match.
12. In Production, we strongly recommend running `codd up --lax-check` to start with until you get acquainted enough to consider strict-checking. Make sure you read `codd up --help` to better understand your options.