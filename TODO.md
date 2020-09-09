# TODO

- Don't expose SqlMigration(..), because to create it the SQL must go through the Parser to detect empty queries and Analysis to detect in-txn which don't work..
   Or do we allow those as runtime errors?

- Postgres v10 not supported currently

- Failed test:
  To rerun use: --match "/Parsing/Parsing tests/Valid SQL Migrations/Sql Migration with one section, missing optional options/"

Randomized with seed 910382880
