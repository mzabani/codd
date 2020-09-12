# TODO

- Don't expose SqlMigration(..), because to create it the SQL must go through the Parser to detect empty queries and Analysis to detect in-txn which don't work..
   Or do we allow those as runtime errors?
- CLI
- Better Logging options
- When applying migrations, query DB for all fully-applied migrations first, so that those don't need to be parsed.
- Do we create the App's User if it does not exist? We probably should.
- Failed test:
     To rerun use: --match "/Parsing/Parsing tests/Valid SQL Migrations/Sql Migration with one section, missing optional options/"
     Randomized with seed 910382880