# TODO

- Check hashes after applying migrations on deployment and fail if they don't match!!
- Don't expose SqlMigration(..), because to create it the SQL must go through the Parser to detect empty queries and Analysis to detect in-txn which don't work..
   Or do we allow those as runtime errors?
- CLI
- Better Logging options
- When applying migrations, query DB for all fully-applied migrations first, so that those don't need to be parsed.
- Do we create the App's User if it does not exist? We probably should.
- HOOGLE Docs Generation + nice docs
- Failed test:
     To rerun use: --match "/Parsing/Parsing tests/Valid SQL Migrations/Sql Migration with one section, missing optional options/"
     Randomized with seed 910382880
- Review non-destructive, both destructive and non-destructive workflows
- More complex login mechanisms: what if the App User has a password, or authenticates by a different mechanism??
- Tests for the App itself, not just the library