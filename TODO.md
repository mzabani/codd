### TODO

- Don't expose SqlMigration(..), because to create it the SQL must go through the Parser to detect empty queries and Analysis to detect in-txn which don't work..
   Or do we allow those as runtime errors?