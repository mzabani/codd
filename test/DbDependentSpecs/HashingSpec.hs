module DbDependentSpecs.HashingSpec where

import           Codd                           ( applyMigrations )
import           Codd.Analysis                  ( DestructiveSectionCheck(..)
                                                , MigrationCheck(..)
                                                , NonDestructiveSectionCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..)
                                                , superUserInAppDatabaseConnInfo
                                                )
import           Codd.Hashing                   ( DiffType(..)
                                                , hashDifferences
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Internal                  ( withConnection )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                , parsedSqlText
                                                , toMigrationTimestamp
                                                )
import           Control.Monad                  ( foldM
                                                , foldM_
                                                , void
                                                , when
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Data.List                      ( nubBy )
import qualified Data.Map                      as Map
import           Data.Text                      ( unpack )
import           Data.Time.Calendar             ( fromGregorian )
import           Data.Time.Clock                ( UTCTime(..) )
import qualified Database.PostgreSQL.Simple    as DB
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import           DbUtils                        ( aroundFreshDatabase
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                )
import           Test.Hspec
import           UnliftIO.Concurrent            ( threadDelay )

data DbChange = ChangeEq [(FilePath, DiffType)] | SomeChange

migrationsAndHashChange :: [(AddedSqlMigration, DbChange)]
migrationsAndHashChange = zipWith
  (\(t, c) i ->
    ( AddedSqlMigration
      SqlMigration { migrationName       = show i <> "-migration.sql"
                   , nonDestructiveSql   = Just $ mkValidSql t
                   , nonDestructiveForce = True
                   , nonDestructiveInTxn = True
                   , destructiveSql      = Nothing
                   , destructiveInTxn    = True
                   }
      (getIncreasingTimestamp i)
    , c
    )
  )
  migs
  (map fromInteger [0 ..]) -- This would be a list of NominalDiffTime, which would have 10^-12s resolution and fail in the DB
 where
  migs =
    [
      -- MISSING:
      -- COLUMNS WITH GENERATED AS
      -- EXCLUSION CONSTRAINTS
      -- COLLATIONS
      -- EXTENSIONS
      -- PARTITIONING
      -- TYPES
      -- LANGUAGES
      -- FOREIGN SERVERS
      -- DOMAINS
      -- TABLESPACES



      -- TABLES AND COLUMNS
      ( "CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name TEXT)"
      , ChangeEq
        [ ("schemas/public/sequences/employee_employee_id_seq", OnlyRight)
        , ("schemas/public/tables/employee/cols/employee_id"  , OnlyRight)
        , ("schemas/public/tables/employee/cols/employee_name", OnlyRight)
        , ( "schemas/public/tables/employee/constraints/employee_pkey"
          , OnlyRight
          )
        , ("schemas/public/tables/employee/indexes/employee_pkey", OnlyRight)
        , ("schemas/public/tables/employee/objhash"              , OnlyRight)
        ]
      )
    , ( "ALTER TABLE employee ALTER COLUMN employee_name SET NOT NULL"
      , ChangeEq
        [ ( "schemas/public/tables/employee/cols/employee_name"
          , BothButDifferent
          )
        ]
      )
    , ( "ALTER TABLE employee ADD COLUMN birthday DATE; ALTER TABLE employee ADD COLUMN deathday DATE;"
      , ChangeEq
        [ ("schemas/public/tables/employee/cols/birthday", OnlyRight)
        , ("schemas/public/tables/employee/cols/deathday", OnlyRight)
        ]
      )
      -- Column order matters because of things like 'SELECT *'
    , ( "ALTER TABLE employee DROP COLUMN birthday; ALTER TABLE employee DROP COLUMN deathday;"
        <> "ALTER TABLE employee ADD COLUMN deathday DATE; ALTER TABLE employee ADD COLUMN birthday DATE;"
      , ChangeEq
        [ ("schemas/public/tables/employee/cols/birthday", BothButDifferent)
        , ("schemas/public/tables/employee/cols/deathday", BothButDifferent)
        ]
      )
    , ( "ALTER TABLE employee ALTER COLUMN birthday TYPE TIMESTAMP;"
      , ChangeEq
        [("schemas/public/tables/employee/cols/birthday", BothButDifferent)]
      )
    , ( "ALTER TABLE employee ADD COLUMN IF NOT EXISTS birthday TIMESTAMP;"
      , ChangeEq []
      )
    , ( "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';"
      , ChangeEq
        [("schemas/public/tables/employee/cols/deathday", BothButDifferent)]
      )
    , ( "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';"
      , ChangeEq []
      )
    , ( "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-04';"
      , ChangeEq
        [("schemas/public/tables/employee/cols/deathday", BothButDifferent)]
      )

    -- Recreating a column exactly like it was before will affect column order, which will affect the index too (sadly for now),
    -- but the sequence should not be affected
    , ( "ALTER TABLE employee DROP COLUMN employee_id; ALTER TABLE employee ADD COLUMN employee_id SERIAL PRIMARY KEY;"
      , ChangeEq
        [ ("schemas/public/tables/employee/cols/employee_id", BothButDifferent)
        , ( "schemas/public/tables/employee/constraints/employee_pkey"
          , BothButDifferent
          )
        , ("schemas/public/sequences/employee_employee_id_seq",BothButDifferent) -- This change happens because due to sequence ownership, we need to
        -- either include the owner column's name or its attnum. We chose the latter thinking it's more common case to rename columns than change
        -- their relative positions.
        ]
      )


      -- SEQUENCES
    , ( "CREATE SEQUENCE some_seq MINVALUE 1 MAXVALUE 100"
      , ChangeEq [("schemas/public/sequences/some_seq", OnlyRight)]
      )
      -- MINVALUE and MAXVALUE that fit other types so we are sure changing just the seq. type has an effect
    , ( "ALTER SEQUENCE some_seq AS smallint"
      , ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
      )
    , ( "ALTER SEQUENCE some_seq AS integer"
      , ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
      )
    , ( "ALTER SEQUENCE some_seq START WITH 3"
      , ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
      )
    , ("ALTER SEQUENCE some_seq RESTART WITH 7", ChangeEq [])
      -- TODO: Where can I find in pg_catalog the restart_with value? Currently it does not affect hashing, sadly.
    , ( "ALTER SEQUENCE some_seq MINVALUE 2"
      , ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
      )
    , ( "ALTER SEQUENCE some_seq MAXVALUE 99999"
      , ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
      )
    , ( "ALTER SEQUENCE some_seq INCREMENT BY 2"
      , ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
      )
    , ( "ALTER SEQUENCE some_seq CYCLE"
      , ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
      )
    , ( "ALTER SEQUENCE some_seq CACHE 2"
      , ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
      )
    , ("ALTER SEQUENCE some_seq OWNED BY employee.employee_id", ChangeEq [("schemas/public/sequences/some_seq",BothButDifferent)])

      -- CHECK CONSTRAINTS
    , ( "ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')"
      , ChangeEq
        [ ( "schemas/public/tables/employee/constraints/employee_ck_name"
          , OnlyRight
          )
        ]
      )
    , ( "ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')"
      , ChangeEq []
      )
    , ( "ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> 'EMPTY')"
      , ChangeEq
        [ ( "schemas/public/tables/employee/constraints/employee_ck_name"
          , BothButDifferent
          )
        ]
      )

      -- FOREIGN KEYS
    , ( "CREATE TABLE employee_car (employee_id INT NOT NULL, car_model TEXT NOT NULL)"
      , ChangeEq
        [ ("schemas/public/tables/employee_car/cols/car_model"  , OnlyRight)
        , ("schemas/public/tables/employee_car/cols/employee_id", OnlyRight)
        , ("schemas/public/tables/employee_car/objhash"         , OnlyRight)
        ]
      )
    , ( "CREATE TABLE employee_computer (employee_id INT NOT NULL, computer_model TEXT NOT NULL, UNIQUE (employee_id))"
      , ChangeEq
        [ ( "schemas/public/tables/employee_computer/cols/computer_model"
          , OnlyRight
          )
        , ( "schemas/public/tables/employee_computer/cols/employee_id"
          , OnlyRight
          )
        , ( "schemas/public/tables/employee_computer/constraints/employee_computer_employee_id_key"
          , OnlyRight
          )
        , ( "schemas/public/tables/employee_computer/indexes/employee_computer_employee_id_key"
          , OnlyRight
          )
        , ("schemas/public/tables/employee_computer/objhash", OnlyRight)
        ]
      )
    , ( "ALTER TABLE employee_car ADD CONSTRAINT employee_car_employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)"
      , ChangeEq
        [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk"
          , OnlyRight
          )
        ]
      )
    , ( "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY DEFERRED"
      , ChangeEq
        [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk"
          , BothButDifferent
          )
        ]
      )
    , ( "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY IMMEDIATE"
      , ChangeEq
        [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk"
          , BothButDifferent
          )
        ]
      )
    , ( "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk NOT DEFERRABLE"
      , ChangeEq
        [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk"
          , BothButDifferent
          )
        ]
      )
    , ( "ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)"
      , ChangeEq
        [ ( "schemas/public/tables/employee_car/constraints/employee__employee_fk"
          , OnlyRight
          )
        ]
      )
      -- Same FK on the same table and column, referencing a different table, but with the same referenced column name as before.
    , ( "ALTER TABLE employee_car DROP CONSTRAINT employee__employee_fk; ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee_computer(employee_id)"
      , ChangeEq
        [ ( "schemas/public/tables/employee_car/constraints/employee__employee_fk"
          , BothButDifferent
          )
        ]
      )

      -- UNIQUE CONSTRAINTS AND INDICES
    , ( "ALTER TABLE employee ADD CONSTRAINT unique_employee UNIQUE(employee_name)"
      , SomeChange
      )
    , ( "ALTER TABLE employee RENAME CONSTRAINT unique_employee TO employee_unique_name"
      , ChangeEq
        [ ( "schemas/public/tables/employee/constraints/employee_unique_name"
          , OnlyRight
          )
        , ( "schemas/public/tables/employee/constraints/unique_employee"
          , OnlyLeft
          )
        , ( "schemas/public/tables/employee/indexes/employee_unique_name"
          , OnlyRight
          )
        , ("schemas/public/tables/employee/indexes/unique_employee", OnlyLeft)
        ]
      )
    , ( "CREATE UNIQUE INDEX unique_employee_idx ON employee (employee_name)"
      , ChangeEq
        [ ( "schemas/public/tables/employee/indexes/unique_employee_idx"
          , OnlyRight
          )
        ]
      )

      -- FUNCTIONS
    , ( "CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$"
        <> "BEGIN  \n RETURN i + 1;  \n END;  \n $$ LANGUAGE plpgsql;"
      , ChangeEq [("schemas/public/routines/increment;{int4}", OnlyRight)]
      )
    , ( "CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$"
        <> "BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
      , ChangeEq
        [("schemas/public/routines/increment;{int4}", BothButDifferent)]
      )
      -- Change in function args means new function
    , ( "CREATE OR REPLACE FUNCTION increment(i integer, x text) RETURNS integer AS $$"
        <> "BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
      , ChangeEq [("schemas/public/routines/increment;{int4,text}", OnlyRight)]
      )
        -- Change in function args means new function
    , ( "CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS integer AS $$"
        <> "BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
      , ChangeEq [("schemas/public/routines/increment;{text,int4}", OnlyRight)]
      )
        -- Same everything as existing function, just changing return type
    , ( "DROP FUNCTION increment(text, integer); CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS bigint AS $$"
        <> "BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
      , ChangeEq
        [("schemas/public/routines/increment;{text,int4}", BothButDifferent)]
      )

      -- TRIGGERS
    , ("ALTER TABLE employee ADD COLUMN name TEXT", SomeChange)
    , ( "CREATE FUNCTION employee_name_rename_set_new() RETURNS TRIGGER AS $$\n"
        <> "BEGIN\n NEW.name = NEW.employee_name;\n RETURN NEW;\n END\n $$ LANGUAGE plpgsql;"
      , SomeChange
      )
    , ( "CREATE TRIGGER employee_old_app_update_column_name"
        <> "\n BEFORE UPDATE ON employee"
        <> "\n FOR EACH ROW"
        <> "\n WHEN (OLD.employee_name IS DISTINCT FROM NEW.employee_name)"
        <> "\n EXECUTE PROCEDURE employee_name_rename_set_new()"
      , ChangeEq
        [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name"
          , OnlyRight
          )
        ]
      )

    -- No WHEN in the recreated trigger
    , ( "DROP TRIGGER employee_old_app_update_column_name ON employee; CREATE TRIGGER employee_old_app_update_column_name"
        <> "\n BEFORE UPDATE ON employee"
        <> "\n FOR EACH ROW"
        <> "\n EXECUTE PROCEDURE employee_name_rename_set_new()"
      , ChangeEq
        [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name"
          , BothButDifferent
          )
        ]
      )
    , ( "DROP TRIGGER employee_old_app_update_column_name ON employee"
      , ChangeEq
        [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name"
          , OnlyLeft
          )
        ]
      )
    , ("ALTER TABLE employee DROP COLUMN employee_name", SomeChange)
    , ("ALTER TABLE employee RENAME COLUMN name TO employee_name", SomeChange)

      -- VIEWS
    , ( "CREATE OR REPLACE VIEW all_employee_names (employee_name) AS (SELECT employee_name FROM employee)"
      , ChangeEq [("schemas/public/views/all_employee_names", OnlyRight)]
      )
    , ( "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT employee_name FROM employee)"
      , ChangeEq [("schemas/public/views/all_employee_names", BothButDifferent)]
      )
    , ( "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT employee_name FROM employee)"
      , ChangeEq []
      )
    , ( "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT 'Mr. ' || employee_name FROM employee)"
      , ChangeEq [("schemas/public/views/all_employee_names", BothButDifferent)]
      )
    , ( "ALTER VIEW all_employee_names OWNER TO \"codd-test-user\""
      , ChangeEq [("schemas/public/views/all_employee_names", BothButDifferent)]
      )

      -- ROW LEVEL SECURITY
    , ( "ALTER TABLE employee ENABLE ROW LEVEL SECURITY"
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "ALTER TABLE employee FORCE ROW LEVEL SECURITY"
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "ALTER TABLE employee NO FORCE ROW LEVEL SECURITY"
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "CREATE POLICY some_policy ON employee USING (employee_name <> 'Some Name');"
      , ChangeEq
        [("schemas/public/tables/employee/policies/some_policy", OnlyRight)]
      )
    , ( "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee USING (employee_name <> 'Some Other Name');"
      , ChangeEq
        [ ( "schemas/public/tables/employee/policies/some_policy"
          , BothButDifferent
          )
        ]
      )
    , ( "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name');"
      , ChangeEq
        [ ( "schemas/public/tables/employee/policies/some_policy"
          , BothButDifferent
          )
        ]
      )
    , ( "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name') WITH CHECK (TRUE);"
      , ChangeEq
        [ ( "schemas/public/tables/employee/policies/some_policy"
          , BothButDifferent
          )
        ]
      )
    , ( "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name') WITH CHECK (TRUE);"
      , ChangeEq []
      )
    , ( "DROP POLICY some_policy ON employee;"
      , ChangeEq
        [("schemas/public/tables/employee/policies/some_policy", OnlyLeft)]
      )

      -- ROLES
      -- Unmapped Roles are not hashed
    , ("CREATE ROLE any_new_role", ChangeEq [])
    , ("DROP ROLE any_new_role", ChangeEq [])
    , ( "CREATE ROLE \"extra-codd-test-user\""
      , ChangeEq [("roles/extra-codd-test-user", OnlyRight)]
      )
    , ( "ALTER ROLE \"codd-test-user\" SET search_path TO public, pg_catalog"
      , ChangeEq [("roles/codd-test-user", BothButDifferent)]
      )
    , ( "ALTER ROLE \"codd-test-user\" WITH BYPASSRLS; ALTER ROLE \"codd-test-user\" WITH REPLICATION; "
      , ChangeEq [("roles/codd-test-user", BothButDifferent)]
      )
    , ("ALTER ROLE \"codd-test-user\" WITH BYPASSRLS", ChangeEq [])
    , ( "REVOKE CONNECT ON DATABASE \"codd-test-db\" FROM \"codd-test-user\""
      , ChangeEq [("roles/codd-test-user", BothButDifferent)]
      )
    , ( "ALTER ROLE postgres SET search_path TO public, pg_catalog"
      , ChangeEq [("roles/postgres", BothButDifferent)]
      )
    , ( "ALTER ROLE \"codd-test-user\" SET search_path TO DEFAULT"
      , ChangeEq [("roles/codd-test-user", BothButDifferent)]
      )
    , ( "ALTER ROLE postgres SET search_path TO DEFAULT"
      , ChangeEq [("roles/postgres", BothButDifferent)]
      )
    , ("ALTER ROLE postgres SET search_path TO DEFAULT", ChangeEq [])

      -- PERMISSIONS
      -- For tables
    , ( "GRANT ALL ON TABLE employee TO \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "REVOKE ALL ON TABLE employee FROM \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "GRANT SELECT ON TABLE employee TO \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "GRANT INSERT ON TABLE employee TO \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "GRANT DELETE ON TABLE employee TO \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )

      -- For sequences
    , ( "REVOKE ALL ON SEQUENCE employee_employee_id_seq FROM \"codd-test-user\""
      , ChangeEq
        [ ( "schemas/public/sequences/employee_employee_id_seq"
          , BothButDifferent
          )
        ]
      )
    , ( "GRANT SELECT ON SEQUENCE employee_employee_id_seq TO \"codd-test-user\""
      , ChangeEq
        [ ( "schemas/public/sequences/employee_employee_id_seq"
          , BothButDifferent
          )
        ]
      )

      -- Order of granting does not matter, nor do grantors
    , ( "REVOKE ALL ON TABLE employee FROM \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "GRANT INSERT ON TABLE employee TO \"codd-test-user\"; GRANT DELETE ON TABLE employee TO \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT DELETE ON TABLE employee TO \"codd-test-user\"; GRANT INSERT ON TABLE employee TO \"codd-test-user\""
      , ChangeEq []
      )
    , ( "GRANT ALL ON TABLE employee TO \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"extra-codd-test-user\"; GRANT ALL ON TABLE employee TO \"codd-test-user\""
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )
    , ( "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"extra-codd-test-user\""
      , ChangeEq []
      )
    , ( "GRANT ALL ON TABLE employee TO PUBLIC"
      , ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]
      )

      -- Permissions of unmapped role don't affect hashing
      -- TODO: Views not working yet for some reason
    , ( "CREATE ROLE unmapped_role1; GRANT ALL ON TABLE employee TO unmapped_role1; GRANT ALL ON SEQUENCE employee_employee_id_seq TO unmapped_role1; -- GRANT ALL ON all_employee_names TO unmapped_role1"
      , ChangeEq []
      )
    , ("DROP OWNED BY unmapped_role1; DROP ROLE unmapped_role1", ChangeEq [])

      -- CREATING UNMAPPED AND MAPPED SCHEMAS
    , ("CREATE SCHEMA unmappedschema", ChangeEq [])
    , ("DROP SCHEMA unmappedschema", ChangeEq [])
    , ( "CREATE SCHEMA \"codd-extra-mapped-schema\""
      , ChangeEq [("schemas/codd-extra-mapped-schema/objhash", OnlyRight)]
      )
    , ( "DROP SCHEMA \"codd-extra-mapped-schema\""
      , ChangeEq [("schemas/codd-extra-mapped-schema/objhash", OnlyLeft)]
      )


      -- CRUD
    , ("INSERT INTO employee (employee_name) VALUES ('Marcelo')", ChangeEq [])
    ]

spec :: Spec
spec = do
  let mkDbInfo baseDbInfo migs = baseDbInfo { sqlMigrations = Right migs }
  describe "DbDependentSpecs" $ do
    describe "Hashing tests" $ do
      aroundFreshDatabase $ it "Checksumming schema changes" $ \emptyDbInfo ->
        do
          let connInfo = superUserInAppDatabaseConnInfo emptyDbInfo
              getHashes sett = runStdoutLoggingT $ withConnection
                connInfo
                (readHashesFromDatabaseWithSettings sett)
          hashBeforeEverything <- getHashes emptyDbInfo
          foldM_
            (\(hashSoFar, appliedMigs :: [AddedSqlMigration]) (nextMig, expectedChanges) ->
              do
                let newMigs = appliedMigs ++ [nextMig]
                    dbInfo  = emptyDbInfo { sqlMigrations = Right newMigs }
                runStdoutLoggingT $ applyMigrations dbInfo False
                dbHashesAfterMig <- getHashes dbInfo
                let migText = parsedSqlText
                      <$> nonDestructiveSql (addedSqlMig nextMig)
                    diff = hashDifferences hashSoFar dbHashesAfterMig
                case expectedChanges of
                  ChangeEq c -> do
                    (migText, diff) `shouldBe` (migText, Map.fromList c)
                    -- The check below is just a safety net in case "hashDifferences" has a problem in its implementation
                    if null c
                      then hashSoFar `shouldBe` dbHashesAfterMig
                      else hashSoFar `shouldNotBe` dbHashesAfterMig
                  SomeChange -> do
                    (migText, diff) `shouldNotBe` (migText, Map.empty)
                    -- The check below is just a safety net in case "hashDifferences" has a problem in its implementation
                    hashSoFar `shouldNotBe` dbHashesAfterMig

                return (dbHashesAfterMig, newMigs)
            )
            (hashBeforeEverything, [])
            migrationsAndHashChange

          -- Let's make sure we're actually applying the migrations by fetching some data..
          withConnection
              connInfo
              (\conn -> DB.query
                conn
                "SELECT employee_id, employee_name FROM employee WHERE employee_name='Marcelo'"
                ()
              )
            `shouldReturn` [(1 :: Int, "Marcelo" :: String)]
