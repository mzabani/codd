module DbDependentSpecs.HashingSpec where

import           Codd                           ( ApplyResult(..)
                                                , CheckHashes(..)
                                                , ChecksumsPair(..)
                                                , applyMigrations
                                                , applyMigrationsNoCheck
                                                )
import           Codd.Analysis                  ( DestructiveSectionCheck(..)
                                                , MigrationCheck(..)
                                                , NonDestructiveSectionCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..)
                                                , superUserInAppDatabaseConnInfo
                                                )
import           Codd.Hashing                   ( DbHashes(..)
                                                , DiffType(..)
                                                , hashDifferences
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Internal                  ( withConnection )
import           Codd.Internal.MultiQueryStatement
                                                ( InTransaction
                                                  ( NotInTransaction
                                                  )
                                                , multiQueryStatement_
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                , parsedSqlText
                                                , toMigrationTimestamp
                                                )
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Types                     ( singleTryPolicy )
import           Control.Monad                  ( foldM
                                                , foldM_
                                                , forM
                                                , forM_
                                                , void
                                                , when
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Control.Monad.State            ( MonadState(put)
                                                , State
                                                , execState
                                                )
import           Control.Monad.State.Class      ( get )
import           Data.List                      ( nubBy )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , unpack
                                                )
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
import           Test.Hspec.QuickCheck          ( modifyMaxSuccess )
import           Test.QuickCheck                ( Arbitrary
                                                , chooseBoundedIntegral
                                                , property
                                                , suchThat
                                                )
import           Test.QuickCheck.Arbitrary      ( Arbitrary(arbitrary) )
import           UnliftIO                       ( liftIO )
import           UnliftIO.Concurrent            ( threadDelay )

data DbChange = ChangeEq [(FilePath, DiffType)] | SomeChange

-- | Contains either text or a migration first and the SQL to undo it next.
data MU a = MU a (Maybe Text)

addMig :: Text -> Text -> DbChange -> State [(MU Text, DbChange)] (Text, Text)
addMig doSql undoSql expectedChanges = do
  existingMigs <- get
  put $ existingMigs ++ [(MU doSql $ Just undoSql, expectedChanges)]
  pure (doSql, undoSql)

addMig_ :: Text -> Text -> DbChange -> State [(MU Text, DbChange)] ()
addMig_ doSql undoSql expectedChanges =
  void $ addMig doSql undoSql expectedChanges

addMigNoChanges_ :: Text -> State [(MU Text, DbChange)] ()
addMigNoChanges_ doSql = do
  existingMigs <- get
  put $ existingMigs ++ [(MU doSql Nothing, ChangeEq [])]

migrationsAndHashChange :: [(MU AddedSqlMigration, DbChange)]
migrationsAndHashChange = zipWith
  (\(MU doSql undoSql, c) i ->
    ( MU
      (AddedSqlMigration
        SqlMigration { migrationName       = show i <> "-migration.sql"
                     , nonDestructiveSql   = Just $ mkValidSql doSql
                     , nonDestructiveForce = True
                     , nonDestructiveInTxn = True
                     , destructiveSql      = Nothing
                     , destructiveInTxn    = True
                     }
        (getIncreasingTimestamp i)
      )
      undoSql
    , c
    )
  )
  migs
  (map fromInteger [0 ..]) -- This would be a list of NominalDiffTime, which would have 10^-12s resolution and fail in the DB
 where
  migs = flip execState [] $ do
      -- MISSING:
      -- COLUMNS WITH GENERATED AS (they are hashed but we can't test them without a pg version check)
      -- EXCLUSION CONSTRAINTS
      -- EXTENSIONS
      -- PARTITIONING
      -- LANGUAGES
      -- FOREIGN SERVERS
      -- DOMAINS
      -- TABLESPACES




      -- TABLES AND COLUMNS
    addMig_
        "CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name TEXT)"
        "DROP TABLE employee"
      $ ChangeEq
          [ ("schemas/public/sequences/employee_employee_id_seq", OnlyRight)
          , ("schemas/public/tables/employee/cols/employee_id"  , OnlyRight)
          , ("schemas/public/tables/employee/cols/employee_name", OnlyRight)
          , ( "schemas/public/tables/employee/constraints/employee_pkey"
            , OnlyRight
            )
          , ("schemas/public/tables/employee/indexes/employee_pkey", OnlyRight)
          , ("schemas/public/tables/employee/objhash"              , OnlyRight)
          ]
    addMig_ "ALTER TABLE employee ALTER COLUMN employee_name SET NOT NULL"
            "ALTER TABLE employee ALTER COLUMN employee_name DROP NOT NULL"
      $ ChangeEq
          [ ( "schemas/public/tables/employee/cols/employee_name"
            , BothButDifferent
            )
          ]

    addMig_
        "ALTER TABLE employee ADD COLUMN birthday DATE; ALTER TABLE employee ADD COLUMN deathday DATE;"

        "ALTER TABLE employee DROP COLUMN deathday; ALTER TABLE employee DROP COLUMN birthday;"
      $ ChangeEq
          [ ("schemas/public/tables/employee/cols/birthday", OnlyRight)
          , ("schemas/public/tables/employee/cols/deathday", OnlyRight)
          ]

      -- Column order matters because of things like 'SELECT *'
    addMig_
        "ALTER TABLE employee DROP COLUMN birthday; ALTER TABLE employee DROP COLUMN deathday; \
          \ ALTER TABLE employee ADD COLUMN deathday DATE; ALTER TABLE employee ADD COLUMN birthday DATE;"

        "ALTER TABLE employee DROP COLUMN birthday; ALTER TABLE employee DROP COLUMN deathday; \
          \ ALTER TABLE employee ADD COLUMN birthday DATE; ALTER TABLE employee ADD COLUMN deathday DATE;"

      $ ChangeEq
          [ ("schemas/public/tables/employee/cols/birthday", BothButDifferent)
          , ("schemas/public/tables/employee/cols/deathday", BothButDifferent)
          ]

    addMig_ "ALTER TABLE employee ALTER COLUMN birthday TYPE TIMESTAMP;"
            "ALTER TABLE employee ALTER COLUMN birthday TYPE DATE;"
      $ ChangeEq
          [("schemas/public/tables/employee/cols/birthday", BothButDifferent)]

    addMigNoChanges_
      "ALTER TABLE employee ADD COLUMN IF NOT EXISTS birthday TIMESTAMP;"

    addMig_
        "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';"
        "ALTER TABLE employee ALTER COLUMN deathday DROP DEFAULT;"
      $ ChangeEq
          [("schemas/public/tables/employee/cols/deathday", BothButDifferent)]

    addMigNoChanges_
      "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';"

    addMig_
        "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-04';"

        "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';"

      $ ChangeEq
          [("schemas/public/tables/employee/cols/deathday", BothButDifferent)]


    -- Recreating a column exactly like it was before will affect column order, which will affect the index and the sequence too
    addMig_
        "ALTER TABLE employee DROP COLUMN employee_id; ALTER TABLE employee ADD COLUMN employee_id SERIAL PRIMARY KEY;"

        "DROP TABLE employee; CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name TEXT NOT NULL); \
             \ ALTER TABLE employee ADD COLUMN deathday DATE DEFAULT '2100-02-04'; ALTER TABLE employee ADD COLUMN birthday TIMESTAMP;"

      $ ChangeEq
          [ ( "schemas/public/tables/employee/cols/employee_id"
            , BothButDifferent
            )
            -- Other columns in the same table have their relative order changed as well
          , ("schemas/public/tables/employee/cols/birthday", BothButDifferent)
          , ("schemas/public/tables/employee/cols/deathday", BothButDifferent)
          , ( "schemas/public/tables/employee/cols/employee_name"
            , BothButDifferent
            )
          , ( "schemas/public/sequences/employee_employee_id_seq"
            , BothButDifferent
            ) -- This change happens because due to sequence ownership, we need to
        -- either include the owner column's name or its attnum. We chose the latter thinking it's more common case to rename columns than change
        -- their relative positions.
        -- Constraints, however, reference column names (argh..) due to their expressions being checksummed
          ]


      -- SEQUENCES
    addMig_ "CREATE SEQUENCE some_seq MINVALUE 1 MAXVALUE 100"
            "DROP SEQUENCE some_seq"
      $ ChangeEq [("schemas/public/sequences/some_seq", OnlyRight)]

      -- MINVALUE and MAXVALUE that fit other types so we are sure changing just the seq. type has an effect
    addMig_ "ALTER SEQUENCE some_seq AS smallint"
            "ALTER SEQUENCE some_seq AS bigint"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
    addMig_ "ALTER SEQUENCE some_seq AS integer"
            "ALTER SEQUENCE some_seq AS smallint"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
    addMig_ "ALTER SEQUENCE some_seq START WITH 3"
            "ALTER SEQUENCE some_seq START WITH 1"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
    addMig_ "ALTER SEQUENCE some_seq RESTART WITH 7"
            "ALTER SEQUENCE some_seq RESTART WITH 1"
      $ ChangeEq []
      -- TODO: Where can I find in pg_catalog the restart_with value? Currently it does not affect hashing, sadly.
    addMig_ "ALTER SEQUENCE some_seq MINVALUE 2"
            "ALTER SEQUENCE some_seq MINVALUE 1"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
    addMig_ "ALTER SEQUENCE some_seq MAXVALUE 99999"
            "ALTER SEQUENCE some_seq MAXVALUE 100"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
    addMig_ "ALTER SEQUENCE some_seq INCREMENT BY 2"
            "ALTER SEQUENCE some_seq INCREMENT BY 1"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
    addMig_ "ALTER SEQUENCE some_seq CYCLE" "ALTER SEQUENCE some_seq NO CYCLE"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
    addMig_ "ALTER SEQUENCE some_seq CACHE 2" "ALTER SEQUENCE some_seq CACHE 1"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]
    addMig_ "ALTER SEQUENCE some_seq OWNED BY employee.employee_id"
            "ALTER SEQUENCE some_seq OWNED BY NONE"
      $ ChangeEq [("schemas/public/sequences/some_seq", BothButDifferent)]

      -- CHECK CONSTRAINTS
    addMig_
        "ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')"
        "ALTER TABLE employee DROP CONSTRAINT employee_ck_name"
      $ ChangeEq
          [ ( "schemas/public/tables/employee/constraints/employee_ck_name"
            , OnlyRight
            )
          ]

    addMigNoChanges_
      "ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')"

    addMig_
        "ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> 'EMPTY')"
        "ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')"
      $ ChangeEq
          [ ( "schemas/public/tables/employee/constraints/employee_ck_name"
            , BothButDifferent
            )
          ]

      -- FOREIGN KEYS
    addMig_
        "CREATE TABLE employee_car (employee_id INT NOT NULL, car_model TEXT NOT NULL)"
        "DROP TABLE employee_car"
      $ ChangeEq
          [ ("schemas/public/tables/employee_car/cols/car_model"  , OnlyRight)
          , ("schemas/public/tables/employee_car/cols/employee_id", OnlyRight)
          , ("schemas/public/tables/employee_car/objhash"         , OnlyRight)
          ]

    addMig_
        "CREATE TABLE employee_computer (employee_id INT NOT NULL, computer_model TEXT NOT NULL, UNIQUE (employee_id))"
        "DROP TABLE employee_computer"
      $ ChangeEq
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

    addMig_
        "ALTER TABLE employee_car ADD CONSTRAINT employee_car_employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)"
        "ALTER TABLE employee_car DROP CONSTRAINT employee_car_employee_fk"
      $ ChangeEq
          [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk"
            , OnlyRight
            )
          ]

    addMig_
        "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY DEFERRED"
        "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk NOT DEFERRABLE INITIALLY IMMEDIATE"
      $ ChangeEq
          [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk"
            , BothButDifferent
            )
          ]

    addMig_
        "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY IMMEDIATE"
        "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY DEFERRED"
      $ ChangeEq
          [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk"
            , BothButDifferent
            )
          ]

    addMig_
        "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk NOT DEFERRABLE"
        "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY IMMEDIATE"
      $ ChangeEq
          [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk"
            , BothButDifferent
            )
          ]

    addMig_
        "ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)"
        "ALTER TABLE employee_car DROP CONSTRAINT employee__employee_fk"
      $ ChangeEq
          [ ( "schemas/public/tables/employee_car/constraints/employee__employee_fk"
            , OnlyRight
            )
          ]

      -- Same FK on the same table and column, referencing a different table, but with the same referenced column name as before.
    addMig_
        "ALTER TABLE employee_car DROP CONSTRAINT employee__employee_fk; ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee_computer(employee_id)"
        "ALTER TABLE employee_car DROP CONSTRAINT employee__employee_fk; ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)"
      $ ChangeEq
          [ ( "schemas/public/tables/employee_car/constraints/employee__employee_fk"
            , BothButDifferent
            )
          ]


      -- UNIQUE CONSTRAINTS AND INDEXES
    addMig_
      "ALTER TABLE employee ADD CONSTRAINT unique_employee UNIQUE(employee_name)"
      "ALTER TABLE employee DROP CONSTRAINT unique_employee"
      SomeChange

    addMig_
        "ALTER TABLE employee RENAME CONSTRAINT unique_employee TO employee_unique_name"
        "ALTER TABLE employee RENAME CONSTRAINT employee_unique_name TO unique_employee"
      $ ChangeEq
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

    addMig_
        "CREATE UNIQUE INDEX unique_employee_idx ON employee (employee_name)"
        "DROP INDEX unique_employee_idx"
      $ ChangeEq
          [ ( "schemas/public/tables/employee/indexes/unique_employee_idx"
            , OnlyRight
            )
          ]


      -- FUNCTIONS
    addMig_
        "CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$\
          \BEGIN  \n RETURN i + 1;  \n END;  \n $$ LANGUAGE plpgsql;"
        "DROP FUNCTION increment(integer)"
      $ ChangeEq [("schemas/public/routines/increment;int4", OnlyRight)]

    addMig_
        "CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$\
           \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
        "CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$\
                                                                              \BEGIN  \n RETURN i + 1;  \n END;  \n $$ LANGUAGE plpgsql;"
      $ ChangeEq [("schemas/public/routines/increment;int4", BothButDifferent)]

      -- Change in function args means new function
    addMig_
        "CREATE OR REPLACE FUNCTION increment(i integer, x text) RETURNS integer AS $$\
           \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
        "DROP FUNCTION increment(integer, text)"
      $ ChangeEq [("schemas/public/routines/increment;int4,text", OnlyRight)]

        -- Change in function args means new function
    addMig_
        "CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS integer AS $$\
           \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
        "DROP FUNCTION increment(text, integer)"
      $ ChangeEq [("schemas/public/routines/increment;text,int4", OnlyRight)]

        -- Same everything as existing function, just changing return type
    addMig_
        "DROP FUNCTION increment(text, integer); CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS bigint AS $$\
        \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
        "DROP FUNCTION increment(text, integer); CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS integer AS $$\
                                                                           \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
      $ ChangeEq
          [("schemas/public/routines/increment;text,int4", BothButDifferent)]


      -- TRIGGERS
    addMig_ "ALTER TABLE employee ADD COLUMN name TEXT"
            "ALTER TABLE employee DROP COLUMN name"
            SomeChange

    addMig_
      "CREATE FUNCTION employee_name_rename_set_new() RETURNS TRIGGER AS $$\n\
           \BEGIN\n NEW.name = NEW.employee_name;\n RETURN NEW;\n END\n $$ LANGUAGE plpgsql;"
      "DROP FUNCTION employee_name_rename_set_new()"
      SomeChange

    (createTrigger1, dropTrigger) <-
      addMig
          "CREATE TRIGGER employee_old_app_update_column_name\
        \ \n BEFORE UPDATE ON employee\
        \ \n FOR EACH ROW\
        \ \n WHEN (OLD.employee_name IS DISTINCT FROM NEW.employee_name)\
        \ \n EXECUTE PROCEDURE employee_name_rename_set_new()"
          "DROP TRIGGER employee_old_app_update_column_name ON employee;"
        $ ChangeEq
            [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name"
              , OnlyRight
              )
            ]


    -- No WHEN in the recreated trigger
    addMig_
        "DROP TRIGGER employee_old_app_update_column_name ON employee; CREATE TRIGGER employee_old_app_update_column_name\
        \ \n BEFORE UPDATE ON employee\
        \ \n FOR EACH ROW\
        \ \n EXECUTE PROCEDURE employee_name_rename_set_new()"
        (dropTrigger <> createTrigger1)
      $ ChangeEq
          [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name"
            , BothButDifferent
            )
          ]

    addMig_
        "DROP TRIGGER employee_old_app_update_column_name ON employee"
        "CREATE TRIGGER employee_old_app_update_column_name\
        \ \n BEFORE UPDATE ON employee\
        \ \n FOR EACH ROW\
        \ \n EXECUTE PROCEDURE employee_name_rename_set_new()"
      $ ChangeEq
          [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name"
            , OnlyLeft
            )
          ]

    addMig_
      "ALTER TABLE employee DROP COLUMN employee_name"
      -- Undoing this statement requires recreating a lot of dependent objects..
      -- Also, on Pg <= 12, dropping and readding employee_id in the same ALTER TABLE statement
      -- makes Pg create a new sequence with a different name instead of doing what you might expect
      "ALTER SEQUENCE some_seq OWNED BY NONE; \
    \\nALTER TABLE employee DROP COLUMN birthday, DROP COLUMN deathday, DROP COLUMN name, DROP COLUMN employee_id CASCADE;\
    \\nALTER TABLE employee ADD COLUMN employee_name TEXT NOT NULL, ADD COLUMN deathday DATE NULL DEFAULT '2100-02-04', \
                          \ ADD COLUMN birthday TIMESTAMP, ADD COLUMN employee_id SERIAL PRIMARY KEY, \
                          \ ADD COLUMN name TEXT;\
    \\nALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> 'EMPTY');\
    \\nALTER TABLE employee ADD CONSTRAINT employee_unique_name UNIQUE(employee_name);\
    \\nCREATE UNIQUE INDEX unique_employee_idx ON employee (employee_name);\
    \\nALTER TABLE employee_car ADD CONSTRAINT employee_car_employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id);\
    \\nALTER SEQUENCE some_seq OWNED BY employee.employee_id;"
      SomeChange

    addMig_ "ALTER TABLE employee RENAME COLUMN name TO employee_name"
            "ALTER TABLE employee RENAME COLUMN employee_name TO name"
            SomeChange


      -- VIEWS
    addMig_
        "CREATE OR REPLACE VIEW all_employee_names (employee_name) AS (SELECT employee_name FROM employee)"
        "DROP VIEW all_employee_names"
      $ ChangeEq [("schemas/public/views/all_employee_names", OnlyRight)]

    addMig_
        "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT employee_name FROM employee)"
        "CREATE OR REPLACE VIEW all_employee_names (employee_name) AS (SELECT employee_name FROM employee)"
      $ ChangeEq [("schemas/public/views/all_employee_names", BothButDifferent)]

    addMigNoChanges_
      "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT employee_name FROM employee)"

    addMig_
        "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT 'Mr. ' || employee_name FROM employee)"
        "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT employee_name FROM employee)"
      $ ChangeEq [("schemas/public/views/all_employee_names", BothButDifferent)]

    addMig_ "ALTER VIEW all_employee_names OWNER TO \"codd-test-user\""
            "ALTER VIEW all_employee_names OWNER TO \"postgres\""
      $ ChangeEq [("schemas/public/views/all_employee_names", BothButDifferent)]


      -- ROW LEVEL SECURITY
    addMig_ "ALTER TABLE employee ENABLE ROW LEVEL SECURITY"
            "ALTER TABLE employee DISABLE ROW LEVEL SECURITY"
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMig_ "ALTER TABLE employee FORCE ROW LEVEL SECURITY"
            "ALTER TABLE employee NO FORCE ROW LEVEL SECURITY"
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMig_ "ALTER TABLE employee NO FORCE ROW LEVEL SECURITY"
            "ALTER TABLE employee FORCE ROW LEVEL SECURITY"
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    (createPolicy1, dropPolicy) <-
      addMig
          "CREATE POLICY some_policy ON employee USING (employee_name <> 'Some Name');"
          "DROP POLICY some_policy ON employee;"
        $ ChangeEq
            [("schemas/public/tables/employee/policies/some_policy", OnlyRight)]

    (dropCreatePolicy2, _) <-
      addMig
          "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee USING (employee_name <> 'Some Other Name');"
          (dropPolicy <> createPolicy1)
        $ ChangeEq
            [ ( "schemas/public/tables/employee/policies/some_policy"
              , BothButDifferent
              )
            ]

    (dropCreatePolicy3, _) <-
      addMig
          "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name');"
          dropCreatePolicy2
        $ ChangeEq
            [ ( "schemas/public/tables/employee/policies/some_policy"
              , BothButDifferent
              )
            ]

    (dropCreatePolicy4, _) <-
      addMig
          "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name') WITH CHECK (TRUE);"
          dropCreatePolicy3
        $ ChangeEq
            [ ( "schemas/public/tables/employee/policies/some_policy"
              , BothButDifferent
              )
            ]

    let
      createPolicy5
        = "CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name') WITH CHECK (TRUE);"
    addMig_ ("DROP POLICY some_policy ON employee;" <> createPolicy5)
            dropCreatePolicy4
      $ ChangeEq []

    addMig_ "DROP POLICY some_policy ON employee;" createPolicy5 $ ChangeEq
      [("schemas/public/tables/employee/policies/some_policy", OnlyLeft)]

      -- ROLES
    (createUnmappedRole, dropUnmappedRole) <-
      addMig "CREATE ROLE any_unmapped_role" "DROP ROLE any_unmapped_role"
        $ ChangeEq []
    addMig_ dropUnmappedRole createUnmappedRole $ ChangeEq []
    addMig_ "CREATE ROLE \"extra-codd-test-user\""
            "DROP ROLE \"extra-codd-test-user\""
      $ ChangeEq [("roles/extra-codd-test-user", OnlyRight)]

    addMig_
        "ALTER ROLE \"codd-test-user\" SET search_path TO public, pg_catalog"
        "ALTER ROLE \"codd-test-user\" RESET search_path"
      $ ChangeEq [("roles/codd-test-user", BothButDifferent)]

    addMig_
        "ALTER ROLE \"codd-test-user\" WITH BYPASSRLS; ALTER ROLE \"codd-test-user\" WITH REPLICATION;"
        "ALTER ROLE \"codd-test-user\" WITH NOBYPASSRLS; ALTER ROLE \"codd-test-user\" WITH NOREPLICATION; "
      $ ChangeEq [("roles/codd-test-user", BothButDifferent)]

    addMigNoChanges_ "ALTER ROLE \"codd-test-user\" WITH BYPASSRLS"

    -- Database-related permissions affect only roles, not db-settings
    (revokeConnect, grantConnect) <-
      addMig
          "REVOKE CONNECT ON DATABASE \"codd-test-db\" FROM \"codd-test-user\""
          "GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\""
        $ ChangeEq [("roles/codd-test-user", BothButDifferent)]

    addMig_ "GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\""
            revokeConnect
      $ ChangeEq [("roles/codd-test-user", BothButDifferent)]

    addMigNoChanges_ grantConnect

    -- Role membership
    (grantRole, revokeRole) <-
      addMig "GRANT \"extra-codd-test-user\" TO \"codd-test-user\""
             "REVOKE \"extra-codd-test-user\" FROM \"codd-test-user\""
        $ ChangeEq [("roles/codd-test-user", BothButDifferent)]

    addMig_ revokeRole grantRole
      $ ChangeEq [("roles/codd-test-user", BothButDifferent)]

    -- Config attributes
    addMig_ "ALTER ROLE postgres SET search_path TO public, pg_catalog"
            "ALTER ROLE postgres RESET search_path"
      $ ChangeEq [("roles/postgres", BothButDifferent)]

    addMig_
        "ALTER ROLE \"codd-test-user\" SET search_path TO DEFAULT"
        "ALTER ROLE \"codd-test-user\" SET search_path TO public, pg_catalog"
      $ ChangeEq [("roles/codd-test-user", BothButDifferent)]

    addMig_ "ALTER ROLE postgres SET search_path TO DEFAULT"
            "ALTER ROLE postgres SET search_path TO public, pg_catalog"
      $ ChangeEq [("roles/postgres", BothButDifferent)]

    addMigNoChanges_ "ALTER ROLE postgres SET search_path TO DEFAULT"

      -- PERMISSIONS
      -- For tables
      -- IMPORTANT note:
      -- For some reason, revoking privileges from a table
      -- seems to have the effect of GRANTing permissions to the revoker, but
      -- only the first time it's revoked.
      -- So be careful making sure REVOKE does what you expect!
    (grantAll, _) <-
      addMig
          "GRANT ALL ON TABLE employee TO \"codd-test-user\""
              -- Read the note above to understand REVOKE from postgres
          "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; REVOKE ALL ON TABLE employee FROM postgres;"
        $ ChangeEq
            [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMig_ "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"" grantAll
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMig_ "GRANT SELECT ON TABLE employee TO \"codd-test-user\""
            "REVOKE SELECT ON TABLE employee FROM \"codd-test-user\""
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMig_ "GRANT INSERT ON TABLE employee TO \"codd-test-user\""
            "REVOKE INSERT ON TABLE employee FROM \"codd-test-user\""
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMig_ "GRANT DELETE ON TABLE employee TO \"codd-test-user\""
            "REVOKE DELETE ON TABLE employee FROM \"codd-test-user\""
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

      -- For sequences
      -- IMPORTANT note:
      -- For some reason, revoking privileges from a sequence owned by a table column
      -- seems to have the effect of GRANTing permissions to the revoker.
      -- So be careful making sure REVOKE does what you expect!
    addMig_
        "GRANT SELECT ON SEQUENCE employee_employee_id_seq TO \"codd-test-user\""
        -- Read the note above to understand REVOKE from postgres
        "REVOKE SELECT ON SEQUENCE employee_employee_id_seq FROM \"codd-test-user\"; REVOKE ALL ON SEQUENCE employee_employee_id_seq FROM postgres"
      $ ChangeEq
          [ ( "schemas/public/sequences/employee_employee_id_seq"
            , BothButDifferent
            )
          ]


    -- At this point codd-test-user has S+I+D permissions on the employee table
    -- Order of granting does not matter, nor do grantors
    addMig_
        "REVOKE ALL ON TABLE employee FROM \"codd-test-user\""
        "GRANT SELECT,INSERT,DELETE ON TABLE employee TO \"codd-test-user\""
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMig_ "GRANT INSERT, DELETE ON TABLE employee TO \"codd-test-user\";"
            "REVOKE INSERT, DELETE ON TABLE employee FROM \"codd-test-user\";"
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    -- At this point codd-test-user has I+D permissions on the employee table
    addMig_
        "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT INSERT, DELETE ON TABLE employee TO \"codd-test-user\";"
        "GRANT INSERT, DELETE ON TABLE employee TO \"codd-test-user\";"
      $ ChangeEq []

    -- At this point codd-test-user has I+D permissions on the employee table
    addMig_
        "GRANT ALL ON TABLE employee TO \"codd-test-user\""
        "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT INSERT, DELETE ON TABLE employee TO \"codd-test-user\""
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMig_ "GRANT ALL ON TABLE employee TO \"extra-codd-test-user\""
            "REVOKE ALL ON TABLE employee FROM \"extra-codd-test-user\""
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]

    addMigNoChanges_
      "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"extra-codd-test-user\""

    addMig_
        "GRANT ALL ON TABLE employee TO PUBLIC"
        "REVOKE ALL ON TABLE employee FROM PUBLIC; GRANT ALL ON TABLE employee TO \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"extra-codd-test-user\";"
      $ ChangeEq [("schemas/public/tables/employee/objhash", BothButDifferent)]


      -- Permissions of unmapped role don't affect hashing
      -- For some reason, GRANTing to unmapped_role1 for a VIEW also adds permissions to the VIEW owner. This doesn't seem to happen for tables or sequences..
    addMig_ "GRANT ALL ON all_employee_names TO \"codd-test-user\""
            "REVOKE ALL ON all_employee_names FROM \"codd-test-user\""
            SomeChange
    (createUnmappedRoleAndGrant, dropUnmappedRole) <-
      addMig
          "CREATE ROLE unmapped_role1; GRANT ALL ON TABLE employee TO unmapped_role1; GRANT ALL ON SEQUENCE employee_employee_id_seq TO unmapped_role1; GRANT ALL ON all_employee_names TO unmapped_role1"
          "DROP OWNED BY unmapped_role1; DROP ROLE unmapped_role1"
        $ ChangeEq []

    addMig_ dropUnmappedRole createUnmappedRoleAndGrant $ ChangeEq []

      -- CREATING UNMAPPED AND MAPPED SCHEMAS
    addMig_ "CREATE SCHEMA unmappedschema" "DROP SCHEMA unmappedschema"
      $ ChangeEq []
    addMig_ "DROP SCHEMA unmappedschema" "CREATE SCHEMA unmappedschema"
      $ ChangeEq []
    (createMappedSchema, dropMappedSchema) <-
      addMig "CREATE SCHEMA \"codd-extra-mapped-schema\""
             "DROP SCHEMA \"codd-extra-mapped-schema\""
        $ ChangeEq [("schemas/codd-extra-mapped-schema/objhash", OnlyRight)]

    addMig_ dropMappedSchema createMappedSchema
      $ ChangeEq [("schemas/codd-extra-mapped-schema/objhash", OnlyLeft)]


      -- DATABASE SETTINGS
    addMig_
        "ALTER DATABASE \"codd-test-db\" SET default_transaction_isolation TO 'serializable'; SET default_transaction_isolation TO 'serializable';"
        "ALTER DATABASE \"codd-test-db\" RESET default_transaction_isolation; RESET default_transaction_isolation;"
      $ ChangeEq [("db-settings", BothButDifferent)]

    -- COLLATIONS
    (createCutf8Coll, dropColl) <-
      addMig "CREATE COLLATION new_collation (locale = 'C.utf8');"
             "DROP COLLATION new_collation;"
        $ ChangeEq [("schemas/public/collations/new_collation", OnlyRight)]
    (dropCreatePtBRColl, _) <-
      addMig
          (  dropColl
          <> " CREATE COLLATION new_collation (locale = 'pt_BR.utf8');"
          )
          (dropColl <> createCutf8Coll)
        $ ChangeEq
            [("schemas/public/collations/new_collation", BothButDifferent)]
    addMig_
        (dropColl
        <> " CREATE COLLATION new_collation (provider = icu, locale = 'de-u-co-phonebk');"
        )
        dropCreatePtBRColl
      $ ChangeEq [("schemas/public/collations/new_collation", BothButDifferent)]

    addMig_ "ALTER TABLE employee ADD COLUMN employee_surname TEXT;"
            "ALTER TABLE employee DROP COLUMN employee_surname;"
            SomeChange

    addMig_
        "ALTER TABLE employee ALTER COLUMN employee_surname TYPE TEXT COLLATE \"new_collation\";"
        "ALTER TABLE employee ALTER COLUMN employee_surname TYPE TEXT COLLATE \"default\";"
      $ ChangeEq
          [ ( "schemas/public/tables/employee/cols/employee_surname"
            , BothButDifferent
            )
          ]

    -- Deterministic collations were introduced in Pg 12..
    -- addMig_ (dropColl <> " CREATE COLLATION (locale = 'C.utf8', deterministic = false) new_collation;") dropColl $ ChangeEq [("schemas/public/collations/new_collation", BothButDifferent )]

    -- TYPES
    -- TODO:
    -- Enum types
    -- Composite types
    -- Types with subscription that are not arrays? (is this possible)
    -- Composite types with row types
    -- Domains

      -- CRUD
    addMig_ "INSERT INTO employee (employee_name) VALUES ('Marcelo')"
            "DELETE FROM employee WHERE employee_name='Marcelo'"
      $ ChangeEq []

lastMaybe :: [a] -> Maybe a
lastMaybe []       = Nothing
lastMaybe [x     ] = Just x
lastMaybe (_ : xs) = lastMaybe xs

newtype NumMigsToReverse = NumMigsToReverse Int deriving stock (Show)
instance Arbitrary NumMigsToReverse where
  arbitrary = NumMigsToReverse
    <$> chooseBoundedIntegral (5, length migrationsAndHashChange - 1)

-- | This type includes each migration with their expected changes and hashes after applied. Hashes before the first migration are not included.
newtype AccumChanges = AccumChanges [((AddedSqlMigration, DbChange), DbHashes)]

spec :: Spec
spec = do
  describe "DbDependentSpecs" $ do
    describe "Hashing tests" $ do
      modifyMaxSuccess (const 3) -- This is a bit heavy on CI but this test is too important
        $ aroundFreshDatabase
        $ it "Checksumming schema changes"
        $ \emptyDbInfo2 -> property $ \(NumMigsToReverse num) -> do
            let
                -- emptyDbInfo = emptyDbInfo2 { hashedChecksums = False }
                -- Use the above definition of emptyDbInfo if it helps debugging
                emptyDbInfo = emptyDbInfo2
                connInfo    = superUserInAppDatabaseConnInfo emptyDbInfo
                getHashes sett = runStdoutLoggingT $ withConnection
                  connInfo
                  (readHashesFromDatabaseWithSettings sett)
            hashBeforeEverything <- getHashes emptyDbInfo
            (_, AccumChanges applyHistory, hashesAndUndo :: [ ( DbHashes
                , Maybe Text
                )
              ]                                                           ) <-
              forwardApplyMigs 0 hashBeforeEverything emptyDbInfo
            let hashAfterAllMigs =
                  maybe hashBeforeEverything snd (lastMaybe applyHistory)

            -- 1. After applying all migrations, let's make sure we're actually applying
            -- them by fetching some data..
            ensureMarceloExists connInfo

            -- 2. Now undo part or all of the SQL migrations and check that
            -- checksums match each step of the way in reverse!
            liftIO
              $  putStrLn
              $  "Undoing migrations in reverse. Num = "
              <> show num
            hashesAfterEachUndo <-
              forM (take num $ reverse hashesAndUndo)
                $ \(expectedHashesAfterUndo, mUndoSql) -> case mUndoSql of
                    Nothing      -> pure expectedHashesAfterUndo
                    Just undoSql -> do
                      runStdoutLoggingT $ withConnection connInfo $ \conn ->
                        multiQueryStatement_
                            (NotInTransaction singleTryPolicy)
                            conn
                          $ mkValidSql undoSql
                      hashesAfterUndo <- getHashes emptyDbInfo
                      let
                        diff = hashDifferences hashesAfterUndo
                                               expectedHashesAfterUndo
                      (undoSql, diff) `shouldBe` (undoSql, Map.empty)
                      -- What follows is just a sanity check
                      (undoSql, hashesAfterUndo)
                        `shouldBe` (undoSql, expectedHashesAfterUndo)
                      pure hashesAfterUndo
            void $ withConnection
              connInfo
              (\conn -> DB.execute
                conn
                "WITH reversedMigs (name) AS (SELECT name FROM codd_schema.sql_migrations ORDER BY migration_timestamp DESC LIMIT ?) DELETE FROM codd_schema.sql_migrations USING reversedMigs WHERE reversedMigs.name=sql_migrations.name"
                (DB.Only num)
              )

            let hashesAfterUndo =
                  fromMaybe hashAfterAllMigs (lastMaybe hashesAfterEachUndo)

            -- 3. Finally, reapply every migration that was reversed.
            -- This may look unnecessary, but if a created object (e.g. a column) is not dropped
            -- on the way forward, we might not get a change to the effects of re-adding it,
            -- which manifests as a dead but unusable `attnum` in pg_attribute.
            liftIO $ putStrLn "Re-applying migrations"
            void $ forwardApplyMigs (length migrationsAndHashChange - num)
                                    hashesAfterUndo
                                    emptyDbInfo
            ensureMarceloExists connInfo
 where
  extractDbCksums = \case
    ChecksumsNotVerified -> error "checksumsNotVerified internal error in test"
    ChecksumsDiffer ChecksumsPair { databaseChecksums } -> databaseChecksums
    ChecksumsMatch cksums -> cksums
  forwardApplyMigs numMigsAlreadyApplied hashBeforeEverything emptyDbInfo =
    foldM
      (\(hashSoFar, AccumChanges appliedMigsAndCksums, hundo) (MU nextMig undoSql, expectedChanges) ->
        do
          let appliedMigs = map (fst . fst) appliedMigsAndCksums
              newMigs     = appliedMigs ++ [nextMig]
              dbInfo      = emptyDbInfo { sqlMigrations = Right newMigs }
          dbHashesAfterMig <- runStdoutLoggingT $ applyMigrationsNoCheck
            dbInfo
            (readHashesFromDatabaseWithSettings dbInfo)
          let migText =
                parsedSqlText <$> nonDestructiveSql (addedSqlMig nextMig)
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

          return
            ( dbHashesAfterMig
            , AccumChanges
            $  appliedMigsAndCksums
            ++ [((nextMig, expectedChanges), dbHashesAfterMig)]
            , hundo ++ [(hashSoFar, undoSql)]
            )
      )
      (hashBeforeEverything, AccumChanges [], [])
      (drop numMigsAlreadyApplied migrationsAndHashChange)
  ensureMarceloExists connInfo =
    withConnection
        connInfo
        (\conn -> unsafeQuery1
          conn
          "SELECT COUNT(*) employee_name FROM employee WHERE employee_name='Marcelo'"
          ()
        )
      `shouldReturn` DB.Only (1 :: Int)
