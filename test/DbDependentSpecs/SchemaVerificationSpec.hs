module DbDependentSpecs.SchemaVerificationSpec where

import Codd (applyMigrationsNoCheck)
import Codd.Environment (CoddSettings (..))
import Codd.Internal (withConnection)
import Codd.Internal.MultiQueryStatement
  ( applyStatementStream,
    forceStreamConcurrently,
  )
import Codd.Logging (runCoddLogger)
import Codd.Parsing
  ( AddedSqlMigration (..),
    EnvVars,
    ParsedSql (..),
    PureStream (..),
    SqlMigration (..),
    hoistAddedSqlMigration,
    parseSqlMigration,
  )
import Codd.Query (queryServerMajorAndFullVersion, unsafeQuery1)
import Codd.Representations
  ( DbRep (..),
    DiffType (..),
    readRepresentationsFromDbWithSettings,
    schemaDifferences,
  )
import Codd.Representations.Database
  ( readRepsFromDbWithNewTxn,
  )
import Codd.Representations.Types (ObjName (..))
import Codd.Types (PgMajorVersion (..), SchemaAlgo (..), SchemaSelection (..))
import Control.Monad
  ( foldM,
    forM,
    void,
    when,
    zipWithM,
  )
import Control.Monad.State
  ( MonadState (put),
    State,
    execState,
  )
import Control.Monad.State.Class (get)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (ConnectInfo (..))
import qualified Database.PostgreSQL.Simple as DB
import DbUtils
  ( aroundFreshDatabase,
    finallyDrop,
    getIncreasingTimestamp,
    mkValidSql,
    parseSqlMigrationIO,
    testConnTimeout,
  )
import qualified Streaming.Prelude as Streaming
import System.Process.Typed
  ( ExitCode (..),
    byteStringInput,
    readProcessStdout,
    runProcess,
    setStdin,
    shell,
  )
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
  ( Arbitrary,
    chooseBoundedIntegral,
    property,
  )
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import UnliftIO (liftIO)

data DiffTypeSimplified = DBothButDifferent | DExpectedButNotFound | DNotExpectedButFound
  deriving stock (Show, Eq)

data DbChange = ChangeEq [(FilePath, DiffTypeSimplified)] | SomeChange

simplifyDiff :: DiffType -> DiffTypeSimplified
simplifyDiff = \case
  ExpectedButNotFound -> DExpectedButNotFound
  NotExpectedButFound _ -> DNotExpectedButFound
  BothButDifferent _ -> DBothButDifferent

-- | Contains either text or a migration first and the SQL to undo it next.
data MU a = MU a (Maybe Text)

unMU :: MU a -> a
unMU (MU a _) = a

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

hoistMU ::
  (Monad m, Monad n) =>
  (forall x. m x -> n x) ->
  (MU (AddedSqlMigration m), DbChange) ->
  (MU (AddedSqlMigration n), DbChange)
hoistMU f (MU sqlMig tst, change) =
  (MU (hoistAddedSqlMigration f sqlMig) tst, change)

migrationsForPgDumpRestoreTest ::
  forall m. (MonadThrow m, EnvVars m) => m [AddedSqlMigration m]
migrationsForPgDumpRestoreTest =
  mapM
    parseMig
    [ "CREATE FUNCTION test_function_with_whitespace() RETURNS INT AS $$\n\
      \    BEGIN\n\
      \        SELECT 1;\n\
      \\n\
      \\n\
      \        -- White space\n\
      \\n\
      \\n\
      \        \n\
      \              -- More white space\n\
      \        \n\
      \    END\n\
      \$$ LANGUAGE plpgsql;"
    ]
  where
    parseMig sql = do
      mig <-
        either (error "Could not parse SQL migration") id
          <$> parseSqlMigration @m @(PureStream m)
            "1900-01-01-00-00-00-migration.sql"
            (PureStream $ Streaming.yield sql)
      pure $
        AddedSqlMigration
          (mig {migrationName = "0-dump-migration.sql"})
          (getIncreasingTimestamp 99999)

migrationsAndRepChange ::
  forall m.
  (MonadThrow m, EnvVars m) =>
  PgMajorVersion ->
  Int ->
  m [(MU (AddedSqlMigration m), DbChange)]
migrationsAndRepChange (PgMajorVersion pgVersion) pgFullVersion =
  zipWithM
    ( \(MU doSql undoSql, c) i -> do
        mig <-
          either (error "Could not parse SQL migration") id
            <$> parseSqlMigration @m @(PureStream m)
              "1900-01-01-00-00-00-migration.sql"
              (PureStream $ Streaming.yield doSql)
        pure
          ( MU
              ( AddedSqlMigration
                  (mig {migrationName = show i <> "-migration.sql"})
                  (getIncreasingTimestamp i)
              )
              undoSql,
            c
          )
    )
    (migrationsAndRepChangeText pgVersion pgFullVersion)
    (map fromInteger [0 ..]) -- This would be a list of DiffTime, which would have 10^-12s resolution and fail in the DB

migrationsAndRepChangeText :: Int -> Int -> [(MU Text, DbChange)]
migrationsAndRepChangeText pgVersion pgFullVersion = flip execState [] $ do
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
    "CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name VARCHAR(30), favorite_number numeric(10,2))"
    "DROP TABLE employee"
    $ ChangeEq
      [ ( "schemas/public/sequences/employee_employee_id_seq",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/cols/employee_id",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/cols/employee_name",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/cols/favorite_number",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/constraints/employee_pkey",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/indexes/employee_pkey",
          DExpectedButNotFound
        ),
        ("schemas/public/tables/employee/objrep", DExpectedButNotFound)
      ]
  addMig_
    "ALTER TABLE employee ALTER COLUMN employee_name TYPE VARCHAR(50)"
    "ALTER TABLE employee ALTER COLUMN employee_name TYPE VARCHAR(30)"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/employee_name",
          DBothButDifferent
        )
      ]
  addMig_
    "ALTER TABLE employee ALTER COLUMN employee_name SET NOT NULL"
    "ALTER TABLE employee ALTER COLUMN employee_name DROP NOT NULL"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/employee_name",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER TABLE employee ALTER COLUMN favorite_number TYPE numeric(9,2)"
    "ALTER TABLE employee ALTER COLUMN favorite_number TYPE numeric(10,2)"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/favorite_number",
          DBothButDifferent
        )
      ]
  addMig_
    "ALTER TABLE employee ALTER COLUMN favorite_number TYPE numeric(9,3)"
    "ALTER TABLE employee ALTER COLUMN favorite_number TYPE numeric(9,2)"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/favorite_number",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER TABLE employee ADD COLUMN birthday DATE; ALTER TABLE employee ADD COLUMN deathday DATE;"
    "ALTER TABLE employee DROP COLUMN deathday; ALTER TABLE employee DROP COLUMN birthday;"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/birthday",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/cols/deathday",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/objrep",
          DBothButDifferent -- Column order goes into objrep
        )
      ]

  -- Column order matters because of things like 'SELECT *' and it goes into the table's objrep
  -- to reduce number of files changed when rewriting tables and other kinds of changes
  addMig_
    "ALTER TABLE employee DROP COLUMN birthday; ALTER TABLE employee DROP COLUMN deathday; \
    \ ALTER TABLE employee ADD COLUMN deathday DATE; ALTER TABLE employee ADD COLUMN birthday DATE;"
    "ALTER TABLE employee DROP COLUMN birthday; ALTER TABLE employee DROP COLUMN deathday; \
    \ ALTER TABLE employee ADD COLUMN birthday DATE; ALTER TABLE employee ADD COLUMN deathday DATE;"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/objrep",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER TABLE employee ALTER COLUMN birthday TYPE TIMESTAMP;"
    "ALTER TABLE employee ALTER COLUMN birthday TYPE DATE;"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/birthday",
          DBothButDifferent
        )
      ]

  addMigNoChanges_
    "ALTER TABLE employee ADD COLUMN IF NOT EXISTS birthday TIMESTAMP;"

  addMig_
    "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';"
    "ALTER TABLE employee ALTER COLUMN deathday DROP DEFAULT;"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/deathday",
          DBothButDifferent
        )
      ]

  addMigNoChanges_
    "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';"

  addMig_
    "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-04';"
    "ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/deathday",
          DBothButDifferent
        )
      ]

  -- Recreating a column exactly like it was before will affect column order, which will affect the index, the sequence and the table's objrep too, but not the column reps themselves
  addMig_
    "ALTER TABLE employee DROP COLUMN employee_id; ALTER TABLE employee ADD COLUMN employee_id SERIAL PRIMARY KEY;"
    "DROP TABLE employee; CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name VARCHAR(50) NOT NULL, favorite_number numeric(9,3)); \
    \ ALTER TABLE employee ADD COLUMN deathday DATE DEFAULT '2100-02-04'; ALTER TABLE employee ADD COLUMN birthday TIMESTAMP;"
    $ ChangeEq
      [ -- Other columns in the same table have their relative order changed as well
        ( "schemas/public/tables/employee/objrep",
          DBothButDifferent
        ),
        ( "schemas/public/sequences/employee_employee_id_seq",
          DBothButDifferent
        ) -- This change happens because due to sequence ownership, we need to
        -- either include the owner column's name or its attnum. We chose the latter thinking it's more common case to rename columns than change
        -- their relative positions.
        -- Constraints, however, reference column names (argh..) due to their expressions being verified
      ]

  -- SEQUENCES
  addMig_
    "CREATE SEQUENCE some_seq MINVALUE 1 MAXVALUE 100"
    "DROP SEQUENCE some_seq"
    $ ChangeEq [("schemas/public/sequences/some_seq", DExpectedButNotFound)]

  -- MINVALUE and MAXVALUE that fit other types so we are sure changing just the seq. type has an effect
  addMig_
    "ALTER SEQUENCE some_seq AS smallint"
    "ALTER SEQUENCE some_seq AS bigint"
    $ ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]
  addMig_
    "ALTER SEQUENCE some_seq AS integer"
    "ALTER SEQUENCE some_seq AS smallint"
    $ ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]
  addMig_
    "ALTER SEQUENCE some_seq START WITH 3"
    "ALTER SEQUENCE some_seq START WITH 1"
    $ ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]
  addMig_
    "ALTER SEQUENCE some_seq RESTART WITH 7"
    "ALTER SEQUENCE some_seq RESTART WITH 1"
    $ ChangeEq []
  -- TODO: Where can I find in pg_catalog the restart_with value? Currently it does not affect hashing, sadly.
  addMig_
    "ALTER SEQUENCE some_seq MINVALUE 2"
    "ALTER SEQUENCE some_seq MINVALUE 1"
    $ ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]
  addMig_
    "ALTER SEQUENCE some_seq MAXVALUE 99999"
    "ALTER SEQUENCE some_seq MAXVALUE 100"
    $ ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]
  addMig_
    "ALTER SEQUENCE some_seq INCREMENT BY 2"
    "ALTER SEQUENCE some_seq INCREMENT BY 1"
    $ ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]
  addMig_ "ALTER SEQUENCE some_seq CYCLE" "ALTER SEQUENCE some_seq NO CYCLE" $
    ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]
  addMig_ "ALTER SEQUENCE some_seq CACHE 2" "ALTER SEQUENCE some_seq CACHE 1" $
    ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]
  addMig_
    "ALTER SEQUENCE some_seq OWNED BY employee.employee_id"
    "ALTER SEQUENCE some_seq OWNED BY NONE"
    $ ChangeEq [("schemas/public/sequences/some_seq", DBothButDifferent)]

  -- CHECK CONSTRAINTS
  addMig_
    "ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')"
    "ALTER TABLE employee DROP CONSTRAINT employee_ck_name"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/constraints/employee_ck_name",
          DExpectedButNotFound
        )
      ]

  addMigNoChanges_
    "ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')"

  addMig_
    "ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> 'EMPTY')"
    "ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')"
    -- There's a bug in pg 17.0-17.2 where constraint definitions are not available, so they don't change in codd
    -- See https://www.postgresql.org/message-id/flat/a90f53c4-56f3-4b07-aefc-49afdc67dba6%40app.fastmail.com
    $ if pgFullVersion <= 170002 && pgVersion == 17
      then ChangeEq []
      else
        ChangeEq
          [ ( "schemas/public/tables/employee/constraints/employee_ck_name",
              DBothButDifferent
            )
          ]

  -- FOREIGN KEYS
  addMig_
    "CREATE TABLE employee_car (employee_id INT NOT NULL, car_model TEXT NOT NULL)"
    "DROP TABLE employee_car"
    $ ChangeEq
      [ ( "schemas/public/tables/employee_car/cols/car_model",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee_car/cols/employee_id",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee_car/objrep",
          DExpectedButNotFound
        )
      ]

  addMig_
    "CREATE TABLE employee_computer (employee_id INT NOT NULL, computer_model TEXT NOT NULL, UNIQUE (employee_id))"
    "DROP TABLE employee_computer"
    $ ChangeEq
      [ ( "schemas/public/tables/employee_computer/cols/computer_model",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee_computer/cols/employee_id",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee_computer/constraints/employee_computer_employee_id_key",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee_computer/indexes/employee_computer_employee_id_key",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee_computer/objrep",
          DExpectedButNotFound
        )
      ]

  addMig_
    "ALTER TABLE employee_car ADD CONSTRAINT employee_car_employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)"
    "ALTER TABLE employee_car DROP CONSTRAINT employee_car_employee_fk"
    $ ChangeEq
      [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk",
          DExpectedButNotFound
        )
      ]

  addMig_
    "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY DEFERRED"
    "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk NOT DEFERRABLE INITIALLY IMMEDIATE"
    $ ChangeEq
      [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY IMMEDIATE"
    "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY DEFERRED"
    $ ChangeEq
      [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk NOT DEFERRABLE"
    "ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY IMMEDIATE"
    $ ChangeEq
      [ ( "schemas/public/tables/employee_car/constraints/employee_car_employee_fk",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)"
    "ALTER TABLE employee_car DROP CONSTRAINT employee__employee_fk"
    $ ChangeEq
      [ ( "schemas/public/tables/employee_car/constraints/employee__employee_fk",
          DExpectedButNotFound
        )
      ]

  -- Same FK on the same table and column, referencing a different table, but with the same referenced column name as before.
  addMig_
    "ALTER TABLE employee_car DROP CONSTRAINT employee__employee_fk; ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee_computer(employee_id)"
    "ALTER TABLE employee_car DROP CONSTRAINT employee__employee_fk; ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)"
    $ ChangeEq
      [ ( "schemas/public/tables/employee_car/constraints/employee__employee_fk",
          DBothButDifferent
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
      [ ( "schemas/public/tables/employee/constraints/employee_unique_name",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/constraints/unique_employee",
          DNotExpectedButFound
        ),
        ( "schemas/public/tables/employee/indexes/employee_unique_name",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/indexes/unique_employee",
          DNotExpectedButFound
        )
      ]

  addMig_
    "CREATE UNIQUE INDEX unique_employee_idx ON employee (employee_name)"
    "DROP INDEX unique_employee_idx"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/indexes/unique_employee_idx",
          DExpectedButNotFound
        )
      ]

  -- FUNCTIONS
  addMig_
    "CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$\
    \BEGIN  \n RETURN i + 1;  \n END;  \n $$ LANGUAGE plpgsql;"
    "DROP FUNCTION increment(integer)"
    $ ChangeEq
      [("schemas/public/routines/increment;int4", DExpectedButNotFound)]

  addMig_
    "CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$\
    \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
    "CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$\
    \BEGIN  \n RETURN i + 1;  \n END;  \n $$ LANGUAGE plpgsql;"
    $ ChangeEq
      [("schemas/public/routines/increment;int4", DBothButDifferent)]

  -- Change in function args means new function
  addMig_
    "CREATE OR REPLACE FUNCTION increment(i integer, x text) RETURNS integer AS $$\
    \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
    "DROP FUNCTION increment(integer, text)"
    $ ChangeEq
      [ ( "schemas/public/routines/increment;int4,text",
          DExpectedButNotFound
        )
      ]

  -- Change in function args means new function
  addMig_
    "CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS integer AS $$\
    \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
    "DROP FUNCTION increment(text, integer)"
    $ ChangeEq
      [ ( "schemas/public/routines/increment;text,int4",
          DExpectedButNotFound
        )
      ]

  -- Same everything as existing function, just changing return type
  addMig_
    "DROP FUNCTION increment(text, integer); CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS bigint AS $$\
    \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
    "DROP FUNCTION increment(text, integer); CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS integer AS $$\
    \BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;"
    $ ChangeEq
      [ ( "schemas/public/routines/increment;text,int4",
          DBothButDifferent
        )
      ]

  -- SQL bodied functions are also verified
  addMig_
    "CREATE OR REPLACE FUNCTION increment_sql(i integer) RETURNS integer AS $$\
    \SELECT i + 1; \n $$ LANGUAGE sql;"
    "DROP FUNCTION increment_sql(integer)"
    $ ChangeEq
      [ ( "schemas/public/routines/increment_sql;int4",
          DExpectedButNotFound
        )
      ]

  addMig_
    "CREATE OR REPLACE FUNCTION increment_sql(i integer) RETURNS integer AS $$\
    \SELECT i + 2; \n $$ LANGUAGE sql;"
    "CREATE OR REPLACE FUNCTION increment_sql(i integer) RETURNS integer AS $$\
    \SELECT i + 1; \n $$ LANGUAGE sql;"
    $ ChangeEq
      [ ( "schemas/public/routines/increment_sql;int4",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER FUNCTION increment(text, integer) SECURITY DEFINER;"
    "ALTER FUNCTION increment(text, integer) SECURITY INVOKER;"
    $ ChangeEq
      [ ( "schemas/public/routines/increment;text,int4",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER FUNCTION increment(text, integer) PARALLEL RESTRICTED;"
    "ALTER FUNCTION increment(text, integer) PARALLEL UNSAFE;"
    $ ChangeEq
      [ ( "schemas/public/routines/increment;text,int4",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER FUNCTION increment(text, integer) PARALLEL SAFE;"
    "ALTER FUNCTION increment(text, integer) PARALLEL RESTRICTED;"
    $ ChangeEq
      [ ( "schemas/public/routines/increment;text,int4",
          DBothButDifferent
        )
      ]

  addMig_
    "ALTER FUNCTION increment(text, integer) LEAKPROOF;"
    "ALTER FUNCTION increment(text, integer) NOT LEAKPROOF;"
    $ ChangeEq
      [ ( "schemas/public/routines/increment;text,int4",
          DBothButDifferent
        )
      ]

  -- TODO: SECURITY attribute should be verified, but the following fails for some reason.
  -- addMig_ "ALTER FUNCTION increment(text, integer) SECURITY DEFINER;"
  --         "ALTER FUNCTION increment(text, integer) SECURITY INVOKER;"
  --   $ ChangeEq
  --       [("schemas/public/routines/increment;text,int4", DBothButDifferent)]

  -- TRIGGERS
  addMig_
    "ALTER TABLE employee ADD COLUMN name TEXT"
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
        [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name",
            DExpectedButNotFound
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
      [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name",
          DBothButDifferent
        )
      ]

  addMig_
    "DROP TRIGGER employee_old_app_update_column_name ON employee"
    "CREATE TRIGGER employee_old_app_update_column_name\
    \ \n BEFORE UPDATE ON employee\
    \ \n FOR EACH ROW\
    \ \n EXECUTE PROCEDURE employee_name_rename_set_new()"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/triggers/employee_old_app_update_column_name",
          DNotExpectedButFound
        )
      ]

  addMig_
    "ALTER TABLE employee DROP COLUMN employee_name"
    -- Undoing this statement requires recreating a lot of dependent objects..
    -- Also, on Pg <= 12, dropping and readding employee_id in the same ALTER TABLE statement
    -- makes Pg create a new sequence with a different name instead of doing what you might expect
    "ALTER SEQUENCE some_seq OWNED BY NONE; \
    \\nALTER TABLE employee DROP COLUMN birthday, DROP COLUMN deathday, DROP COLUMN name, DROP COLUMN favorite_number, DROP COLUMN employee_id CASCADE;\
    \\nALTER TABLE employee ADD COLUMN employee_name VARCHAR(50) NOT NULL, ADD COLUMN favorite_number numeric(9,3), ADD COLUMN deathday DATE NULL DEFAULT '2100-02-04', \
    \ ADD COLUMN birthday TIMESTAMP, ADD COLUMN employee_id SERIAL PRIMARY KEY, \
    \ ADD COLUMN name TEXT;\
    \\nALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> 'EMPTY');\
    \\nALTER TABLE employee ADD CONSTRAINT employee_unique_name UNIQUE(employee_name);\
    \\nCREATE UNIQUE INDEX unique_employee_idx ON employee (employee_name);\
    \\nALTER TABLE employee_car ADD CONSTRAINT employee_car_employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id);\
    \\nALTER SEQUENCE some_seq OWNED BY employee.employee_id;"
    SomeChange

  addMig_
    "ALTER TABLE employee RENAME COLUMN name TO employee_name"
    "ALTER TABLE employee RENAME COLUMN employee_name TO name"
    SomeChange

  -- VIEWS
  addMig_
    "CREATE OR REPLACE VIEW all_employee_names (employee_name) AS (SELECT employee_name FROM employee)"
    "DROP VIEW all_employee_names"
    $ ChangeEq
      [ ( "schemas/public/views/all_employee_names",
          DExpectedButNotFound
        )
      ]

  addMig_
    "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT employee_name FROM employee)"
    "CREATE OR REPLACE VIEW all_employee_names (employee_name) AS (SELECT employee_name FROM employee)"
    $ ChangeEq
      [("schemas/public/views/all_employee_names", DBothButDifferent)]

  addMigNoChanges_
    "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT employee_name FROM employee)"

  addMig_
    "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT 'Mr. ' || employee_name FROM employee)"
    "CREATE OR REPLACE VIEW all_employee_names (employee_name) WITH (security_barrier=TRUE) AS (SELECT employee_name FROM employee)"
    $ ChangeEq
      [("schemas/public/views/all_employee_names", DBothButDifferent)]

  addMig_
    "ALTER VIEW all_employee_names OWNER TO \"codd-test-user\""
    "ALTER VIEW all_employee_names OWNER TO \"postgres\""
    $ ChangeEq
      [("schemas/public/views/all_employee_names", DBothButDifferent)]

  -- ROW LEVEL SECURITY
  addMig_
    "ALTER TABLE employee ENABLE ROW LEVEL SECURITY"
    "ALTER TABLE employee DISABLE ROW LEVEL SECURITY"
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  addMig_
    "ALTER TABLE employee FORCE ROW LEVEL SECURITY"
    "ALTER TABLE employee NO FORCE ROW LEVEL SECURITY"
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  addMig_
    "ALTER TABLE employee NO FORCE ROW LEVEL SECURITY"
    "ALTER TABLE employee FORCE ROW LEVEL SECURITY"
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  (createPolicy1, dropPolicy) <-
    addMig
      "CREATE POLICY some_policy ON employee USING (employee_name <> 'Some Name');"
      "DROP POLICY some_policy ON employee;"
      $ ChangeEq
        [ ( "schemas/public/tables/employee/policies/some_policy",
            DExpectedButNotFound
          )
        ]

  (dropCreatePolicy2, _) <-
    addMig
      "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee USING (employee_name <> 'Some Other Name');"
      (dropPolicy <> createPolicy1)
      $ ChangeEq
        [ ( "schemas/public/tables/employee/policies/some_policy",
            DBothButDifferent
          )
        ]

  (dropCreatePolicy3, _) <-
    addMig
      "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name');"
      dropCreatePolicy2
      $ ChangeEq
        [ ( "schemas/public/tables/employee/policies/some_policy",
            DBothButDifferent
          )
        ]

  (dropCreatePolicy4, _) <-
    addMig
      "DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name') WITH CHECK (TRUE);"
      dropCreatePolicy3
      $ ChangeEq
        [ ( "schemas/public/tables/employee/policies/some_policy",
            DBothButDifferent
          )
        ]

  let createPolicy5 =
        "CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name') WITH CHECK (TRUE);"
  addMig_
    ("DROP POLICY some_policy ON employee;" <> createPolicy5)
    dropCreatePolicy4
    $ ChangeEq []

  addMig_ "DROP POLICY some_policy ON employee;" createPolicy5 $
    ChangeEq
      [ ( "schemas/public/tables/employee/policies/some_policy",
          DNotExpectedButFound
        )
      ]

  -- ROLES
  (createUnmappedRole, dropUnmappedRole) <-
    addMig "CREATE ROLE any_unmapped_role" "DROP ROLE any_unmapped_role" $
      ChangeEq []
  addMig_ dropUnmappedRole createUnmappedRole $ ChangeEq []
  addMig_
    "CREATE ROLE \"extra-codd-test-user\""
    "DROP ROLE \"extra-codd-test-user\""
    $ ChangeEq [("roles/extra-codd-test-user", DExpectedButNotFound)]

  addMig_
    "ALTER ROLE \"codd-test-user\" SET search_path TO public, pg_catalog"
    "ALTER ROLE \"codd-test-user\" RESET search_path"
    $ ChangeEq [("roles/codd-test-user", DBothButDifferent)]

  addMig_
    "ALTER ROLE \"codd-test-user\" SET default_transaction_isolation='repeatable read'"
    "ALTER ROLE \"codd-test-user\" RESET default_transaction_isolation"
    $ ChangeEq [("roles/codd-test-user", DBothButDifferent)]

  addMig_
    "ALTER ROLE \"codd-test-user\" WITH BYPASSRLS; ALTER ROLE \"codd-test-user\" WITH REPLICATION;"
    "ALTER ROLE \"codd-test-user\" WITH NOBYPASSRLS; ALTER ROLE \"codd-test-user\" WITH NOREPLICATION; "
    $ ChangeEq [("roles/codd-test-user", DBothButDifferent)]

  addMigNoChanges_ "ALTER ROLE \"codd-test-user\" WITH BYPASSRLS"

  -- Database-related permissions affect only roles, not db-settings
  (revokeConnect, grantConnect) <-
    addMig
      "REVOKE CONNECT ON DATABASE \"codd-test-db\" FROM \"codd-test-user\""
      "GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\""
      $ ChangeEq [("roles/codd-test-user", DBothButDifferent)]

  addMig_
    "GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\""
    revokeConnect
    $ ChangeEq [("roles/codd-test-user", DBothButDifferent)]

  addMigNoChanges_ grantConnect

  -- Role membership
  (grantRole, revokeRole) <-
    addMig
      "GRANT \"extra-codd-test-user\" TO \"codd-test-user\""
      "REVOKE \"extra-codd-test-user\" FROM \"codd-test-user\""
      $ ChangeEq [("roles/codd-test-user", DBothButDifferent)]

  addMig_ revokeRole grantRole $
    ChangeEq [("roles/codd-test-user", DBothButDifferent)]

  -- Config attributes
  addMig_
    "ALTER ROLE postgres SET search_path TO public, pg_catalog"
    "ALTER ROLE postgres RESET search_path"
    $ ChangeEq [("roles/postgres", DBothButDifferent)]

  addMig_
    "ALTER ROLE \"codd-test-user\" SET search_path TO DEFAULT"
    "ALTER ROLE \"codd-test-user\" SET search_path TO public, pg_catalog"
    $ ChangeEq [("roles/codd-test-user", DBothButDifferent)]

  addMig_
    "ALTER ROLE postgres SET search_path TO DEFAULT"
    "ALTER ROLE postgres SET search_path TO public, pg_catalog"
    $ ChangeEq [("roles/postgres", DBothButDifferent)]

  addMigNoChanges_ "ALTER ROLE postgres SET search_path TO DEFAULT"

  -- PERMISSIONS
  -- For tables

  -- Owner of the table implicitly has all privileges by default
  addMig_ "GRANT ALL ON TABLE employee TO postgres" "SELECT 1;" $ ChangeEq []

  addMig_
    "GRANT SELECT ON TABLE employee TO \"codd-test-user\""
    "REVOKE SELECT ON TABLE employee FROM \"codd-test-user\""
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  addMig_
    "GRANT INSERT ON TABLE employee TO \"codd-test-user\""
    "REVOKE INSERT ON TABLE employee FROM \"codd-test-user\""
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  addMig_
    "GRANT DELETE ON TABLE employee TO \"codd-test-user\""
    "REVOKE DELETE ON TABLE employee FROM \"codd-test-user\""
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  -- For sequences
  addMig_
    "GRANT SELECT ON SEQUENCE employee_employee_id_seq TO \"codd-test-user\""
    "REVOKE SELECT ON SEQUENCE employee_employee_id_seq FROM \"codd-test-user\";"
    $ ChangeEq
      [ ( "schemas/public/sequences/employee_employee_id_seq",
          DBothButDifferent
        )
      ]

  -- At this point codd-test-user has S+I+D permissions on the employee table
  -- Order of granting does not matter, nor do grantors
  addMig_
    "REVOKE ALL ON TABLE employee FROM \"codd-test-user\""
    "GRANT SELECT,INSERT,DELETE ON TABLE employee TO \"codd-test-user\""
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  addMig_
    "GRANT INSERT, DELETE ON TABLE employee TO \"codd-test-user\";"
    "REVOKE INSERT, DELETE ON TABLE employee FROM \"codd-test-user\";"
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  -- At this point codd-test-user has I+D permissions on the employee table
  addMig_
    "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT INSERT, DELETE ON TABLE employee TO \"codd-test-user\";"
    "GRANT INSERT, DELETE ON TABLE employee TO \"codd-test-user\";"
    $ ChangeEq []

  -- At this point codd-test-user has I+D permissions on the employee table
  addMig_
    "GRANT ALL ON TABLE employee TO \"codd-test-user\""
    "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT INSERT, DELETE ON TABLE employee TO \"codd-test-user\""
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  addMig_
    "GRANT ALL ON TABLE employee TO \"extra-codd-test-user\""
    "REVOKE ALL ON TABLE employee FROM \"extra-codd-test-user\""
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  addMigNoChanges_
    "REVOKE ALL ON TABLE employee FROM \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"extra-codd-test-user\""

  addMig_
    "GRANT ALL ON TABLE employee TO PUBLIC"
    "REVOKE ALL ON TABLE employee FROM PUBLIC; GRANT ALL ON TABLE employee TO \"codd-test-user\"; GRANT ALL ON TABLE employee TO \"extra-codd-test-user\";"
    $ ChangeEq
      [("schemas/public/tables/employee/objrep", DBothButDifferent)]

  -- Permissions of unmapped role don't affect hashing
  (createUnmappedRole1AndGrant, dropUnmappedRole1) <-
    addMig
      "CREATE ROLE unmapped_role1; GRANT ALL ON TABLE employee TO unmapped_role1; GRANT ALL ON SEQUENCE employee_employee_id_seq TO unmapped_role1; GRANT ALL ON all_employee_names TO unmapped_role1"
      "DROP OWNED BY unmapped_role1; DROP ROLE unmapped_role1"
      $ ChangeEq []

  addMig_ dropUnmappedRole1 createUnmappedRole1AndGrant $ ChangeEq []

  -- CREATING UNMAPPED AND MAPPED SCHEMAS
  addMig_ "CREATE SCHEMA unmappedschema" "DROP SCHEMA unmappedschema" $
    ChangeEq []
  addMig_ "DROP SCHEMA unmappedschema" "CREATE SCHEMA unmappedschema" $
    ChangeEq []
  (createMappedSchema, dropMappedSchema) <-
    addMig
      "CREATE SCHEMA \"codd-extra-mapped-schema\""
      "DROP SCHEMA \"codd-extra-mapped-schema\""
      $ ChangeEq
        [ ( "schemas/codd-extra-mapped-schema/objrep",
            DExpectedButNotFound
          )
        ]

  addMig_ dropMappedSchema createMappedSchema $
    ChangeEq
      [("schemas/codd-extra-mapped-schema/objrep", DNotExpectedButFound)]

  -- DATABASE SETTINGS
  -- Default privileges change nothing
  let grantConnectPublic =
        "GRANT CONNECT ON DATABASE \"codd-test-db\" TO public"
  addMigNoChanges_ grantConnectPublic

  -- Privileges for public affect db-settings, not some role
  addMig_
    "REVOKE CONNECT ON DATABASE \"codd-test-db\" FROM public;"
    grantConnectPublic
    $ ChangeEq [("db-settings", DBothButDifferent)]

  -- codd-test-user owns codd-test-db, so it already has permissions to connect to it
  addMigNoChanges_
    "GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\""

  addMig_
    "ALTER DATABASE \"codd-test-db\" SET default_transaction_isolation TO 'serializable';"
    "ALTER DATABASE \"codd-test-db\" RESET default_transaction_isolation;"
    $ ChangeEq [("db-settings", DBothButDifferent)]

  -- Session settings don't change the schema
  addMig_
    "SET default_transaction_isolation TO 'serializable';"
    "RESET default_transaction_isolation;"
    $ ChangeEq []

  -- COLLATIONS
  (createCutf8Coll, dropColl) <-
    addMig
      "CREATE COLLATION new_collation (locale = 'en_GB.utf-8');"
      "DROP COLLATION new_collation;"
      $ ChangeEq
        [ ( "schemas/public/collations/new_collation",
            DExpectedButNotFound
          )
        ]
  (dropCreateEnUSColl, _) <-
    addMig
      ( dropColl
          <> " CREATE COLLATION new_collation (locale = 'en_US.utf-8');"
      )
      (dropColl <> createCutf8Coll)
      $ ChangeEq
        [ ( "schemas/public/collations/new_collation",
            DBothButDifferent
          )
        ]
  addMig_
    ( dropColl
        <> " CREATE COLLATION new_collation (provider = icu, locale = 'de-u-co-phonebk');"
    )
    dropCreateEnUSColl
    $ ChangeEq
      [("schemas/public/collations/new_collation", DBothButDifferent)]

  (createCol2, _) <- addMig "CREATE COLLATION new_collation_2 (provider = icu, locale = 'de-u-co-phonebk', deterministic = false);" "DROP COLLATION new_collation_2" $ ChangeEq [("schemas/public/collations/new_collation_2", DExpectedButNotFound)]
  addMig_ "DROP COLLATION new_collation_2; CREATE COLLATION new_collation_2 (provider = icu, locale = 'de-u-co-phonebk', deterministic = true);" ("DROP COLLATION new_collation_2; " <> createCol2) $ ChangeEq [("schemas/public/collations/new_collation_2", DBothButDifferent)]

  addMig_
    "ALTER TABLE employee ADD COLUMN employee_surname TEXT;"
    "ALTER TABLE employee DROP COLUMN employee_surname;"
    SomeChange

  addMig_
    "ALTER TABLE employee ALTER COLUMN employee_surname TYPE TEXT COLLATE \"new_collation\";"
    "ALTER TABLE employee ALTER COLUMN employee_surname TYPE TEXT COLLATE \"default\";"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/employee_surname",
          DBothButDifferent
        )
      ]

  -- TYPES

  -- Enum Types
  (createExp, dropExp) <-
    addMig
      "CREATE TYPE experience AS ENUM ('junior', 'senior');"
      "DROP TYPE experience;"
      $ ChangeEq
        [("schemas/public/types/experience", DExpectedButNotFound)]

  addMig_
    "-- codd: no-txn\n\
    \ALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';"
    (dropExp <> createExp)
    $ ChangeEq [("schemas/public/types/experience", DBothButDifferent)]

  -- Composite types
  addMig_ "CREATE TYPE complex AS (a double precision);" "DROP TYPE complex;" $
    ChangeEq [("schemas/public/types/complex", DExpectedButNotFound)]
  addMig_
    "ALTER TYPE complex ADD ATTRIBUTE b double precision;"
    "ALTER TYPE complex DROP ATTRIBUTE b"
    $ ChangeEq [("schemas/public/types/complex", DBothButDifferent)]

  addMig_
    "ALTER TYPE complex ALTER ATTRIBUTE b SET DATA TYPE text;"
    "ALTER TYPE complex ALTER ATTRIBUTE b SET DATA TYPE double precision;"
    $ ChangeEq [("schemas/public/types/complex", DBothButDifferent)]

  addMig_
    "ALTER TYPE complex ALTER ATTRIBUTE b TYPE text COLLATE new_collation;"
    "ALTER TYPE complex ALTER ATTRIBUTE b TYPE text COLLATE \"default\";"
    $ ChangeEq [("schemas/public/types/complex", DBothButDifferent)]

  addMig_
    "ALTER TYPE complex ADD ATTRIBUTE c employee;"
    "ALTER TYPE complex DROP ATTRIBUTE c;"
    $ ChangeEq [("schemas/public/types/complex", DBothButDifferent)]

  -- We don't want the type to change when the table changes
  -- because it'd be unnecessarily verbose.
  addMig_
    "ALTER TABLE employee ADD COLUMN anycolumn TEXT;"
    "ALTER TABLE employee DROP COLUMN anycolumn;"
    $ ChangeEq
      [ ( "schemas/public/tables/employee/cols/anycolumn",
          DExpectedButNotFound
        ),
        ( "schemas/public/tables/employee/objrep",
          DBothButDifferent
        )
      ]

  -- Range types
  (createFloatRange1, dropFloatRange) <-
    addMig
      "CREATE TYPE floatrange AS RANGE (subtype = float8,subtype_diff = float8mi);"
      "DROP TYPE floatrange;"
      $ ChangeEq
        -- Range and multirange constructor/types functions created by PG too
        ( [ ("schemas/public/types/floatrange", DExpectedButNotFound),
            ( "schemas/public/routines/floatrange;float8,float8",
              DExpectedButNotFound
            ),
            ( "schemas/public/routines/floatrange;float8,float8,text",
              DExpectedButNotFound
            )
          ]
            ++ if pgVersion >= 14
              then
                [ ( "schemas/public/types/floatmultirange",
                    DExpectedButNotFound
                  ),
                  ( "schemas/public/routines/floatmultirange;",
                    DExpectedButNotFound
                  ),
                  ( "schemas/public/routines/floatmultirange;floatrange",
                    DExpectedButNotFound
                  ),
                  ( "schemas/public/routines/floatmultirange;_floatrange",
                    DExpectedButNotFound
                  )
                ]
              else []
        )

  addMig_
    "CREATE FUNCTION time_subtype_diff(x time, y time) RETURNS float8 AS 'SELECT EXTRACT(EPOCH FROM (x - y))' LANGUAGE sql STRICT IMMUTABLE;"
    "DROP FUNCTION time_subtype_diff"
    SomeChange

  -- Change of subtype
  addMig_
    ( dropFloatRange
        <> "CREATE TYPE floatrange AS RANGE (subtype = time,subtype_diff = time_subtype_diff);"
    )
    (dropFloatRange <> createFloatRange1)
    $ ChangeEq
      [ ("schemas/public/types/floatrange", DBothButDifferent),
        -- Constructor functions:
        ( "schemas/public/routines/floatrange;time,time",
          DExpectedButNotFound
        ),
        ( "schemas/public/routines/floatrange;time,time,text",
          DExpectedButNotFound
        ),
        ( "schemas/public/routines/floatrange;float8,float8",
          DNotExpectedButFound
        ),
        ( "schemas/public/routines/floatrange;float8,float8,text",
          DNotExpectedButFound
        )
      ]
  -- Multirange constructors are unchanged since the type of their arguments
  -- is exclusively `floatrange`.

  -- Domain types
  addMig_
    "CREATE DOMAIN non_empty_text TEXT NOT NULL CHECK (VALUE != '');"
    "DROP DOMAIN non_empty_text;"
    $ ChangeEq
      [("schemas/public/types/non_empty_text", DExpectedButNotFound)]

  addMig_
    "ALTER DOMAIN non_empty_text SET DEFAULT 'empty';"
    "ALTER DOMAIN non_empty_text DROP DEFAULT;"
    $ ChangeEq [("schemas/public/types/non_empty_text", DBothButDifferent)]

  addMig_
    "ALTER DOMAIN non_empty_text DROP NOT NULL;"
    "ALTER DOMAIN non_empty_text SET NOT NULL;"
    $ ChangeEq [("schemas/public/types/non_empty_text", DBothButDifferent)]

  (addTypeCheck, dropTypeCheck) <-
    addMig
      "ALTER DOMAIN non_empty_text ADD CONSTRAINT new_constraint CHECK(TRIM(VALUE) != '') NOT VALID;"
      "ALTER DOMAIN non_empty_text DROP CONSTRAINT new_constraint;"
      $ ChangeEq
        [("schemas/public/types/non_empty_text", DBothButDifferent)]

  addMig_
    "ALTER DOMAIN non_empty_text VALIDATE CONSTRAINT new_constraint;"
    (dropTypeCheck <> addTypeCheck)
    $ ChangeEq [("schemas/public/types/non_empty_text", DBothButDifferent)]

  addMig_
    "ALTER DOMAIN non_empty_text RENAME CONSTRAINT new_constraint TO new_constraint_2;"
    "ALTER DOMAIN non_empty_text RENAME CONSTRAINT new_constraint_2 TO new_constraint;"
    $ ChangeEq [("schemas/public/types/non_empty_text", DBothButDifferent)]

  -- Change type permissions/ownership.
  addMig_
    "ALTER DOMAIN non_empty_text OWNER TO \"codd-test-user\""
    "ALTER DOMAIN non_empty_text OWNER TO postgres;"
    $ ChangeEq [("schemas/public/types/non_empty_text", DBothButDifferent)]

  addMig_
    "ALTER DOMAIN non_empty_text OWNER TO postgres"
    "ALTER DOMAIN non_empty_text OWNER TO \"codd-test-user\";"
    $ ChangeEq [("schemas/public/types/non_empty_text", DBothButDifferent)]

  addMig_
    "GRANT ALL ON DOMAIN non_empty_text TO \"codd-test-user\""
    "REVOKE ALL ON DOMAIN non_empty_text FROM \"codd-test-user\";"
    $ ChangeEq [("schemas/public/types/non_empty_text", DBothButDifferent)]

  -- Extended statistics
  (createStats1, _) <-
    addMig
      "CREATE STATISTICS test_stat (dependencies,ndistinct) ON employee_id, employee_name FROM employee"
      "DROP STATISTICS test_stat"
      $ ChangeEq [("schemas/public/tables/employee/statistics/test_stat", DExpectedButNotFound)]

  (dropAndCreateStats2, _) <-
    addMig
      -- Changing kinds
      "DROP STATISTICS test_stat; CREATE STATISTICS test_stat (dependencies,mcv) ON employee_name, employee_id FROM employee"
      ("DROP STATISTICS test_stat; " <> createStats1)
      $ ChangeEq [("schemas/public/tables/employee/statistics/test_stat", DBothButDifferent)]

  when (pgVersion >= 14)
    $ addMig_
      -- Using an expression
      "DROP STATISTICS test_stat; CREATE STATISTICS test_stat (dependencies,mcv) ON LOWER(employee_name), employee_id FROM employee"
      dropAndCreateStats2
    $ ChangeEq [("schemas/public/tables/employee/statistics/test_stat", DBothButDifferent)]

  -- CRUD
  addMig_
    "INSERT INTO employee (employee_name) VALUES ('Marcelo')"
    "DELETE FROM employee WHERE employee_name='Marcelo'"
    $ ChangeEq []

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_ : xs) = lastMaybe xs

newtype NumMigsToReverse = NumMigsToReverse Int deriving stock (Show)

instance Arbitrary NumMigsToReverse where
  arbitrary =
    NumMigsToReverse
      -- TODO: We know the number of migrations does not depend on the server's version,
      -- but this is really ugly. We should avoid this generate random input without
      -- an instance of Arbitrary instead.
      <$> chooseBoundedIntegral
        (5, length (migrationsAndRepChangeText 0 0) - 1)

-- | This type includes each migration with their expected changes and hashes after applied. Hashes before the first migration are not included.
newtype AccumChanges m = AccumChanges [((AddedSqlMigration m, DbChange), DbRep)]

spec :: Spec
spec = do
  describe "DbDependentSpecs" $ do
    aroundFreshDatabase $
      it "User search_path does not affect representations" $
        \emptyTestDbInfo -> do
          createStuffMig <-
            AddedSqlMigration
              <$> ( either (error "Could not parse SQL migration") id
                      <$> parseSqlMigrationIO
                        "1900-01-01-00-00-00-create-stuff.sql"
                        ( PureStream $
                            Streaming.yield
                              "-- Columns with default values are SQL expressions that should reference the schema's name.\
                              \\n--  And SQL expressions are what we need for this test, since they change when the search_path changes.\
                              \\nCREATE SCHEMA nsp1; CREATE SCHEMA nsp2; CREATE TABLE nsp1.tbl1 (col1 SERIAL PRIMARY KEY); CREATE TABLE nsp2.tbl2 (col2 SERIAL PRIMARY KEY);"
                        )
                  )
              <*> pure (getIncreasingTimestamp 0)
          let setSearchPath conn sp =
                liftIO $
                  void $
                    DB.query @(DB.Only Text) @(DB.Only Text)
                      conn
                      "SELECT set_config('search_path', ?, false)"
                      (DB.Only sp)
          (reps1, reps2, reps3, reps4) <-
            runCoddLogger $
              applyMigrationsNoCheck
                emptyTestDbInfo
                (Just [hoistAddedSqlMigration lift createStuffMig])
                testConnTimeout
                ( \conn -> do
                    setSearchPath conn "nsp1"
                    reps1 <-
                      readRepresentationsFromDbWithSettings
                        emptyTestDbInfo
                        conn
                    setSearchPath conn "nsp2"
                    reps2 <-
                      readRepresentationsFromDbWithSettings
                        emptyTestDbInfo
                        conn
                    setSearchPath conn "nsp1, nsp2"
                    reps3 <-
                      readRepresentationsFromDbWithSettings
                        emptyTestDbInfo
                        conn
                    setSearchPath conn "public"
                    reps4 <-
                      readRepresentationsFromDbWithSettings
                        emptyTestDbInfo
                        conn
                    pure (reps1, reps2, reps3, reps4)
                )
          reps1 `shouldBe` reps2
          reps2 `shouldBe` reps3
          reps3 `shouldBe` reps4

    aroundFreshDatabase $
      it "Strict and Lax collation representations differ" $
        \emptyTestDbInfo -> do
          -- It'd be nice to test that different libc/libicu versions make hashes
          -- change, but I don't know how to do that sanely.
          -- So we just test the code paths and make sure hashes differ.
          let strictCollDbInfo =
                emptyTestDbInfo
                  { schemaAlgoOpts =
                      SchemaAlgo
                        { strictCollations = True,
                          strictRangeCtorPrivs = False,
                          ignoreColumnOrder = False
                        }
                  }
          createCollMig <-
            AddedSqlMigration
              <$> ( either (error "Could not parse SQL migration") id
                      <$> parseSqlMigrationIO
                        "1900-01-01-00-00-00-create-coll.sql"
                        ( PureStream $
                            Streaming.yield
                              "CREATE COLLATION new_collation (provider = icu, locale = 'de-u-co-phonebk');"
                        )
                  )
              <*> pure (getIncreasingTimestamp 0)
          (laxCollHashes, strictCollHashes) <-
            runCoddLogger $
              applyMigrationsNoCheck
                emptyTestDbInfo
                (Just [hoistAddedSqlMigration lift createCollMig])
                testConnTimeout
                ( \conn ->
                    (,)
                      <$> readRepresentationsFromDbWithSettings
                        emptyTestDbInfo
                        conn
                      <*> readRepresentationsFromDbWithSettings
                        strictCollDbInfo
                        conn
                )
          laxCollHashes `shouldNotBe` strictCollHashes

    aroundFreshDatabase $
      it "Strict range constructor ownership" $
        \emptyTestDbInfo -> do
          let strictRangeDbInfo =
                emptyTestDbInfo
                  { schemaAlgoOpts =
                      SchemaAlgo
                        { strictCollations = False,
                          strictRangeCtorPrivs = True,
                          ignoreColumnOrder = False
                        }
                  }
          createMig <-
            AddedSqlMigration
              <$> ( either (error "Could not parse SQL migration") id
                      <$> parseSqlMigrationIO
                        "1900-01-01-00-00-00-create-range-and-other-function.sql"
                        ( PureStream $
                            Streaming.yield
                              "CREATE TYPE floatrange AS RANGE (subtype = float8,subtype_diff = float8mi); \
                              \\n CREATE FUNCTION time_subtype_diff(x time, y time) RETURNS float8 AS 'SELECT EXTRACT(EPOCH FROM (x - y))' LANGUAGE sql STRICT IMMUTABLE;"
                        )
                  )
              <*> pure (getIncreasingTimestamp 0)
          (PgMajorVersion pgVersion, _) <-
            withConnection
              (migsConnString emptyTestDbInfo)
              testConnTimeout
              queryServerMajorAndFullVersion
          (laxRangeHashes, strictRangeHashes) <-
            runCoddLogger $
              applyMigrationsNoCheck
                emptyTestDbInfo
                (Just [hoistAddedSqlMigration lift createMig])
                testConnTimeout
                ( \conn ->
                    (,)
                      <$> readRepresentationsFromDbWithSettings
                        emptyTestDbInfo
                        conn
                      <*> readRepresentationsFromDbWithSettings
                        strictRangeDbInfo
                        conn
                )

          -- The time_subtype_diff function shouldn't have its representation change,
          -- but the constructors of floatrange and floatmultirange should.
          ( schemaDifferences laxRangeHashes strictRangeHashes
              <&> simplifyDiff
            )
            `shouldBe` Map.fromList
              ( [ ( "schemas/public/routines/floatrange;float8,float8",
                    DBothButDifferent
                  ),
                  ( "schemas/public/routines/floatrange;float8,float8,text",
                    DBothButDifferent
                  )
                ]
                  ++ if pgVersion >= 14
                    then
                      [ ( "schemas/public/routines/floatmultirange;",
                          DBothButDifferent
                        ),
                        ( "schemas/public/routines/floatmultirange;floatrange",
                          DBothButDifferent
                        ),
                        ( "schemas/public/routines/floatmultirange;_floatrange",
                          DBothButDifferent
                        )
                      ]
                    else []
              )

    aroundFreshDatabase $
      it "ignore-column-order setting" $
        \emptyTestDbInfo -> do
          createMig <-
            AddedSqlMigration
              <$> ( either (error "Could not parse SQL migration") id
                      <$> parseSqlMigrationIO
                        "1900-01-01-00-00-00-ignore-col-order-1.sql"
                        ( PureStream $
                            Streaming.yield
                              "CREATE TABLE othertbl(col2 INT PRIMARY KEY);\
                              \CREATE TABLE tbl(col1 INT, col2 SERIAL PRIMARY KEY CHECK (col2 > 0) REFERENCES othertbl(col2));\
                              \CREATE UNIQUE INDEX someidx ON tbl(col2);"
                        )
                        -- TODO: Other dependent objects like triggers, custom locales and whatnot
                  )
              <*> pure (getIncreasingTimestamp 0)
          let ignColOrderDbInfo =
                emptyTestDbInfo
                  { schemaAlgoOpts =
                      SchemaAlgo
                        { strictCollations = False,
                          strictRangeCtorPrivs = False,
                          ignoreColumnOrder = True
                        }
                  }
          initialHashes <-
            runCoddLogger $
              applyMigrationsNoCheck
                ignColOrderDbInfo
                (Just [hoistAddedSqlMigration lift createMig])
                testConnTimeout
                (readRepresentationsFromDbWithSettings ignColOrderDbInfo)

          dropCol1Mig <-
            AddedSqlMigration
              <$> ( either (error "Could not parse SQL migration 2") id
                      <$> parseSqlMigrationIO
                        "1900-01-01-00-00-01-ignore-col-order-2.sql"
                        ( PureStream $
                            Streaming.yield
                              "ALTER TABLE tbl DROP COLUMN col1;"
                        )
                  )
              <*> pure (getIncreasingTimestamp 1)

          afterDropHashes <-
            runCoddLogger $
              applyMigrationsNoCheck
                ignColOrderDbInfo
                ( Just $
                    map
                      (hoistAddedSqlMigration lift)
                      [createMig, dropCol1Mig]
                )
                testConnTimeout
                (readRepresentationsFromDbWithSettings ignColOrderDbInfo)

          -- Only the removed column should have its representation file removed.
          -- The other column and all dependent objects should not change one bit.
          ( afterDropHashes
              `schemaDifferences` initialHashes
              <&> simplifyDiff
            )
            `shouldBe` Map.fromList
              [ ( "schemas/public/tables/tbl/cols/col1",
                  DExpectedButNotFound
                )
              ]

    aroundFreshDatabase $ it "Schema selection" $ \emptyTestDbInfo -> do
      let nonInternalSchemasDbInfo =
            emptyTestDbInfo
              { namespacesToCheck = AllNonInternalSchemas
              }
          publicSchemaDbInfo =
            emptyTestDbInfo
              { namespacesToCheck = IncludeSchemas ["public"]
              }
          emptySchemasDbInfo =
            emptyTestDbInfo {namespacesToCheck = IncludeSchemas []}
          nonExistingAndCatalogSchemasDbInfo =
            emptyTestDbInfo
              { namespacesToCheck =
                  IncludeSchemas
                    ["non-existing-schema", "pg_catalog"]
              }
          getSchemaHashes dbinfo = do
            DbRep _ hashes _ <-
              runCoddLogger $
                applyMigrationsNoCheck
                  dbinfo
                  Nothing
                  testConnTimeout
                  (readRepresentationsFromDbWithSettings dbinfo)
            pure $ Map.keys hashes

      -- 1. We should not see pg_catalog in any verified object, but "public"
      -- should be there as the only schema. The same for the one that includes "public"
      -- explicitly
      nonInternalSchemas <- getSchemaHashes nonInternalSchemasDbInfo
      publicSchemas <- getSchemaHashes publicSchemaDbInfo
      nonInternalSchemas `shouldBe` [ObjName "public"]
      publicSchemas `shouldBe` [ObjName "public"]

      -- 2. No schema hashes at all for empty list of schemas
      emptySchemas <- getSchemaHashes emptySchemasDbInfo
      emptySchemas `shouldBe` []

      -- 3. Non-existing schema ignored and internal pg_catalog listed
      pgCatSchemas <- getSchemaHashes nonExistingAndCatalogSchemasDbInfo
      pgCatSchemas `shouldBe` [ObjName "pg_catalog"]

    aroundFreshDatabase
      $ it
        "Restoring a pg_dump yields the same schema as applying original migrations"
      $ \emptyDbInfo -> do
        let connInfo = migsConnString emptyDbInfo
        (pgVersion, pgFullVersion) <-
          withConnection
            connInfo
            testConnTimeout
            queryServerMajorAndFullVersion

        -- We also use migrations of the "Accurate and reversible representation changes" test
        -- because that's the most complete set of database objects we have in our test codebase.
        -- But we append other migrations we know to be problematic to that set.
        bunchOfOtherMigs <-
          map (unMU . fst . hoistMU lift)
            <$> migrationsAndRepChange pgVersion pgFullVersion
        problematicMigs <-
          map (hoistAddedSqlMigration lift)
            <$> migrationsForPgDumpRestoreTest
        let allMigs = bunchOfOtherMigs ++ problematicMigs
        expectedSchema <-
          runCoddLogger $
            applyMigrationsNoCheck
              emptyDbInfo
              (Just allMigs)
              testConnTimeout
              (readRepresentationsFromDbWithSettings emptyDbInfo)

        -- Take the pg_dump and drop the database
        pg_dump_output <-
          finallyDrop (Text.pack $ connectDatabase connInfo) $ do
            (pg_dump_exitCode, pg_dump_output) <-
              readProcessStdout $
                shell $
                  "pg_dump --create -d \""
                    ++ connectDatabase connInfo
                    ++ "\""
            pg_dump_exitCode `shouldBe` ExitSuccess
            pure pg_dump_output

        -- Apply the dump with psql and check schemas match
        psqlExitCode <-
          runProcess $
            setStdin (byteStringInput pg_dump_output) $
              shell "psql -d postgres"
        psqlExitCode `shouldBe` ExitSuccess
        schemaAfterRestore <-
          runCoddLogger $
            withConnection connInfo testConnTimeout $
              readRepsFromDbWithNewTxn emptyDbInfo
        schemaAfterRestore `shouldBe` expectedSchema

    describe "Schema verification tests" $ do
      modifyMaxSuccess (const 3) $ -- This is a bit heavy on CI but this test is too important
        aroundFreshDatabase $
          it "Accurate and reversible representation changes" $
            \emptyDbInfo2 -> property $ \(NumMigsToReverse num) -> do
              let emptyDbInfo =
                    emptyDbInfo2
                      { namespacesToCheck =
                          IncludeSchemas
                            ["public", "codd-extra-mapped-schema"]
                      }
                  connInfo = migsConnString emptyDbInfo
                  getHashes sett =
                    runCoddLogger $
                      withConnection
                        connInfo
                        testConnTimeout
                        (readRepsFromDbWithNewTxn sett)
              (pgVersion, pgFullVersion) <-
                withConnection
                  connInfo
                  testConnTimeout
                  queryServerMajorAndFullVersion
              allMigsAndExpectedChanges <-
                map (hoistMU lift)
                  <$> migrationsAndRepChange pgVersion pgFullVersion
              hashBeforeEverything <- getHashes emptyDbInfo
              ( _,
                AccumChanges applyHistory,
                hashesAndUndo ::
                  [ ( DbRep,
                      Maybe Text
                    )
                  ]
                ) <-
                forwardApplyMigs
                  0
                  hashBeforeEverything
                  emptyDbInfo
                  allMigsAndExpectedChanges
              let hashAfterAllMigs =
                    maybe
                      hashBeforeEverything
                      snd
                      (lastMaybe applyHistory)

              -- 1. After applying all migrations, let's make sure we're actually applying
              -- them by fetching some data..
              ensureMarceloExists connInfo

              -- 2. Now undo part or all of the SQL migrations and check that
              -- schemas match each step of the way in reverse!
              liftIO $
                putStrLn $
                  "Undoing migrations in reverse. Num = "
                    <> show num
              hashesAfterEachUndo <-
                forM (take num $ reverse hashesAndUndo) $
                  \(expectedHashesAfterUndo, mUndoSql) ->
                    case mUndoSql of
                      Nothing -> pure expectedHashesAfterUndo
                      Just undoSql -> do
                        void
                          $ runCoddLogger
                          $ withConnection
                            connInfo
                            testConnTimeout
                          $ \conn ->
                            Streaming.effects
                              $ applyStatementStream
                                conn
                              $ forceStreamConcurrently 1
                              $ ( \case
                                    (WellParsedSql sqlStream) ->
                                      sqlStream
                                    _ -> error "Not WellParsedSql"
                                )
                                ( mkValidSql
                                    undoSql
                                )
                        hashesAfterUndo <-
                          getHashes
                            emptyDbInfo
                        let diff =
                              schemaDifferences
                                hashesAfterUndo
                                expectedHashesAfterUndo
                        (undoSql, diff)
                          `shouldBe` (undoSql, Map.empty)
                        -- What follows is just a sanity check
                        (undoSql, hashesAfterUndo)
                          `shouldBe` ( undoSql,
                                       expectedHashesAfterUndo
                                     )
                        pure hashesAfterUndo
              void $
                withConnection
                  connInfo
                  testConnTimeout
                  ( \conn ->
                      DB.execute
                        conn
                        "WITH reversedMigs (name) AS (SELECT name FROM codd.sql_migrations ORDER BY migration_timestamp DESC LIMIT ?) DELETE FROM codd.sql_migrations USING reversedMigs WHERE reversedMigs.name=sql_migrations.name"
                        (DB.Only num)
                  )

              let hashesAfterUndo =
                    fromMaybe
                      hashAfterAllMigs
                      (lastMaybe hashesAfterEachUndo)

              -- 3. Finally, reapply every migration that was reversed.
              -- This may look unnecessary, but if a created object (e.g. a column) is not dropped
              -- on the way forward, we might not get a change to the effects of re-adding it,
              -- which manifests as a dead but unusable `attnum` in pg_attribute.
              liftIO $ putStrLn "Re-applying migrations"
              void $
                forwardApplyMigs
                  (length allMigsAndExpectedChanges - num)
                  hashesAfterUndo
                  emptyDbInfo
                  allMigsAndExpectedChanges
              ensureMarceloExists connInfo
  where
    forwardApplyMigs numMigsAlreadyApplied hashBeforeEverything dbInfo allMigsAndExpectedChanges =
      foldM
        ( \(hashSoFar, AccumChanges appliedMigsAndCksums, hundo) (MU nextMig undoSql, expectedChanges) ->
            do
              let appliedMigs = map (fst . fst) appliedMigsAndCksums
                  newMigs = appliedMigs ++ [nextMig]
              dbHashesAfterMig <-
                runCoddLogger $
                  applyMigrationsNoCheck
                    dbInfo
                    (Just newMigs)
                    testConnTimeout
                    (readRepresentationsFromDbWithSettings dbInfo)
              -- migText <- parsedSqlText <$> migrationSql (addedSqlMig nextMig)
              let diff = schemaDifferences hashSoFar dbHashesAfterMig
              case expectedChanges of
                ChangeEq c -> do
                  (simplifyDiff <$> diff) `shouldBe` Map.fromList c
                  -- The check below is just a safety net in case "schemaDifferences" has a problem in its implementation
                  if null c
                    then hashSoFar `shouldBe` dbHashesAfterMig
                    else hashSoFar `shouldNotBe` dbHashesAfterMig
                SomeChange -> do
                  diff `shouldNotBe` Map.empty
                  -- The check below is just a safety net in case "schemaDifferences" has a problem in its implementation
                  hashSoFar `shouldNotBe` dbHashesAfterMig

              return
                ( dbHashesAfterMig,
                  AccumChanges $
                    appliedMigsAndCksums
                      ++ [((nextMig, expectedChanges), dbHashesAfterMig)],
                  hundo ++ [(hashSoFar, undoSql)]
                )
        )
        (hashBeforeEverything, AccumChanges [], [])
        (drop numMigsAlreadyApplied allMigsAndExpectedChanges)
    ensureMarceloExists connInfo =
      withConnection
        connInfo
        testConnTimeout
        ( \conn ->
            unsafeQuery1
              conn
              "SELECT COUNT(*) employee_name FROM employee WHERE employee_name='Marcelo'"
              ()
        )
        `shouldReturn` DB.Only (1 :: Int)
