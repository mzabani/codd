module DbDependentSpecs.HashingSpec where

import Codd (applyMigrations)
import Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration)
import Codd.Environment (superUserInAppDatabaseConnInfo)
import Codd.Hashing (readHashesFromDatabaseWithSettings)
import Codd.Internal (withConnection)
import Codd.Parsing (toMigrationTimestamp)
import Codd.Types (CoddSettings(..), SqlMigration(..), AddedSqlMigration(..))
import Control.Monad (when, void, foldM)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.List (nubBy)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import DbUtils (aroundFreshDatabase, getIncreasingTimestamp)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple (ConnectInfo(..))
import qualified Database.PostgreSQL.Simple.Time as DB
import Data.Text (unpack)
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)

migrationsAndHashChange :: [(AddedSqlMigration, Bool)]
migrationsAndHashChange =
    zipWith (\(t, c) i -> 
        (AddedSqlMigration SqlMigration {
            migrationName = show i <> "-migration.sql"
            , nonDestructiveSql = Just t
            , nonDestructiveForce = True
            , nonDestructiveInTxn = True
            , destructiveSql = Nothing
            , destructiveInTxn = True
        } (getIncreasingTimestamp i), c))
        migs
        (map fromInteger [0..]) -- This would be a list of NominalDiffTime, which would have 10^-12s resolution and fail in the DB
    where
        migs = [
            -- TABLES AND COLUMNS
            ("CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, employee_name TEXT)", True)
            , ("ALTER TABLE employee ALTER COLUMN employee_name SET NOT NULL", True)
            , ("ALTER TABLE employee ADD COLUMN birthday DATE; ALTER TABLE employee ADD COLUMN deathday DATE;", True)
            , ("ALTER TABLE employee DROP COLUMN birthday; ALTER TABLE employee DROP COLUMN deathday;"
            <> "ALTER TABLE employee ADD COLUMN deathday DATE; ALTER TABLE employee ADD COLUMN birthday DATE;", False)
            -- ^ The migration above is just changing the order of columns and should not affect hashing!

            , ("ALTER TABLE employee ALTER COLUMN birthday TYPE TIMESTAMP;", True)
            , ("ALTER TABLE employee ADD COLUMN IF NOT EXISTS birthday TIMESTAMP;", False)
            , ("ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';", True)
            , ("ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-03';", False)
            , ("ALTER TABLE employee ALTER COLUMN deathday SET DEFAULT '2100-02-04';", True)

            , ("ALTER TABLE employee DROP COLUMN employee_id; ALTER TABLE employee ADD COLUMN employee_id SERIAL PRIMARY KEY;", True)
            -- ^ Recreating a sequence should not change our schema hash.

            -- SEQUENCES
            , ("CREATE SEQUENCE some_seq MINVALUE 1 MAXVALUE 100", True)
            -- ^ MINVALUE and MAXVALUE that fit other types so we are sure changing just the seq. type has an effect
            , ("ALTER SEQUENCE some_seq AS smallint", True)
            , ("ALTER SEQUENCE some_seq AS integer", True)
            , ("ALTER SEQUENCE some_seq START WITH 3", True)
            , ("ALTER SEQUENCE some_seq RESTART WITH 7", False)
            -- ^ TODO: Where can I find in pg_catalog the restart_with value? Currently it does not affect hashing, sadly.
            , ("ALTER SEQUENCE some_seq MINVALUE 2", True)
            , ("ALTER SEQUENCE some_seq MAXVALUE 99999", True)
            , ("ALTER SEQUENCE some_seq INCREMENT BY 2", True)
            , ("ALTER SEQUENCE some_seq CYCLE", True)
            , ("ALTER SEQUENCE some_seq CACHE 2", True)
            -- , ("ALTER SEQUENCE some_seq OWNED BY employee.employee_id", True)
            -- ^ TODO: Couldn't find owner table in the pg_catalog..

            -- TODO: Column Collations

            -- CHECK CONSTRAINTS
            , ("ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')", True)
            , ("ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> '')", False)
            , ("ALTER TABLE employee DROP CONSTRAINT employee_ck_name; ALTER TABLE employee ADD CONSTRAINT employee_ck_name CHECK (employee_name <> 'EMPTY')", True)

            -- FOREIGN KEYS
            , ("CREATE TABLE employee_car (employee_id INT NOT NULL, car_model TEXT NOT NULL)", True)
            , ("CREATE TABLE employee_computer (employee_id INT NOT NULL, computer_model TEXT NOT NULL, UNIQUE (employee_id))", True)
            , ("ALTER TABLE employee_car ADD CONSTRAINT employee_car_employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)", True)
            , ("ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY DEFERRED", True)
            , ("ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk DEFERRABLE INITIALLY IMMEDIATE", True)
            , ("ALTER TABLE employee_car ALTER CONSTRAINT employee_car_employee_fk NOT DEFERRABLE", True)

            , ("ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee(employee_id)", True)
            , ("ALTER TABLE employee_car DROP CONSTRAINT employee__employee_fk; ALTER TABLE employee_car ADD CONSTRAINT employee__employee_fk FOREIGN KEY (employee_id) REFERENCES employee_computer(employee_id)", True)
            -- ^ Same FK on the same table and column, referencing a different table, but with the same referenced column name as before.


            -- UNIQUE CONSTRAINTS AND INDICES
            , ("ALTER TABLE employee ADD CONSTRAINT unique_employee UNIQUE(employee_name)", True)
            , ("ALTER TABLE employee RENAME CONSTRAINT unique_employee TO employee_unique_name", True)            
            -- TODO: , ("CREATE UNIQUE INDEX unique_employee_idx ON employee (employee_name)", True)

            -- EXCLUSION CONSTRAINTS

            -- FUNCTIONS
            , ("CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$"
                <> "BEGIN  \n RETURN i + 1;  \n END;  \n $$ LANGUAGE plpgsql;", True)
            , ("CREATE OR REPLACE FUNCTION increment(i integer) RETURNS integer AS $$"
                <> "BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;", True)
            , ("CREATE OR REPLACE FUNCTION increment(i integer, x text) RETURNS integer AS $$"
                <> "BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;", True)
                -- ^ Same body as existing function, just a new parameter
            , ("CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS integer AS $$"
                <> "BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;", True)
                -- ^ Same body as existing function, just changing input argument order
            , ("DROP FUNCTION increment(text, integer); CREATE OR REPLACE FUNCTION increment(x text, i integer) RETURNS bigint AS $$"
                <> "BEGIN  \n RETURN i + 2;  \n END;  \n $$ LANGUAGE plpgsql;", True)
                -- ^ Same everything as existing function, just changing return type

            -- VIEWS


            -- TRIGGERS
            , ("ALTER TABLE employee ADD COLUMN name TEXT", True)
            , ("CREATE FUNCTION employee_name_rename_set_new() RETURNS TRIGGER AS $$\n"
               <> "BEGIN\n NEW.name = NEW.employee_name;\n RETURN NEW;\n END\n $$ LANGUAGE plpgsql;", True)
            , ("CREATE TRIGGER employee_old_app_update_column_name"
                <> "\n BEFORE UPDATE ON employee"
                <> "\n FOR EACH ROW"
                <> "\n WHEN (OLD.employee_name IS DISTINCT FROM NEW.employee_name)"
                <> "\n EXECUTE PROCEDURE employee_name_rename_set_new()", True)
            , ("DROP TRIGGER employee_old_app_update_column_name ON employee; CREATE TRIGGER employee_old_app_update_column_name"
                <> "\n BEFORE UPDATE ON employee"
                <> "\n FOR EACH ROW"
                <> "\n EXECUTE PROCEDURE employee_name_rename_set_new()", True)
                -- ^ No WHEN in the recreated trigger
            , ("DROP TRIGGER employee_old_app_update_column_name ON employee", True)
            , ("ALTER TABLE employee DROP COLUMN employee_name", True)
            , ("ALTER TABLE employee RENAME COLUMN name TO employee_name", True)


            -- ROW LEVEL SECURITY
            , ("ALTER TABLE employee ENABLE ROW LEVEL SECURITY", True)
            , ("CREATE POLICY some_policy ON employee USING (employee_name <> 'Some Name');", True)
            , ("DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee USING (employee_name <> 'Some Other Name');", True)
            , ("DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name');", True)
            , ("DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name') WITH CHECK (TRUE);", True)
            , ("DROP POLICY some_policy ON employee; CREATE POLICY some_policy ON employee FOR UPDATE USING (employee_name <> 'Some Other Name') WITH CHECK (TRUE);", False)
            , ("DROP POLICY some_policy ON employee;", True)


            -- ROLES
            , ("CREATE ROLE any_new_role", False)
            , ("DROP ROLE any_new_role", False)
            -- ^ Unmapped Role does not affect hashing

            , ("CREATE ROLE \"extra-codd-test-user\"", True)
            , ("DROP ROLE \"extra-codd-test-user\"", True)
            , ("ALTER ROLE \"codd-test-user\" SET search_path TO public, pg_catalog", True)
            , ("ALTER ROLE postgres SET search_path TO public, pg_catalog", True)
            , ("ALTER ROLE \"codd-test-user\" SET search_path TO DEFAULT", True)
            , ("ALTER ROLE postgres SET search_path TO DEFAULT", True)
            , ("ALTER ROLE postgres SET search_path TO DEFAULT", False)

            -- PERMISSIONS


            -- EXTENSIONS


            -- PARTITIONING

            
            -- CREATING UNMAPPED SCHEMAS


            -- CRUD
            , ("INSERT INTO employee (employee_name) VALUES ('Marcelo')", False)
            ]

spec :: Spec
spec = do
    let
        mkDbInfo baseDbInfo migs = baseDbInfo {
            sqlMigrations = Right migs
            }
    describe "DbDependentSpecs" $ do
        describe "Hashing tests" $ do
            aroundFreshDatabase $
                it "Because our DB hashing is so complex, let's make sure operations we know should change schema compatibility do" $ \emptyDbInfo -> do
                    let
                        connInfo = superUserInAppDatabaseConnInfo emptyDbInfo
                        getHashes sett = withConnection connInfo (readHashesFromDatabaseWithSettings sett)
                    hashBeforeEverything <- getHashes emptyDbInfo
                    void $
                        foldM (\(hashSoFar, appliedMigs :: [AddedSqlMigration]) (nextMig, nextMigModifiesSchema) -> do
                            let
                                newMigs = appliedMigs ++ [ nextMig ]
                                dbInfo = emptyDbInfo {
                                    sqlMigrations = Right newMigs
                                }
                            runStdoutLoggingT $ applyMigrations dbInfo False
                            dbHashesAfterMig <- getHashes dbInfo
                            let migText = nonDestructiveSql $ addedSqlMig nextMig
                            case nextMigModifiesSchema of
                                True -> do
                                    if hashSoFar == dbHashesAfterMig then do
                                        print hashSoFar
                                        print dbHashesAfterMig
                                        print migText
                                        error "Equal but should not be"
                                    else pure ()
                                    -- (migText, dbHashesAfterMig) `shouldNotBe` (migText, hashSoFar)
                                False -> (migText, dbHashesAfterMig) `shouldBe` (migText, hashSoFar)

                            return (dbHashesAfterMig, newMigs)
                        ) (hashBeforeEverything, []) migrationsAndHashChange
                    
                    -- Let's make sure we're actually applying the migrations by fetching some data..
                    withConnection connInfo (\conn -> DB.query conn "SELECT employee_id, employee_name FROM employee WHERE employee_name='Marcelo'" ())
                        `shouldReturn` [ (1 :: Int, "Marcelo" :: String) ]