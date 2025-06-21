module DbDependentSpecs.BackgroundJobsSpec where

import Codd (applyMigrationsNoCheck, migsConnString)
import Codd.Internal (withConnection)
import Codd.Internal.MultiQueryStatement (SqlStatementException)
import Codd.Logging (runCoddLogger)
import Codd.Parsing (AddedSqlMigration (..), SqlMigration (..))
import Codd.Query (query, unsafeQuery1)
import Codd.Representations (readRepresentationsFromDbWithSettings)
import Control.Monad (forM_, void, when)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.List (isInfixOf, sortOn)
import qualified Data.List as List
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (ConnectInfo)
import qualified Database.PostgreSQL.Simple as DB
import DbUtils (aroundDatabaseWithMigsAndPgCron, aroundFreshDatabase, aroundTestDbInfo, createTestUserMig, getIncreasingTimestamp, mkValidSql, testConnTimeout)
import GHC.Generics (Generic)
import LiftedExpectations (shouldThrow)
import Test.Hspec (Spec, shouldBe, shouldNotBe, shouldSatisfy)
import Test.Hspec.Core.Spec (describe, it)
import qualified Test.QuickCheck as QC
import UnliftIO (liftIO)

spec :: Spec
spec = do
  describe "DbDependentSpecs" $ do
    describe "Background jobs" $ do
      aroundTestDbInfo $ do
        it "Nice error message when setup has not been called" $ \emptyTestDbInfo -> do
          void @IO $
            runCoddLogger $ do
              bootstrapMig <- createTestUserMig
              applyMigrationsNoCheck
                emptyTestDbInfo
                (Just [bootstrapMig, someBgMigration])
                testConnTimeout
                (const $ pure ())
                `shouldThrow` (\(e :: SqlStatementException) -> "You must call the codd.setup_background_worker function before scheduling jobs" `isInfixOf` show e)

      aroundFreshDatabase $ do
        it "Nice error message when pg_cron is not installed during setup" $ \emptyTestDbInfo -> do
          void @IO $
            runCoddLogger $ do
              applyMigrationsNoCheck
                emptyTestDbInfo
                (Just [setupWithPgCron])
                testConnTimeout
                (const $ pure ())
                `shouldThrow` (\(e :: SqlStatementException) -> "Setting up codd background migrations with pg_cron requires the pg_cron extension to be installed" `isInfixOf` show e)

      aroundDatabaseWithMigsAndPgCron [] $ forM_ [True, False] $ \pgCronSetup ->
        it ("Setup works - " ++ show pgCronSetup) $ \testDbInfo -> do
          void @IO $
            runCoddLogger $ do
              applyMigrationsNoCheck
                testDbInfo
                (Just [if pgCronSetup then setupWithPgCron else setupWithExternalJobRunner])
                testConnTimeout
                (const $ pure ())

      aroundDatabaseWithMigsAndPgCron [] $ forM_ [True, False] $ \pgCronSetup ->
        it ("Fully test gradual migration and its synchronous finalisation - " ++ show pgCronSetup) $ \testDbInfo -> do
          void @IO $
            runCoddLogger $ do
              applyMigrationsNoCheck
                testDbInfo
                (Just [if pgCronSetup then setupWithPgCron else setupWithExternalJobRunner, createEmployeesTable, scheduleExperienceMigration])
                testConnTimeout
                (const $ pure ())
              liftIO $ when pgCronSetup $ withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> do
                -- Wait a few seconds and check that the job runs once a second
                startedCoddJob <- unsafeQuery1 conn "SELECT pg_sleep(5); SELECT * FROM codd.jobs" ()
                DB.Only (scheduledCronJob :: String) <- unsafeQuery1 conn "SELECT jobname FROM cron.job" ()
                -- The periodic job updates 1 employee per run, and it has 80% chance of failing (look at the random() expression).
                -- Since there are 5 employees to migrate (some are inserted after the job and thus the trigger are created),
                -- the chance that 5 seconds (at most 5 runs) have all succeeded is 0.2^5=0.032%, which I consider to be 0
                -- for the purposes of this test.
                numJobsSucceeded startedCoddJob + numJobsError startedCoddJob `shouldSatisfy` (>= 4) -- >=5 would be pushing our luck
                status startedCoddJob `shouldBe` "started"
                scheduledCronJob `shouldBe` "change-experience"
                lastRunAt startedCoddJob `shouldNotBe` Nothing
                completedOrAbortedAt startedCoddJob `shouldBe` Nothing
                finalizedAt startedCoddJob `shouldBe` Nothing
              (allEmployees :: [(String, String)], finalizedCoddJob :: JobInfo, scheduledCronJobs :: [String]) <-
                applyMigrationsNoCheck
                  testDbInfo
                  (Just [finalizeExperienceMigration])
                  testConnTimeout
                  (\conn -> (,,) <$> query conn "SELECT name, experience::text FROM employee ORDER BY name" () <*> unsafeQuery1 conn "SELECT * FROM codd.jobs" () <*> if pgCronSetup then query conn "SELECT jobname FROM cron.job" () else pure [])
              liftIO $ do
                allEmployees `shouldBe` sortOn fst [("John Doe", "senior"), ("Bob", "senior"), ("Alice", "senior"), ("Marcelo", "junior"), ("Goku", "senior"), ("Dracula", "senior"), ("Frankenstein", "senior"), ("Jimi", "junior")]
                -- The probability of no failures is 0.2^5=0.032% (see comment ~20 lines above), which
                -- is as good as 0 for the purposes of this test.
                numJobsError finalizedCoddJob `shouldSatisfy` (> 0)
                lastErrorAt finalizedCoddJob `shouldNotBe` Nothing
                lastError finalizedCoddJob `shouldSatisfy` ("22P02" `List.isInfixOf`) . show -- This is invalid input value for the enum
                numJobsSucceeded finalizedCoddJob `shouldBe` 6 -- 5 + 1 last finalizedCoddJob that updates no rows
                jobname finalizedCoddJob `shouldBe` "change-experience"
                status finalizedCoddJob `shouldBe` "finalized"
                lastRunAt finalizedCoddJob `shouldNotBe` Nothing
                completedOrAbortedAt finalizedCoddJob `shouldNotBe` Nothing
                finalizedAt finalizedCoddJob `shouldNotBe` Nothing
                lastErrorAt finalizedCoddJob `shouldNotBe` Nothing
                scheduledCronJobs `shouldBe` []

data JobInfo = JobInfo
  { jobname :: String,
    createdAt :: UTCTime,
    status :: String,
    description :: String,
    numJobsSucceeded :: Int,
    numJobsError :: Int,
    lastRunAt :: Maybe UTCTime,
    completedOrAbortedAt :: Maybe UTCTime,
    finalizedAt :: Maybe UTCTime,
    lastErrorAt :: Maybe UTCTime,
    lastError :: Maybe String
  }
  deriving stock (Generic, Show)
  deriving anyclass (DB.FromRow)

setupWithPgCron :: (MonadThrow m) => AddedSqlMigration m
setupWithPgCron =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0001-setup-with-pg-cron.sql",
        migrationSql = mkValidSql "SELECT codd.setup_background_worker('pg_cron')",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 1)

setupWithExternalJobRunner :: (MonadThrow m) => AddedSqlMigration m
setupWithExternalJobRunner =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0001-setup-with-external-job-runner.sql",
        migrationSql = mkValidSql "SELECT codd.setup_background_worker('external')",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 1)

-- | A migration that calls a function in the "codd" schema only for the purposes of
-- testing that it was called, i.e. it is not correcty written otherwise.
someBgMigration :: (MonadThrow m) => AddedSqlMigration m
someBgMigration =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0002-some-bg-migration.sql",
        migrationSql =
          mkValidSql
            "SELECT codd.populate_column_gradually('change-experience', '1 seconds', 'whatever', 'employee', 'somecolumn', 'whatever');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 2)

createEmployeesTable :: (MonadThrow m) => AddedSqlMigration m
createEmployeesTable =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0002-create-employees-table.sql",
        migrationSql =
          mkValidSql
            "CREATE TYPE experience AS ENUM ('intern', 'junior', 'senior', 'master');\n\
            \CREATE TABLE employee (employee_id SERIAL PRIMARY KEY, name TEXT NOT NULL, experience experience NOT NULL);\n\
            \INSERT INTO employee (name, experience) VALUES ('John Doe', 'master'), ('Bob', 'master'), ('Alice', 'senior'), ('Marcelo', 'junior'), ('Jimi', 'junior');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = False,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 2)

-- | Begins and synchronously finishes a job to gradually transform 'master' employees into 'senior'.
-- Inserts some employees before finishing the job to test any triggers' code paths.
scheduleExperienceMigration :: (MonadThrow m) => AddedSqlMigration m
scheduleExperienceMigration =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0003-experience-migration.sql",
        migrationSql =
          mkValidSql
            "CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');\n\
            \ALTER TABLE employee ADD COLUMN experience2 experience2;\n\
            \SELECT codd.populate_column_gradually('change-experience', '1 seconds',\n\
            \$$\n\
            \UPDATE employee SET experience2=CASE WHEN ((RANDOM() * 100)::int % 5) <= 3 THEN (experience::text || '-invalid')::experience2 WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END\n\
            \WHERE employee_id=(SELECT employee_id FROM employee WHERE (experience IS NULL) <> (experience2 IS NULL) LIMIT 1);\n\
            \$$\n\
            \, 'employee', 'experience2', $$CASE WHEN NEW.experience='master' THEN 'senior' ELSE NEW.experience::text::experience2 END$$\n\
            \);\n\
            \INSERT INTO employee (name, experience) VALUES ('Dracula', 'master'), ('Frankenstein', 'senior');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 3)

finalizeExperienceMigration :: (MonadThrow m) => AddedSqlMigration m
finalizeExperienceMigration =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0004-finalize-experience-migration.sql",
        migrationSql =
          mkValidSql
            "SELECT codd.synchronously_finalize_background_job('change-experience', '100 seconds');\n\
            \ALTER TABLE employee DROP COLUMN experience;\n\
            \ALTER TABLE employee RENAME COLUMN experience2 TO experience;\n\
            \DROP TYPE experience;\n\
            \ALTER TYPE experience2 RENAME TO experience;\n\
            \INSERT INTO employee (name, experience) VALUES ('Goku', 'senior');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 4)
