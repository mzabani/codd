module DbDependentSpecs.BackgroundJobsSpec where

import Codd (applyMigrationsNoCheck, migsConnString)
import Codd.Environment (CoddSettings (..))
import Codd.Internal (withConnection)
import Codd.Internal.MultiQueryStatement (SqlStatementException)
import Codd.Logging (runCoddLogger)
import Codd.Parsing (AddedSqlMigration (..), SqlMigration (..))
import Codd.Query (InTxnT, query, unsafeQuery1, withTransaction)
import Codd.Representations (readRepresentationsFromDbWithSettings)
import Codd.Types (ConnectionString (..), TxnIsolationLvl (..))
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.List (isInfixOf, sortOn)
import qualified Data.List as List
import Data.String (fromString)
import Data.Time (UTCTime)
import qualified Database.PostgreSQL.Simple as DB
import DbUtils (aroundDatabaseWithMigs, aroundDatabaseWithMigsAndPgCron, aroundFreshDatabase, aroundTestDbInfo, createTestUserMig, getIncreasingTimestamp, mkValidSql, testConnTimeout)
import GHC.Generics (Generic)
import LiftedExpectations (shouldThrow)
import Test.Hspec (Spec, shouldBe, shouldContain, shouldNotBe, shouldReturn, shouldSatisfy)
import Test.Hspec.Core.Spec (describe, it)
import qualified Test.QuickCheck as QC
import UnliftIO (SomeException, liftIO)
import UnliftIO.Concurrent (threadDelay)

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
          void @IO $ do
            runCoddLogger $ do
              applyMigrationsNoCheck
                testDbInfo
                (Just [if pgCronSetup then setupWithPgCron else setupWithExternalJobRunner])
                testConnTimeout
                (const $ pure ())
            -- Switching to a different background worker is possible as long as there aren't active jobs
            withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> do
              _ :: DB.Only () <- unsafeQuery1 conn "SELECT codd.setup_background_worker('external')" ()
              _ :: DB.Only () <- unsafeQuery1 conn "SELECT codd.setup_background_worker('pg_cron')" ()
              _ :: DB.Only () <- unsafeQuery1 conn "SELECT codd.setup_background_worker('external')" ()
              pure ()

      aroundDatabaseWithMigsAndPgCron [] $ forM_ [(i, pgCronSetup) | i <- [DbDefault, ReadUncommitted, ReadCommitted, RepeatableRead, Serializable], pgCronSetup <- [False, True]] $ \(txnIsolationLvl, pgCronSetup) ->
        it ("Isolation level used correctly - " ++ show (txnIsolationLvl, pgCronSetup)) $ \testDbInfo -> do
          void @IO $
            runCoddLogger $ do
              applyMigrationsNoCheck
                testDbInfo {txnIsolationLvl}
                (Just [if pgCronSetup then setupWithPgCron else setupWithExternalJobRunner, createEmployeesTable, scheduleExperienceMigration])
                testConnTimeout
                (const $ pure ())
              withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> do
                DB.Only storedIsolLvl <- unsafeQuery1 conn "SELECT txn_isolation_level FROM codd._background_jobs" ()
                let expectedStoredIsolLvl = case txnIsolationLvl of
                      DbDefault -> "db-default" :: String
                      ReadUncommitted -> "read-uncommitted"
                      ReadCommitted -> "read-committed"
                      RepeatableRead -> "repeatable-read"
                      Serializable -> "serializable"
                liftIO $ storedIsolLvl `shouldBe` expectedStoredIsolLvl
                when pgCronSetup $ do
                  DB.Only (scheduledCronCommand :: String) <- unsafeQuery1 conn "SELECT command FROM cron.job" ()
                  let expectedBegin = case txnIsolationLvl of
                        DbDefault -> "BEGIN READ WRITE;" :: String
                        Serializable -> "BEGIN READ WRITE,ISOLATION LEVEL SERIALIZABLE;"
                        RepeatableRead -> "BEGIN READ WRITE,ISOLATION LEVEL REPEATABLE READ;"
                        ReadCommitted -> "BEGIN READ WRITE,ISOLATION LEVEL READ COMMITTED;"
                        ReadUncommitted -> "BEGIN READ WRITE,ISOLATION LEVEL READ UNCOMMITTED;"
                  -- The pg_cron command must have the right isolation level, too
                  liftIO $ scheduledCronCommand `shouldContain` expectedBegin

      aroundFreshDatabase $ it "Values supplied explicitly by users for migrated columns are ignored" $ \testDbInfo -> do
        void @IO $
          runCoddLogger $ do
            applyMigrationsNoCheck
              testDbInfo
              (Just [setupWithExternalJobRunner, createEmployeesTable, scheduleExperienceMigration])
              testConnTimeout
              (const $ pure ())
            withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> liftIO $ do
              -- Hendrix should be inserted as a senior, not a junior, despite the explicit value here
              -- The UPDATE too should have no effect
              DB.Only (overwrittenExperience2 :: String) <- unsafeQuery1 conn "INSERT INTO \"empl  oyee\" (name, experience, \"expE Rience2\") VALUES ('Hendrix', 'master', 'junior') RETURNING \"expE Rience2\"::text" ()
              DB.Only (overwrittenExperience2' :: String) <- unsafeQuery1 conn "UPDATE \"empl  oyee\" SET \"expE Rience2\"='junior' WHERE name='Hendrix' RETURNING \"expE Rience2\"::text" ()
              overwrittenExperience2 `shouldBe` "senior"
              overwrittenExperience2' `shouldBe` "senior"

      aroundDatabaseWithMigsAndPgCron [] $ forM_ [False, True] $ \pgCronSetup ->
        it ("Apply and wait until background migration goes into run-complete-awaiting-finalization - " ++ show pgCronSetup) $ \testDbInfo -> do
          void @IO $ do
            runCoddLogger $
              applyMigrationsNoCheck
                testDbInfo
                (Just [if pgCronSetup then setupWithPgCron else setupWithExternalJobRunner, createEmployeesTable, scheduleQuickerMigration])
                testConnTimeout
                (const $ pure ())
            withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> do
              -- There are 5 employees to migrate and the job updates 2 per run, so it'll take 3 runs to update them all,
              -- but only the fourth run will return a 0 UPDATE count
              if pgCronSetup then waitUntilJobRuns conn "change- expèRiénce$" 1 else runJobN conn "change- expèRiénce$" 1
              coddJobAfterOneRun <- unsafeQuery1 conn "SELECT * FROM codd.jobs" ()
              if pgCronSetup then waitUntilJobRuns conn "change- expèRiénce$" 3 else runJobN conn "change- expèRiénce$" 2
              coddJobStillNotDone <- unsafeQuery1 conn "SELECT * FROM codd.jobs" ()
              if pgCronSetup then waitUntilJobRuns conn "change- expèRiénce$" 4 else runJobN conn "change- expèRiénce$" 1
              finalizedCoddJob <- unsafeQuery1 conn "SELECT * FROM codd.jobs" ()
              DB.Only (someCronJobRunning :: Bool) <- unsafeQuery1 conn "SELECT COUNT(*)>0 FROM cron.job" ()
              allEmployees :: [(String, String)] <- query conn "SELECT name, \"expE Rience2\"::text FROM \"empl  oyee\" ORDER BY name" ()
              forM_ [(coddJobAfterOneRun, 1), (coddJobStillNotDone, 3)] $ \(coddJob, numRuns) -> do
                status coddJob `shouldBe` "started"
                numJobsSucceeded coddJob `shouldBe` numRuns
                lastRunAt coddJob `shouldNotBe` Nothing
                completedOrAbortedAt coddJob `shouldBe` Nothing
                finalizedAt coddJob `shouldBe` Nothing
                description coddJob `shouldContain` "Gradually populating values in the"
              lastRunAt coddJobStillNotDone `shouldSatisfy` (> lastRunAt coddJobAfterOneRun)
              -- Unlike in other tests, this doesn't have Goku because Goku is inserted in the synchronous finalisation
              -- migration
              allEmployees `shouldBe` sortOn fst [("John Doe", "senior"), ("Bob", "senior"), ("Alice", "senior"), ("Marcelo", "junior"), ("Dracula", "senior"), ("Frankenstein", "senior"), ("Jimi", "junior")]
              numJobsError finalizedCoddJob `shouldBe` 0
              lastErrorAt finalizedCoddJob `shouldBe` Nothing
              lastError finalizedCoddJob `shouldBe` Nothing
              numJobsSucceeded finalizedCoddJob `shouldBe` 4
              jobname finalizedCoddJob `shouldBe` "change- expèRiénce$"
              status finalizedCoddJob `shouldBe` "run-complete-awaiting-finalization"
              lastRunAt finalizedCoddJob `shouldNotBe` Nothing
              lastRunAt finalizedCoddJob `shouldSatisfy` (> lastRunAt coddJobStillNotDone)
              completedOrAbortedAt finalizedCoddJob `shouldNotBe` Nothing
              finalizedAt finalizedCoddJob `shouldBe` Nothing
              lastErrorAt finalizedCoddJob `shouldBe` Nothing
              someCronJobRunning `shouldBe` False
              description finalizedCoddJob `shouldContain` "You can now call codd.synchronously_finalize_background_job to remove the triggers and accessory functions created to keep the new column up-to-date"

      aroundDatabaseWithMigsAndPgCron [] $ forM_ [False, True] $ \pgCronSetup ->
        it ("Aborting a job - " ++ show pgCronSetup) $ \testDbInfo -> do
          void @IO $
            runCoddLogger $ do
              applyMigrationsNoCheck
                testDbInfo
                (Just [if pgCronSetup then setupWithPgCron else setupWithExternalJobRunner, createEmployeesTable, scheduleExperienceMigration])
                testConnTimeout
                (const $ pure ())
              withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> liftIO $ do
                abortedCoddJob <-
                  if pgCronSetup
                    then do
                      abortedCoddJob <- unsafeQuery1 conn "SELECT pg_sleep(5); SELECT codd.abort_background_job('change- expèRiénce$'); SELECT * FROM codd.jobs" ()
                      unsafeQuery1 conn "SELECT COUNT(*) FROM cron.job" () `shouldReturn` DB.Only (0 :: Int)
                      numJobsSucceeded abortedCoddJob + numJobsError abortedCoddJob `shouldSatisfy` (>= 4) -- >=5 would be pushing our luck
                      lastRunAt abortedCoddJob `shouldNotBe` Nothing
                      pure abortedCoddJob
                    else do
                      abortedCoddJob <- unsafeQuery1 conn "SELECT codd.abort_background_job('change- expèRiénce$'); SELECT * FROM codd.jobs" ()
                      -- No external runner
                      numJobsSucceeded abortedCoddJob + numJobsError abortedCoddJob `shouldBe` 0
                      lastRunAt abortedCoddJob `shouldBe` Nothing
                      pure abortedCoddJob
                status abortedCoddJob `shouldBe` "aborted"
                jobname abortedCoddJob `shouldBe` "change- expèRiénce$"
                completedOrAbortedAt abortedCoddJob `shouldNotBe` Nothing
                finalizedAt abortedCoddJob `shouldBe` Nothing
                description abortedCoddJob `shouldContain` "Given up populating values in the"
                -- Test that we can DELETE from _background_jobs, like we promise, and that synchronous finalization
                -- errors out
                DB.execute conn "SELECT codd.synchronously_finalize_background_job('change- expèRiénce$', '0 seconds')" () `shouldThrow` (\(ex :: SomeException) -> "It is not possible to finalize the aborted job " `List.isInfixOf` show ex)
                DB.execute conn "DELETE FROM codd._background_jobs WHERE jobname='change- expèRiénce$'" () `shouldReturn` 1

      aroundDatabaseWithMigsAndPgCron [] $ forM_ [(cron, isol) | cron <- [True, False], isol <- [DbDefault, ReadUncommitted, ReadCommitted, RepeatableRead, Serializable]] $ \(pgCronSetup, txnIsolationLvl) ->
        it ("Fully test gradual migration and its synchronous finalisation - " ++ show (pgCronSetup, txnIsolationLvl)) $ \testDbInfo -> do
          void @IO $
            runCoddLogger $ do
              applyMigrationsNoCheck
                testDbInfo {txnIsolationLvl = txnIsolationLvl}
                (Just [if pgCronSetup then setupWithPgCron else setupWithExternalJobRunner, createEmployeesTable, scheduleExperienceMigration])
                testConnTimeout
                (const $ pure ())
              liftIO $ when pgCronSetup $ withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> do
                -- Wait a few seconds and check that the job runs once a second
                threadDelay 5_000_000
                withTransaction @(InTxnT IO) txnIsolationLvl conn $ do
                  startedCoddJob <- unsafeQuery1 conn "SELECT * FROM codd.jobs" ()
                  DB.Only (scheduledCronJob :: String) <- unsafeQuery1 conn "SELECT jobname FROM cron.job" ()
                  -- The periodic job updates 1 employee per run, and it has 80% chance of failing (look at the random() expression).
                  -- Since there are 5 employees to migrate (some are inserted after the job and thus the trigger are created),
                  -- the chance that 5 seconds (at most 5 runs) have all succeeded is 0.2^5=0.032%, which I consider to be 0
                  -- for the purposes of this test.
                  liftIO $ do
                    numJobsSucceeded startedCoddJob + numJobsError startedCoddJob `shouldSatisfy` (>= 4) -- >=5 would be pushing our luck
                    status startedCoddJob `shouldBe` "started"
                    scheduledCronJob `shouldBe` "change- expèRiénce$"
                    lastRunAt startedCoddJob `shouldNotBe` Nothing
                    completedOrAbortedAt startedCoddJob `shouldBe` Nothing
                    finalizedAt startedCoddJob `shouldBe` Nothing
              (allEmployees :: [(String, String)], finalizedCoddJob :: JobInfo, scheduledCronJobs :: [String]) <-
                applyMigrationsNoCheck
                  testDbInfo {txnIsolationLvl = txnIsolationLvl}
                  (Just [finalizeExperienceMigration])
                  testConnTimeout
                  (\conn -> (,,) <$> query conn "SELECT name, experience::text FROM employee ORDER BY name" () <*> unsafeQuery1 conn "SELECT * FROM codd.jobs" () <*> if pgCronSetup then query conn "SELECT jobname FROM cron.job" () else pure [])
              liftIO $ withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> do
                allEmployees `shouldBe` sortOn fst [("John Doe", "senior"), ("Bob", "senior"), ("Alice", "senior"), ("Marcelo", "junior"), ("Goku", "senior"), ("Dracula", "senior"), ("Frankenstein", "senior"), ("Jimi", "junior")]
                -- The probability of no failures is 0.2^5=0.032% (see comment ~20 lines above), which
                -- is as good as 0 for the purposes of this test.
                numJobsError finalizedCoddJob `shouldSatisfy` (> 0)
                lastErrorAt finalizedCoddJob `shouldNotBe` Nothing
                lastError finalizedCoddJob `shouldSatisfy` ("22P02" `List.isInfixOf`) . show -- This is invalid input value for the enum
                numJobsSucceeded finalizedCoddJob `shouldBe` 6 -- 5 + 1 last job run that updates no rows
                jobname finalizedCoddJob `shouldBe` "change- expèRiénce$"
                status finalizedCoddJob `shouldBe` "finalized"
                lastRunAt finalizedCoddJob `shouldNotBe` Nothing
                completedOrAbortedAt finalizedCoddJob `shouldNotBe` Nothing
                finalizedAt finalizedCoddJob `shouldNotBe` Nothing
                lastErrorAt finalizedCoddJob `shouldNotBe` Nothing
                scheduledCronJobs `shouldBe` []
                description finalizedCoddJob `shouldContain` "Job completed successfully. You may delete this row from codd._background_jobs at any time with no side effects"
                -- Finalizing a job that's already finalized does nothing and does not throw
                _ :: DB.Only () <- unsafeQuery1 conn "SELECT codd.synchronously_finalize_background_job('change- expèRiénce$', '0 seconds')" ()
                pure ()

      forM_ [DbDefault, ReadUncommitted, ReadCommitted, RepeatableRead, Serializable] $ \txnIsolationLvl -> aroundDatabaseWithMigsAndPgCron [] $
        it ("Synchronous finalisation concurrent to job run does not deadlock - " ++ show txnIsolationLvl) $ \testDbInfo -> void @IO $ do
          -- If you comment out the locking statements in codd's functions, this test should deadlock
          runCoddLogger $ do
            applyMigrationsNoCheck
              testDbInfo {txnIsolationLvl = txnIsolationLvl}
              (Just [setupWithPgCron, createEmployeesTable, scheduleExperienceMigrationSlowLocking])
              testConnTimeout
              (const $ pure ())
            liftIO $ withConnection (migsConnString testDbInfo) testConnTimeout $ \conn -> do
              -- Wait until right after the job runs for the first time but before it runs a second
              -- time to acquire locks that will conflict with the next job run
              waitUntilJobRuns conn "change- expèRiénce$" 1
              withTransaction @(InTxnT IO) txnIsolationLvl conn $ do
                startedCoddJob <- unsafeQuery1 conn "SELECT * FROM codd.jobs" ()
                DB.Only (scheduledCronJob :: String) <- unsafeQuery1 conn "SELECT jobname FROM cron.job" ()
                liftIO $ do
                  numJobsSucceeded startedCoddJob `shouldBe` 1
                  status startedCoddJob `shouldBe` "started"
                  scheduledCronJob `shouldBe` "change- expèRiénce$"
                  lastRunAt startedCoddJob `shouldNotBe` Nothing
                  completedOrAbortedAt startedCoddJob `shouldBe` Nothing
                  finalizedAt startedCoddJob `shouldBe` Nothing

            -- Here it gets interesting: finalizing this migration synchronously ought to conflict with the job soon-to-run
            -- itself, since the job's query runs an UPDATE+pg_sleep+UPDATE, keeping locks on updated rows long enough for
            -- it to conflict with itself.
            -- The call to applyMigrationsNoCheck below deadlocks unless codd's schema has carefully designed locks
            -- in its functions.
            (allEmployees :: [(String, String)], finalizedCoddJob :: JobInfo, scheduledCronJobs :: [String]) <-
              applyMigrationsNoCheck
                testDbInfo {txnIsolationLvl = txnIsolationLvl}
                (Just [finalizeExperienceMigration])
                testConnTimeout
                (\conn -> (,,) <$> query conn "SELECT name, experience::text FROM employee ORDER BY name" () <*> unsafeQuery1 conn "SELECT * FROM codd.jobs" () <*> query conn "SELECT jobname FROM cron.job" ())
            liftIO $ do
              allEmployees `shouldBe` sortOn fst [("John Doe", "senior"), ("Bob", "senior"), ("Alice", "senior"), ("Marcelo", "junior"), ("Goku", "senior"), ("Dracula", "senior"), ("Frankenstein", "senior"), ("Jimi", "junior")]
              numJobsError finalizedCoddJob `shouldBe` 0
              lastErrorAt finalizedCoddJob `shouldBe` Nothing
              numJobsSucceeded finalizedCoddJob `shouldBe` 3 -- 2 rows updated per job, third job returns 0 count
              jobname finalizedCoddJob `shouldBe` "change- expèRiénce$"
              status finalizedCoddJob `shouldBe` "finalized"
              lastRunAt finalizedCoddJob `shouldNotBe` Nothing
              completedOrAbortedAt finalizedCoddJob `shouldNotBe` Nothing
              finalizedAt finalizedCoddJob `shouldNotBe` Nothing
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

waitUntilJobRuns ::
  DB.Connection ->
  String ->
  -- | Minimum number of runs to wait for
  Int ->
  IO ()
waitUntilJobRuns conn jobname minRuns = do
  [DB.Only jobRan] <- DB.query conn "SELECT COUNT(*)>0 FROM codd.jobs WHERE jobname=? AND num_jobs_succeeded>=?" (jobname, minRuns)
  unless jobRan $ threadDelay 50_000 >> waitUntilJobRuns conn jobname minRuns

-- | Emulates an external job runner and runs the stored function in codd's
-- background jobs tables the supplied number of times.
runJobN ::
  DB.Connection ->
  String ->
  Int ->
  IO ()
runJobN conn jobname numRuns = do
  [DB.Only (funcName :: String)] <- DB.query conn "SELECT job_function FROM codd._background_jobs WHERE jobname=?" (DB.Only jobname)
  forM_ [1 .. numRuns] $ const $ do
    _ :: [DB.Only ()] <- DB.query conn ("SELECT " <> fromString funcName <> "()") ()
    pure ()

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
            "-- RESET ALL checks that we're not relying on session-defined (instead of connection-defined) for things like the isolation level\n\
            \RESET ALL;\n\
            \SELECT codd.populate_column_gradually('change- expèRiénce$', '1 seconds', 'whatever', 'employee', 'somecolumn', 'whatever');",
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

-- | Begins a job to gradually transform 'master' employees into 'senior'.
-- Inserts some employees before finishing the job to test any triggers' code paths.
-- The idea is to migrate columns of an "employee" table and a temporary "experience2" column for
-- the new values, before it replaces the "experience" column. However, there's a lot of templating code,
-- so we do test bizarre object names for the table, the column and the job name.
scheduleExperienceMigration :: (MonadThrow m) => AddedSqlMigration m
scheduleExperienceMigration =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0003-experience-migration.sql",
        migrationSql =
          mkValidSql
            "CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');\n\
            \ALTER TABLE employee RENAME TO \"empl  oyee\";\n\
            \ALTER TABLE \"empl  oyee\" ADD COLUMN \"expE Rience2\" experience2;\n\
            \-- RESET ALL checks that we're not relying on session-defined (instead of connection-defined) for things like the isolation level\n\
            \RESET ALL;\n\
            \SELECT codd.populate_column_gradually('change- expèRiénce$', '1 seconds',\n\
            \$$\n\
            \UPDATE \"empl  oyee\" SET \"expE Rience2\"=CASE WHEN ((RANDOM() * 100)::int % 5) <= 3 THEN (experience::text || '-invalid')::experience2 WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END\n\
            \WHERE employee_id=(SELECT employee_id FROM \"empl  oyee\" WHERE (experience IS NULL) <> (\"expE Rience2\" IS NULL) LIMIT 1);\n\
            \$$\n\
            \, 'empl  oyee', 'expE Rience2', $$CASE WHEN NEW.experience='master' THEN 'senior' ELSE NEW.experience::text::experience2 END$$\n\
            \);\n\
            \INSERT INTO \"empl  oyee\" (name, experience) VALUES ('Dracula', 'master'), ('Frankenstein', 'senior');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 3)

scheduleExperienceMigrationSlowLocking :: (MonadThrow m) => AddedSqlMigration m
scheduleExperienceMigrationSlowLocking =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0003-experience-migration-slow-locking.sql",
        migrationSql =
          mkValidSql
            "CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');\n\
            \ALTER TABLE employee RENAME TO \"empl  oyee\";\n\
            \ALTER TABLE \"empl  oyee\" ADD COLUMN \"expE Rience2\" experience2;\n\
            \-- RESET ALL checks that we're not relying on session-defined (instead of connection-defined) for things like the isolation level\n\
            \RESET ALL;\n\
            \SELECT codd.populate_column_gradually('change- expèRiénce$', '2 seconds',\n\
            \$$\n\
            \UPDATE \"empl  oyee\" SET \"expE Rience2\"=CASE WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END\n\
            \WHERE employee_id=(SELECT employee_id FROM \"empl  oyee\" WHERE (experience IS NULL) <> (\"expE Rience2\" IS NULL) LIMIT 1);\n\
            \-- Sleep 1 second to give the test time to run before the job runs a second time (i.e. between 3s and 4s) \n\
            \PERFORM pg_sleep(1);\n\
            \UPDATE \"empl  oyee\" SET \"expE Rience2\"=CASE WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END\n\
            \WHERE employee_id=(SELECT employee_id FROM \"empl  oyee\" WHERE (experience IS NULL) <> (\"expE Rience2\" IS NULL) LIMIT 1);\n\
            \$$\n\
            \, 'empl  oyee', 'expE Rience2', $$CASE WHEN NEW.experience='master' THEN 'senior' ELSE NEW.experience::text::experience2 END$$\n\
            \);\n\
            \INSERT INTO \"empl  oyee\" (name, experience) VALUES ('Dracula', 'master'), ('Frankenstein', 'senior');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 3)

scheduleQuickerMigration :: (MonadThrow m) => AddedSqlMigration m
scheduleQuickerMigration =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0003-experience-quicker-migration.sql",
        migrationSql =
          mkValidSql
            "CREATE TYPE experience2 AS ENUM ('intern', 'junior', 'senior');\n\
            \ALTER TABLE employee RENAME TO \"empl  oyee\";\n\
            \ALTER TABLE \"empl  oyee\" ADD COLUMN \"expE Rience2\" experience2;\n\
            \-- RESET ALL checks that we're not relying on session-defined (instead of connection-defined) for things like the isolation level\n\
            \RESET ALL;\n\
            \SELECT codd.populate_column_gradually('change- expèRiénce$', '1 seconds',\n\
            \$$\n\
            \UPDATE \"empl  oyee\" SET \"expE Rience2\"=CASE WHEN experience='master' THEN 'senior' ELSE experience::text::experience2 END\n\
            \WHERE employee_id IN (SELECT employee_id FROM \"empl  oyee\" WHERE (experience IS NULL) <> (\"expE Rience2\" IS NULL) LIMIT 2);$$, 'empl  oyee', 'expE Rience2', $$CASE WHEN NEW.experience='master' THEN 'senior' ELSE NEW.experience::text::experience2 END$$\n\
            \);\n\
            \INSERT INTO \"empl  oyee\" (name, experience) VALUES ('Dracula', 'master'), ('Frankenstein', 'senior');",
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
            "-- RESET ALL checks that we're not relying on session-defined (instead of connection-defined) for things like the isolation level\n\
            \RESET ALL;\n\
            \SELECT codd.synchronously_finalize_background_job('change- expèRiénce$', '100 seconds');\n\
            \ALTER TABLE \"empl  oyee\" RENAME TO employee;\n\
            \ALTER TABLE employee DROP COLUMN experience;\n\
            \ALTER TABLE employee RENAME COLUMN \"expE Rience2\" TO experience;\n\
            \DROP TYPE experience;\n\
            \ALTER TYPE experience2 RENAME TO experience;\n\
            \INSERT INTO employee (name, experience) VALUES ('Goku', 'senior');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 4)
