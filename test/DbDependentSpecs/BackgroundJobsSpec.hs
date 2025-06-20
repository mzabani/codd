module DbDependentSpecs.BackgroundJobsSpec where

import Codd (applyMigrationsNoCheck)
import Codd.Internal.MultiQueryStatement (SqlStatementException)
import Codd.Logging (runCoddLogger)
import Codd.Parsing (AddedSqlMigration (..), SqlMigration (..))
import Control.Monad (void)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (ConnectInfo)
import DbUtils (aroundDatabaseWithMigsAndPgCron, aroundFreshDatabase, aroundTestDbInfo, createTestUserMig, getIncreasingTimestamp, mkValidSql, testConnTimeout)
import LiftedExpectations (shouldThrow)
import Test.Hspec (Spec)
import Test.Hspec.Core.Spec (describe, it)
import qualified Test.QuickCheck as QC

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
                (Just [bootstrapMig, beginSomeBackgroundJob])
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

      aroundDatabaseWithMigsAndPgCron [] $ do
        it "Setup works" $ \testDbInfo -> QC.property $ \pgCronSetup -> do
          void @IO $
            runCoddLogger $ do
              applyMigrationsNoCheck
                testDbInfo
                (Just [if pgCronSetup then setupWithPgCron else setupWithExternalJobRunner])
                testConnTimeout
                (const $ pure ())

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

beginSomeBackgroundJob :: (MonadThrow m) => AddedSqlMigration m
beginSomeBackgroundJob =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0002-begin-some-bg-job.sql",
        migrationSql = mkValidSql "SELECT codd.populate_column_gradually('jobname', 'cron schedule', 'plpgsql', 'tablename', 'colname', 'triggerexpr');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 2)
