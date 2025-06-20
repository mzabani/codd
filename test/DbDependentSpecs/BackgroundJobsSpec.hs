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

spec :: Spec
spec = do
  describe "DbDependentSpecs" $ do
    describe "Background jobs" $ do
      aroundTestDbInfo $ do
        it "Nice error message when pg_cron is not installed" $ \emptyTestDbInfo -> do
          void @IO $
            runCoddLogger $ do
              bootstrapMig <- createTestUserMig
              applyMigrationsNoCheck
                emptyTestDbInfo
                (Just [bootstrapMig, beginSomeBackgroundJob])
                testConnTimeout
                (const $ pure ())
                `shouldThrow` (\(e :: SqlStatementException) -> "background migrations require the pg_cron extension to work" `isInfixOf` show e)

      aroundDatabaseWithMigsAndPgCron [] $ do
        it "Nice error message when pg_cron is not installed" $ \emptyTestDbInfo -> do
          void @IO $
            runCoddLogger $ do
              applyMigrationsNoCheck
                emptyTestDbInfo
                (Just [beginSomeBackgroundJob])
                testConnTimeout
                (const $ pure ())
                `shouldThrow` (\(e :: SqlStatementException) -> "background migrations require the pg_cron extension to work" `isInfixOf` show e)

beginSomeBackgroundJob :: (MonadThrow m) => AddedSqlMigration m
beginSomeBackgroundJob =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0001-begin-some-bg-job.sql",
        migrationSql = mkValidSql "SELECT codd.populate_column_gradually('jobname', 'cron schedule', 'plpgsql', 'tablename', 'colname', 'triggerexpr');",
        migrationInTxn = True,
        migrationRequiresCoddSchema = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 1)
