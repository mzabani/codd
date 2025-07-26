module DbDependentSpecs.CoddAddSpec where

import Codd (applyMigrationsNoCheck, migsConnString)
import Codd.AppCommands.AddMigration (addMigration)
import Codd.Environment (CoddSettings (..))
import Codd.Internal (withConnection)
import Codd.Internal.MultiQueryStatement (SqlStatementException)
import Codd.Logging (runCoddLogger)
import Codd.Parsing (AddedSqlMigration (..), SqlMigration (..))
import Codd.Query (InTxnT, query, unsafeQuery1, withTransaction)
import Codd.Representations (readRepresentationsFromDbWithSettings)
import Codd.Types (ConnectionString (..), SqlFilePath (..), TxnIsolationLvl (..))
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Trans.Resource (MonadThrow)
import Data.List (isInfixOf, sortOn)
import qualified Data.List as List
import Data.String (fromString)
import Data.Time (UTCTime)
import qualified Database.PostgreSQL.Simple as DB
import DbUtils (aroundDatabaseWithMigs, aroundDatabaseWithMigsAndPgCron, aroundFreshDatabase, aroundTestDbInfo, createTestUserMig, getEmptyTempDir, getIncreasingTimestamp, mkValidSql, testConnTimeout)
import GHC.Generics (Generic)
import LiftedExpectations (shouldThrow)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec (Spec, shouldBe, shouldContain, shouldNotBe, shouldReturn, shouldSatisfy)
import Test.Hspec.Core.Spec (describe, it)
import qualified Test.QuickCheck as QC
import UnliftIO (MonadIO, SomeException, liftIO)
import UnliftIO.Concurrent (threadDelay)

-- |
-- The tests here exist to ensure good error messages on `codd add`, one of the first commands
-- users use.
-- They take specific error codes to mean a specific error, and assume a good message is printed to stderr.
spec :: Spec
spec = do
  describe "DbDependentSpecs" $ do
    describe "codd add" $ do
      aroundTestDbInfo $ do
        it "No directory for expected schema representation" tellsUserToCreateExpectedSchemaDir
        it "Very first migration but connection inaccessible has nice error message" tellsUserAboutBootstrapMigrationToCreateDatabase

tellsUserToCreateExpectedSchemaDir :: CoddSettings -> IO ()
tellsUserToCreateExpectedSchemaDir dbInfo = do
  emptyMigsFolder <- getEmptyTempDir
  someEmptyFolder <- getEmptyTempDir
  let missingFolder = someEmptyFolder </> "non-existing"
  assertExitsWithCode 97 $
    runCoddLogger $
      addMigration
        dbInfo {sqlMigrations = [emptyMigsFolder], onDiskReps = Left missingFolder}
        Nothing
        (SqlFilePath "test/migrations/codd-add-tests/very-first-migration-but-connection-not-accessible.sql")

tellsUserAboutBootstrapMigrationToCreateDatabase :: CoddSettings -> IO ()
tellsUserAboutBootstrapMigrationToCreateDatabase dbInfo = do
  -- The empty folder emulates a user adding their very first migration, but forgetting
  -- they need to `CREATE DATABASE` first.
  emptyMigsFolder <- getEmptyTempDir
  expectedSchemaDir <- getEmptyTempDir
  assertExitsWithCode 113 $ runCoddLogger $ do
    addMigration
      dbInfo {sqlMigrations = [emptyMigsFolder], onDiskReps = Left expectedSchemaDir}
      Nothing
      (SqlFilePath "test/migrations/codd-add-tests/very-first-migration-but-connection-not-accessible.sql")

assertExitsWithCode :: Int -> IO a -> IO ()
assertExitsWithCode code f =
  f
    `shouldThrow` ( \(e :: ExitCode) -> case e of
                      ExitFailure n -> n == code
                      ExitSuccess -> False
                  )
