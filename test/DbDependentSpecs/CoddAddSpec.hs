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
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import DbDependentSpecs.RetrySpec (runMVarLogger)
import DbUtils (aroundDatabaseWithMigs, aroundDatabaseWithMigsAndPgCron, aroundFreshDatabase, aroundTestDbInfo, createTestUserMig, getEmptyTempDir, getIncreasingTimestamp, mkValidSql, testConnTimeout)
import LiftedExpectations (shouldThrow)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec (Spec, shouldBe, shouldContain, shouldNotBe, shouldReturn, shouldSatisfy)
import Test.Hspec.Core.Spec (describe, it)
import qualified Test.QuickCheck as QC
import UnliftIO (MonadIO, SomeException, liftIO)
import UnliftIO.Concurrent (newMVar, readMVar, threadDelay)

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
      aroundFreshDatabase $ it "Background migration prints tip and adds special top-level comment" backgroundMigrationPrintsTipAndAddsSpecialToplevelComment

tellsUserToCreateExpectedSchemaDir :: CoddSettings -> IO ()
tellsUserToCreateExpectedSchemaDir dbInfo = do
  emptyMigsFolder <- getEmptyTempDir
  someEmptyFolder <- getEmptyTempDir
  let missingFolder = someEmptyFolder </> "non-existing"
  assertExitsWithCode 97 $
    runCoddLogger $
      void $
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
  assertExitsWithCode 113 $
    runCoddLogger $
      void $
        addMigration
          dbInfo {sqlMigrations = [emptyMigsFolder], onDiskReps = Left expectedSchemaDir}
          Nothing
          (SqlFilePath "test/migrations/codd-add-tests/very-first-migration-but-connection-not-accessible.sql")

backgroundMigrationPrintsTipAndAddsSpecialToplevelComment :: CoddSettings -> IO ()
backgroundMigrationPrintsTipAndAddsSpecialToplevelComment dbInfo = do
  emptyMigsFolder <- getEmptyTempDir
  expectedSchemaDir <- getEmptyTempDir
  logsmv <- newMVar []
  finalMigFile <-
    runMVarLogger logsmv $
      addMigration
        dbInfo {sqlMigrations = [emptyMigsFolder], onDiskReps = Left expectedSchemaDir}
        Nothing
        (SqlFilePath "test/migrations/codd-add-tests/example-background-migration.sql")
  logs <- readMVar logsmv
  -- Check a nice tip was printed
  logs
    `shouldSatisfy` any
      ( \l ->
          "SELECT codd.synchronously_finalize_background_job('user-job-name',"
            `Text.isInfixOf` l
      )
  -- Read the target file and check it has the nice top-level instructions
  addedTimestampedMigContents <- Text.readFile finalMigFile
  let contentsLines = Text.split (== '\n') addedTimestampedMigContents
  take 2 contentsLines
    `shouldBe` [ "-- codd: requires-codd-schema",
                 "-- Comment above added automatically by codd since this migration requires the 'codd' schema to exist. Please don't remove it. You can add more '-- codd:' top-level comments at the top of the file or even below this line. You can also remove this comment as it's purely instructive."
               ]

assertExitsWithCode :: Int -> IO a -> IO ()
assertExitsWithCode code f =
  f
    `shouldThrow` ( \(e :: ExitCode) -> case e of
                      ExitFailure n -> n == code
                      ExitSuccess -> False
                  )
