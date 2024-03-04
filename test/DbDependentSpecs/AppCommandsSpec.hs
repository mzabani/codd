module DbDependentSpecs.AppCommandsSpec where

import           Codd.AppCommands.VerifySchema  ( verifySchema )
import           Codd.AppCommands.WriteSchema   ( WriteSchemaOpts(WriteToStdout)
                                                , writeSchema
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( withConnection )
import           Codd.Logging                   ( Verbosity(..)
                                                , runCoddLogger
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                )
import           Codd.Query                     ( execvoid_
                                                , unsafeQuery1
                                                )
import           Codd.Representations           ( DbRep(..) )
import           Control.Monad.Logger           ( LoggingT )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import qualified Data.Aeson                    as Aeson
import           Data.List                      ( isInfixOf )
import qualified Data.Map                      as Map
import qualified Database.PostgreSQL.Simple    as DB
import           DbUtils                        ( getIncreasingTimestamp
                                                , mkValidSql
                                                , testCoddSettings
                                                , testConnTimeout
                                                )
import           LiftedExpectations             ( shouldThrow )
import           System.Exit                    ( ExitCode )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           UnliftIO                       ( IOException )


migThatWontRun :: MonadThrow m => AddedSqlMigration m
migThatWontRun = AddedSqlMigration
    SqlMigration
        { migrationName           = "create-things.sql"
        , migrationSql            = mkValidSql
                                    $  "CREATE USER \"user-that-wont-exist\";\n"
                                    <> "CREATE TABLE table_that_wont_exist();\n"
                                    <> "CREATE SCHEMA schema_that_wont_exist;"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 99999)

doesNotCreateDB :: (CoddSettings -> LoggingT IO a) -> IO ()
doesNotCreateDB act = do
    vanillaTestSettings <- testCoddSettings
    let testSettings = vanillaTestSettings
            { onDiskReps     = Right $ DbRep Aeson.Null Map.empty Map.empty
            , migsConnString = (migsConnString vanillaTestSettings)
                                   { DB.connectDatabase = "non-existing-db-name"
                                   }
            }
    runCoddLogger Verbose $ do
        -- libpq's fatal connection error is an IOException
        verifySchema testSettings False
            `shouldThrow` (\(e :: IOException) ->
                              "database \"non-existing-db-name\" does not exist"
                                  `isInfixOf` show e
                          )

    withConnection
            (migsConnString testSettings) { DB.connectDatabase = "postgres" }
            testConnTimeout
        $ \conn -> do
              dbExists :: Int <- DB.fromOnly <$> unsafeQuery1
                  conn
                  "SELECT COUNT(*) FROM pg_database WHERE datname = ?"
                  (DB.Only $ DB.connectDatabase $ migsConnString testSettings)
              dbExists `shouldBe` 0

doesNotModifyExistingDb
    :: (CoddSettings -> LoggingT IO a) -> (IO a -> IO ()) -> IO ()
doesNotModifyExistingDb act assert = do
    vanillaTestSettings <- testCoddSettings
    let testSettings = vanillaTestSettings
            { onDiskReps     = Right $ DbRep Aeson.Null Map.empty Map.empty
            , migsConnString = (migsConnString vanillaTestSettings)
                                   { DB.connectDatabase =
                                       "new_checksums_test_db"
                                   }
            }

        getCounts =
            withConnection
                    (migsConnString vanillaTestSettings)
                        { DB.connectDatabase = "postgres"
                        }
                    testConnTimeout
                $ \conn -> unsafeQuery1 @(Int, Int, Int)
                      conn
                      "SELECT (SELECT COUNT(*) FROM pg_catalog.pg_namespace), (SELECT COUNT(*) FROM pg_catalog.pg_class), (SELECT COUNT(*) FROM pg_catalog.pg_roles)"
                      ()
    withConnection
            (migsConnString vanillaTestSettings)
                { DB.connectDatabase = "postgres"
                }
            testConnTimeout
        $ \conn -> do
              execvoid_ conn "DROP DATABASE IF EXISTS new_checksums_test_db"
              execvoid_ conn "CREATE DATABASE new_checksums_test_db"

    countsBefore <- getCounts
    assert $ runCoddLogger Verbose $ act testSettings
    countsAfter <- getCounts
    countsAfter `shouldBe` countsBefore

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Application Commands tests" $ do
            it "verify-schema does not create Database when it does not exist"
                $ doesNotCreateDB
                $ \testSettings -> verifySchema testSettings False

            it "write-schema does not create Database when it does not exist"
                $ doesNotCreateDB
                $ \testSettings -> writeSchema testSettings WriteToStdout

            it "verify-schema does not write to existing Database"
                $ doesNotModifyExistingDb
                      (`verifySchema` False)
                      (-- Throws because expected hashes do not match
                       `shouldThrow` (\(_ :: ExitCode) -> True))
            it "write-schema does not write to existing Database"
                $ doesNotModifyExistingDb (`writeSchema` WriteToStdout) id
