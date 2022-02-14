module DbDependentSpecs.AppCommandsSpec where

import           Codd.AppCommands.VerifyChecksums
                                                ( verifyChecksums )
import           Codd.AppCommands.WriteChecksums
                                                ( WriteChecksumsOpts
                                                    ( WriteToStdout
                                                    )
                                                , writeChecksums
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( DbHashes(..) )
import           Codd.Hashing.Types             ( ObjHash(ObjHash) )
import           Codd.Internal                  ( withConnection )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                )
import           Codd.Query                     ( execvoid_
                                                , unsafeQuery1
                                                )
import           Control.Monad.Logger           ( LoggingT
                                                , runStdoutLoggingT
                                                )
import           Data.List                      ( isInfixOf )
import qualified Data.Map                      as Map
import qualified Database.PostgreSQL.Simple    as DB
import           DbUtils                        ( getIncreasingTimestamp
                                                , mkValidSql
                                                , testCoddSettings
                                                )
import           LiftedExpectations             ( shouldThrow )
import           System.Exit                    ( ExitCode )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           UnliftIO                       ( IOException )


migThatWontRun :: AddedSqlMigration
migThatWontRun = AddedSqlMigration
    SqlMigration
        { migrationName            = "create-things.sql"
        , nonDestructiveSql        = Just
                                     $ mkValidSql
                                     $ "CREATE USER \"user-that-wont-exist\";\n"
                                     <> "CREATE TABLE table_that_wont_exist();\n"
                                     <> "CREATE SCHEMA schema_that_wont_exist;"
        , nonDestructiveForce      = True
        , nonDestructiveInTxn      = True
        , nonDestructiveCustomConn = Nothing
        }
    (getIncreasingTimestamp 99999)

doesNotCreateDB :: (CoddSettings -> LoggingT IO a) -> IO ()
doesNotCreateDB act = do
    vanillaTestSettings <- testCoddSettings []
    let testSettings = vanillaTestSettings
            { dbName         = "non-existing-db-name"
            , onDiskHashes   = Right $ DbHashes (ObjHash "") Map.empty Map.empty
            , sqlMigrations  = Right [migThatWontRun]
            , migsConnString = (migsConnString vanillaTestSettings)
                                   { DB.connectDatabase = "non-existing-db-name"
                                   }
            }
    runStdoutLoggingT $ do
        -- libpq's fatal connection error is an IOException
        verifyChecksums testSettings False
            `shouldThrow` (\(e :: IOException) ->
                              "database \"non-existing-db-name\" does not exist"
                                  `isInfixOf` show e
                          )

    withConnection (migsConnString testSettings)
            { DB.connectDatabase = "postgres"
            }
        $ \conn -> do
              dbExists :: Int <- DB.fromOnly <$> unsafeQuery1
                  conn
                  "SELECT COUNT(*) FROM pg_database WHERE datname = ?"
                  (DB.Only $ dbName testSettings)
              dbExists `shouldBe` 0

doesNotModifyExistingDb
    :: (CoddSettings -> LoggingT IO a) -> (IO a -> IO ()) -> IO ()
doesNotModifyExistingDb act assert = do
    vanillaTestSettings <- testCoddSettings []
    let testSettings = vanillaTestSettings
            { dbName         = "new_checksums_test_db"
            , onDiskHashes   = Right $ DbHashes (ObjHash "") Map.empty Map.empty
            , sqlMigrations  = Right [migThatWontRun]
            , migsConnString = (migsConnString vanillaTestSettings)
                                   { DB.connectDatabase =
                                       "new_checksums_test_db"
                                   }
            }

        getCounts =
            withConnection (migsConnString vanillaTestSettings)
                    { DB.connectDatabase = "postgres"
                    }
                $ \conn -> unsafeQuery1 @(Int, Int, Int)
                      conn
                      "SELECT (SELECT COUNT(*) FROM pg_catalog.pg_namespace), (SELECT COUNT(*) FROM pg_catalog.pg_class), (SELECT COUNT(*) FROM pg_catalog.pg_roles)"
                      ()
    withConnection (migsConnString vanillaTestSettings)
            { DB.connectDatabase = "postgres"
            }
        $ \conn -> do
              execvoid_ conn "DROP DATABASE IF EXISTS new_checksums_test_db"
              execvoid_ conn "CREATE DATABASE new_checksums_test_db"

    countsBefore <- getCounts
    assert $ runStdoutLoggingT $ act testSettings
    countsAfter <- getCounts
    countsAfter `shouldBe` countsBefore

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Application Commands tests" $ do
            it
                    "verify-checksums does not create Database when it does not exist"
                $ doesNotCreateDB
                $ \testSettings -> verifyChecksums testSettings False

            it "write-checksums does not create Database when it does not exist"
                $ doesNotCreateDB
                $ \testSettings -> writeChecksums testSettings WriteToStdout

            it "verify-checksums does not write to existing Database"
                $ doesNotModifyExistingDb
                      (\testSettings -> verifyChecksums testSettings False)
                      (-- Throws because expected hashes do not match
                       `shouldThrow` (\(_ :: ExitCode) -> True))
            it "write-checksums does not write to existing Database"
                $ doesNotModifyExistingDb
                      (\testSettings ->
                          writeChecksums testSettings WriteToStdout
                      )
                      id
