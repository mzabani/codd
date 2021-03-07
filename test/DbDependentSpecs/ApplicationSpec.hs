module DbDependentSpecs.ApplicationSpec where

import           Codd                           ( applyMigrations
                                                , withDbAndDrop
                                                )
import           Codd.Analysis                  ( DestructiveSectionCheck(..)
                                                , MigrationCheck(..)
                                                , NonDestructiveSectionCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..)
                                                , superUserInAppDatabaseConnInfo
                                                )
import           Codd.Hashing.Types             ( DbHashes(..)
                                                , ObjHash(..)
                                                )
import           Codd.Internal                  ( withConnection )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                )
import           Codd.Query                     ( unsafeQuery1 )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( unpack )
import qualified Database.PostgreSQL.Simple    as DB
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           DbUtils                        ( aroundFreshDatabase
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                )
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.QuickCheck

placeHoldersMig, selectMig, copyMig :: AddedSqlMigration
placeHoldersMig = AddedSqlMigration
    SqlMigration
        { migrationName       = "0000-placeholders.sql"
        , nonDestructiveSql   = Just
            $ mkValidSql "CREATE TABLE any_table();\n-- ? $1 $2 ? ? ?"
        , nonDestructiveForce = False
        , nonDestructiveInTxn = True
        , destructiveSql      = Nothing
        , destructiveInTxn    = True
        }
    (getIncreasingTimestamp 0)
selectMig = AddedSqlMigration
    SqlMigration { migrationName       = "0001-select-mig.sql"
                 , nonDestructiveSql   = Just $ mkValidSql "SELECT 1, 3"
                 , nonDestructiveForce = True
                 , nonDestructiveInTxn = False
                 , destructiveSql      = Nothing
                 , destructiveInTxn    = True
                 }
    (getIncreasingTimestamp 1)
copyMig = AddedSqlMigration
    SqlMigration
        { migrationName       = "0002-copy-mig.sql"
        , nonDestructiveSql   =
            Just
                $ mkValidSql
                      "CREATE TABLE x(name TEXT); COPY x (name) FROM STDIN WITH (FORMAT CSV);\nSome name\n\\.\n COPY x FROM STDIN WITH (FORMAT CSV);\n\\.\n "
        , nonDestructiveForce = True
        , nonDestructiveInTxn = False
        , destructiveSql      = Nothing
        , destructiveInTxn    = True
        }
    (getIncreasingTimestamp 2)

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Application tests" $ do
            aroundFreshDatabase $ do
                it
                        "SQL containing characters typical to placeholders does not throw"
                    $ \emptyTestDbInfo -> do
                          void @IO $ runStdoutLoggingT $ applyMigrations
                              (emptyTestDbInfo
                                  { sqlMigrations = Right [placeHoldersMig]
                                  }
                              )
                              False

                it "Rows returning function work for no-txn migrations"
                    $ \emptyTestDbInfo -> do
                          void @IO $ runStdoutLoggingT $ applyMigrations
                              (emptyTestDbInfo
                                  { sqlMigrations = Right [selectMig]
                                  }
                              )
                              False

                it "Rows returning function work for in-txn migrations"
                    $ \emptyTestDbInfo -> do
                          let (AddedSqlMigration mig t) = selectMig
                              inTxnMig                  = AddedSqlMigration
                                  mig { nonDestructiveForce = False
                                      , nonDestructiveInTxn = True
                                      }
                                  t
                          void @IO $ runStdoutLoggingT $ applyMigrations
                              (emptyTestDbInfo
                                  { sqlMigrations = Right [inTxnMig]
                                  }
                              )
                              False

                it "COPY works well" $ \emptyTestDbInfo -> do
                    void @IO $ runStdoutLoggingT $ applyMigrations
                        (emptyTestDbInfo { sqlMigrations = Right [copyMig] })
                        False

                it "Bogus on-disk hashes makes applying migrations fail"
                    $ \emptyTestDbInfo -> do
                          let bogusDbHashes =
                                  DbHashes (ObjHash "") Map.empty Map.empty
                          void @IO $ do
                              runStdoutLoggingT
                                      (applyMigrations
                                          (emptyTestDbInfo
                                              { sqlMigrations = Right
                                                  [placeHoldersMig]
                                              , onDiskHashes  = Right
                                                                    bogusDbHashes
                                              }
                                          )
                                          True
                                      )
                                  `shouldThrow` anyIOException

                it
                        "no-txn migrations and in-txn migrations run in intertwined blocks"
                    $ \emptyTestDbInfo -> do
                          let
                              migs =
                                  [ AddedSqlMigration
                                      SqlMigration
                                          { migrationName       =
                                              "0000-first-in-txn-mig.sql"
                                          , nonDestructiveSql   =
                                              Just
                                              $ mkValidSql
                                              $ "CREATE TABLE any_table (txid bigint not null);"
                                              <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                              <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                          -- One unique txid from this migration, two rows
                                          , nonDestructiveForce = False
                                          , nonDestructiveInTxn = True
                                          , destructiveSql      = Nothing
                                          , destructiveInTxn    = True
                                          }
                                      (getIncreasingTimestamp 0)
                                  , AddedSqlMigration
                                      SqlMigration
                                          { migrationName       =
                                              "0001-second-in-txn-mig.sql"
                                          , nonDestructiveSql   =
                                              Just
                                              $ mkValidSql
                                              $ "INSERT INTO any_table (txid) VALUES (txid_current());"
                                              <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                          -- No txids from this migration because it runs in the same transaction as the last one, two more rows
                                          , nonDestructiveForce = False
                                          , nonDestructiveInTxn = True
                                          , destructiveSql      = Nothing
                                          , destructiveInTxn    = True
                                          }
                                      (getIncreasingTimestamp 1)
                                  , AddedSqlMigration
                                      SqlMigration
                                          { migrationName       =
                                              "0002-no-txn-mig.sql"
                                          , nonDestructiveSql   =
                                              Just
                                              $ mkValidSql
                                              $ "CREATE TYPE experience AS ENUM ('junior', 'senior');"
                                              <> "\nALTER TABLE any_table ADD COLUMN experience experience;"
                                              <> "\nALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';"
                                              <> "\nUPDATE any_table SET experience='intern';"
                                              <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                              <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                          -- Two distinct txids because this one doesn't run in a migration and two more rows
                                          , nonDestructiveForce = True
                                          , nonDestructiveInTxn = False
                                          , destructiveSql      = Nothing
                                          , destructiveInTxn    = True
                                          }
                                      (getIncreasingTimestamp 2)
                                  , AddedSqlMigration
                                      SqlMigration
                                          { migrationName       =
                                              "0003-second-in-txn-mig.sql"
                                          , nonDestructiveSql   =
                                              Just
                                              $ mkValidSql
                                              $ "INSERT INTO any_table (txid) VALUES (txid_current());"
                                              <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                          -- One unique txid from this migration because it runs in a new transaction, two more rows
                                          , nonDestructiveForce = False
                                          , nonDestructiveInTxn = True
                                          , destructiveSql      = Nothing
                                          , destructiveInTxn    = True
                                          }
                                      (getIncreasingTimestamp 3)
                                  , AddedSqlMigration
                                      SqlMigration
                                          { migrationName       =
                                              "0004-second-in-txn-mig.sql"
                                          , nonDestructiveSql   =
                                              Just
                                              $ mkValidSql
                                              $ "INSERT INTO any_table (txid) VALUES (txid_current());"
                                              <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                          -- No txids from this migration because it runs in the same transaction as the last one, two more rows
                                          , nonDestructiveForce = False
                                          , nonDestructiveInTxn = True
                                          , destructiveSql      = Nothing
                                          , destructiveInTxn    = True
                                          }
                                      (getIncreasingTimestamp 4)
                                  ]

                          void @IO $ runStdoutLoggingT $ applyMigrations
                              (emptyTestDbInfo { sqlMigrations = Right migs })
                              False
                          withConnection
                                  (superUserInAppDatabaseConnInfo
                                      emptyTestDbInfo
                                  )
                              $ \conn -> do
                                    (countTxIds :: Int, countInterns :: Int, totalRows :: Int) <-
                                        unsafeQuery1
                                            conn
                                            "SELECT (SELECT COUNT(DISTINCT txid) FROM any_table), (SELECT COUNT(*) FROM any_table WHERE experience='intern'), (SELECT COUNT(*) FROM any_table);"
                                            ()
                                    countTxIds `shouldBe` 4
                                    countInterns `shouldBe` 4
                                    totalRows `shouldBe` 10
