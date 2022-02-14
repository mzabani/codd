module DbDependentSpecs.AnalysisSpec where

import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                )
import           Control.Monad                  ( forM_
                                                , void
                                                , when
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( unpack )
import qualified Database.PostgreSQL.Simple    as DB
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           DbUtils                        ( aroundDatabaseWithMigs
                                                , aroundFreshDatabase
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                )
import           Test.Hspec

createTableMig, addColumnMig, dropColumnMig, dropTableMig :: AddedSqlMigration
createTableMig = AddedSqlMigration
    SqlMigration { migrationName            = "0001-create-table.sql"
                 , migrationSql = Just $ mkValidSql "CREATE TABLE anytable ();"
                 , migrationInTxn           = True
                 , migrationCustomConnInfo = Nothing
                 }
    (getIncreasingTimestamp 1)
addColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName            = "0002-add-column.sql"
        , migrationSql             = Just
            $ mkValidSql "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;"
        , migrationInTxn           = True
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp 2)
dropColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName            = "0003-drop-column.sql"
        , migrationSql             = Just
            $ mkValidSql "ALTER TABLE anytable DROP COLUMN anycolumn;"
        , migrationInTxn           = True
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp 3)
dropTableMig = AddedSqlMigration
    SqlMigration { migrationName            = "0004-drop-table.sql"
                 , migrationSql = Just $ mkValidSql "DROP TABLE anytable;"
                 , migrationInTxn           = True
                 , migrationCustomConnInfo = Nothing
                 }
    (getIncreasingTimestamp 4)

spec :: Spec
spec = do
    let mkDbInfo baseDbInfo migs = baseDbInfo { sqlMigrations = Right migs }
    describe "DbDependentSpecs"
        $ describe "Analysis tests"
        $ context "Transaction modifying migrations"
        $ do
              it
                      "Non-destructive section of no-txn migrations can't leave transactions open"
                  $ void @IO
                  $ do
                        let badMigs = map
                                (\c -> SqlMigration
                                    { migrationName = "0000-begin.sql"
                                    , migrationSql = Just $ mkValidSql c
                                    , migrationInTxn = False
                                    , migrationCustomConnInfo = Nothing
                                    }
                                )
                                ["BEGIN", "BEGIN; BEGIN; SELECT 1;"]

                            goodMigs = map
                                (\c -> SqlMigration
                                    { migrationName = "0000-begin.sql"
                                    , migrationSql = Just $ mkValidSql c
                                    , migrationInTxn = False
                                    , migrationCustomConnInfo = Nothing
                                    }
                                )
                                [ "BEGIN;ROLLBACK"
                                , "BEGIN;COMMIT"
                                , "BEGIN; BEGIN; SELECT 1; ROLLBACK; SELECT 1;"
                                , "BEGIN; BEGIN; SELECT 1; COMMIT"
                                ]

                        forM_ badMigs $ \mig ->
                            checkMigration mig `shouldSatisfy` \case
                                Right (MigrationCheck (Just _)) -> True
                                _                               -> False

                        forM_ goodMigs $ \mig ->
                            checkMigration mig `shouldSatisfy` \case
                                Right (MigrationCheck Nothing) -> True
                                _                              -> False

              aroundFreshDatabase
                  $ it
                        "Non-destructive section containing COMMIT detected correctly"
                  $ \emptyTestDbInfo -> do
                        let commitTxnMig = SqlMigration
                                { migrationName            = "0000-commit.sql"
                                , migrationSql = Just $ mkValidSql "COMMIT;"
                                , migrationInTxn           = True
                                , migrationCustomConnInfo = Nothing
                                }
                        checkMigration commitTxnMig `shouldSatisfy` \case
                            Right (MigrationCheck (Just _)) -> True
                            _                               -> False
              aroundFreshDatabase
                  $ it
                        "Non-destructive section containing ROLLBACK detected correctly"
                  $ \emptyTestDbInfo -> do
                        let
                            rollbackTxnMig = SqlMigration
                                { migrationName            = "0000-rollback.sql"
                                , migrationSql = Just $ mkValidSql "ROLLBACK;"
                                , migrationInTxn           = True
                                , migrationCustomConnInfo = Nothing
                                }
                        checkMigration rollbackTxnMig `shouldSatisfy` \case
                            Right (MigrationCheck (Just _)) -> True
                            _                               -> False
