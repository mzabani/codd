module DbDependentSpecs.AnalysisSpec where

import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                )
import           Control.Monad                  ( (>=>)
                                                , forM_
                                                , void
                                                )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import           DbUtils                        ( getIncreasingTimestamp
                                                , mkValidSql
                                                )
import           Test.Hspec

createTableMig, addColumnMig, dropColumnMig, dropTableMig
    :: MonadThrow m => AddedSqlMigration m
createTableMig = AddedSqlMigration
    SqlMigration { migrationName           = "0001-create-table.sql"
                 , migrationSql = mkValidSql "CREATE TABLE anytable ();"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 1)
addColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0002-add-column.sql"
        , migrationSql            = mkValidSql
                                        "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 2)
dropColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0003-drop-column.sql"
        , migrationSql            = mkValidSql
                                        "ALTER TABLE anytable DROP COLUMN anycolumn;"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 3)
dropTableMig = AddedSqlMigration
    SqlMigration { migrationName           = "0004-drop-table.sql"
                 , migrationSql            = mkValidSql "DROP TABLE anytable;"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 4)

spec :: Spec
spec = do
    describe "DbDependentSpecs"
        $ describe "Analysis tests"
        $ context "Transaction modifying migrations"
        $ do
              it "no-txn migrations can't leave transactions open"
                  $ void @IO
                  $ do
                        let badMigs = map
                                (\c -> SqlMigration
                                    { migrationName           = "0000-begin.sql"
                                    , migrationSql            = mkValidSql c
                                    , migrationInTxn          = False
                                    , migrationCustomConnInfo = Nothing
                                    , migrationEnvVars        = mempty
                                    }
                                )
                                ["BEGIN", "BEGIN; BEGIN; SELECT 1;"]

                            goodMigs = map
                                (\c -> SqlMigration
                                    { migrationName           = "0000-begin.sql"
                                    , migrationSql            = mkValidSql c
                                    , migrationInTxn          = False
                                    , migrationCustomConnInfo = Nothing
                                    , migrationEnvVars        = mempty
                                    }
                                )
                                [ "BEGIN;ROLLBACK"
                                , "BEGIN;COMMIT"
                                , "BEGIN; BEGIN; SELECT 1; ROLLBACK; SELECT 1;"
                                , "BEGIN; BEGIN; SELECT 1; COMMIT"
                                ]

                        forM_
                            badMigs
                            (   checkMigration
                            >=> (`shouldSatisfy` \case
                                    Right (MigrationCheck (Just _)) -> True
                                    _                               -> False
                                )
                            )

                        forM_ goodMigs
                            $   checkMigration
                            >=> (`shouldSatisfy` \case
                                    Right (MigrationCheck Nothing) -> True
                                    _                              -> False
                                )

              it "in-txn migration containing COMMIT detected correctly" $ do
                  let commitTxnMig = SqlMigration
                          { migrationName           = "0000-commit.sql"
                          , migrationSql            = mkValidSql "COMMIT;"
                          , migrationInTxn          = True
                          , migrationCustomConnInfo = Nothing
                          , migrationEnvVars        = mempty
                          }
                  checkMigration commitTxnMig
                      >>= (`shouldSatisfy` \case
                              Right (MigrationCheck (Just _)) -> True
                              _                               -> False
                          )
              it "in-txn migration containing ROLLBACK detected correctly" $ do
                  let rollbackTxnMig = SqlMigration
                          { migrationName           = "0000-rollback.sql"
                          , migrationSql            = mkValidSql "ROLLBACK;"
                          , migrationInTxn          = True
                          , migrationCustomConnInfo = Nothing
                          , migrationEnvVars        = mempty
                          }
                  checkMigration rollbackTxnMig
                      >>= (`shouldSatisfy` \case
                              Right (MigrationCheck (Just _)) -> True
                              _                               -> False
                          )
