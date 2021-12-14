module DbDependentSpecs.AnalysisSpec where

import           Codd                           ( withDbAndDrop )
import           Codd.Analysis                  ( DestructiveSectionCheck(..)
                                                , MigrationCheck(..)
                                                , MigrationCheckSimpleWorkflow
                                                    ( MigrationCheckSimpleWorkflow
                                                    )
                                                , NonDestructiveSectionCheck(..)
                                                , checkMigration
                                                , checkMigrationSimpleWorkflow
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
    SqlMigration
        { migrationName            = "0001-create-table.sql"
        , nonDestructiveSql = Just $ mkValidSql "CREATE TABLE anytable ();"
        , nonDestructiveForce      = False
        , nonDestructiveInTxn      = True
        , nonDestructiveCustomConn = Nothing
        , destructiveSql           = Nothing
        , destructiveInTxn         = True
        }
    (getIncreasingTimestamp 1)
addColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName            = "0002-add-column.sql"
        , nonDestructiveSql        = Just
            $ mkValidSql "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;"
        , nonDestructiveForce      = False
        , nonDestructiveInTxn      = True
        , nonDestructiveCustomConn = Nothing
        , destructiveSql           = Nothing
        , destructiveInTxn         = True
        }
    (getIncreasingTimestamp 2)
dropColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName            = "0003-drop-column.sql"
        , nonDestructiveSql        = Just
            $ mkValidSql "ALTER TABLE anytable DROP COLUMN anycolumn;"
        , nonDestructiveForce      = True
        , nonDestructiveInTxn      = True
        , nonDestructiveCustomConn = Nothing
        , destructiveSql           = Nothing
        , destructiveInTxn         = True
        }
    (getIncreasingTimestamp 3)
dropTableMig = AddedSqlMigration
    SqlMigration { migrationName            = "0004-drop-table.sql"
                 , nonDestructiveSql = Just $ mkValidSql "DROP TABLE anytable;"
                 , nonDestructiveForce      = True
                 , nonDestructiveInTxn      = True
                 , nonDestructiveCustomConn = Nothing
                 , destructiveSql           = Nothing
                 , destructiveInTxn         = True
                 }
    (getIncreasingTimestamp 4)

isMigrationDestructive :: MigrationCheck -> Bool
isMigrationDestructive (MigrationCheck nonDestCheck _) =
    nonDestSectionIsDestructive nonDestCheck

isNonDestTxnClosing :: MigrationCheck -> Bool
isNonDestTxnClosing (MigrationCheck nonDestCheck _) =
    nonDestSectionEndsTransaction nonDestCheck

isDestTxnClosing :: MigrationCheck -> Bool
isDestTxnClosing (MigrationCheck _ destCheck) =
    destSectionEndsTransaction destCheck

spec :: Spec
spec = do
    let mkDbInfo baseDbInfo migs = baseDbInfo { sqlMigrations = Right migs }
    describe "DbDependentSpecs" $ do
        describe "Analysis tests" $ do
            context "No False positives for destructiveness checks" $ do
                aroundFreshDatabase
                    $ it "Create Table is non-destructive"
                    $ \emptyTestDbInfo ->
                          isMigrationDestructive
                              <$>            (runStdoutLoggingT $ checkMigration
                                                 emptyTestDbInfo
                                                 (addedSqlMig createTableMig)
                                             )
                              `shouldReturn` False

                aroundDatabaseWithMigs [createTableMig]
                    $ it "Adding columns is non-destructive"
                    $ \dbInfo ->
                          isMigrationDestructive
                              <$>            (runStdoutLoggingT $ checkMigration
                                                 dbInfo
                                                 (addedSqlMig addColumnMig)
                                             )
                              `shouldReturn` False

            context "Obviously destructive actions detected as such" $ do
                aroundFreshDatabase
                    $ it "Dropping columns is destructive"
                    $ \emptyTestDbInfo ->
                          isMigrationDestructive
                              <$>            (runStdoutLoggingT $ checkMigration
                                                 (mkDbInfo
                                                     emptyTestDbInfo
                                                     [createTableMig, addColumnMig]
                                                 )
                                                 (addedSqlMig dropColumnMig)
                                             )
                              `shouldReturn` True

                aroundFreshDatabase
                    $ it "Dropping tables is destructive"
                    $ \emptyTestDbInfo -> do
                          isMigrationDestructive
                              <$>            (runStdoutLoggingT $ checkMigration
                                                 (mkDbInfo
                                                     emptyTestDbInfo
                                                     [createTableMig, addColumnMig]
                                                 )
                                                 (addedSqlMig dropTableMig)
                                             )
                              `shouldReturn` True
                          isMigrationDestructive
                              <$>            (runStdoutLoggingT $ checkMigration
                                                 (mkDbInfo
                                                     emptyTestDbInfo
                                                     [ createTableMig
                                                     , addColumnMig
                                                     , dropColumnMig
                                                     ]
                                                 )
                                                 (addedSqlMig dropTableMig)
                                             )
                              `shouldReturn` True

            context "Transaction modifying migrations" $ do
                it
                        "Non-destructive section of no-txn migrations can't leave transactions open"
                    $ void @IO
                    $ do
                          let badMigs = map
                                  (\c -> SqlMigration
                                      { migrationName = "0000-begin.sql"
                                      , nonDestructiveSql = Just $ mkValidSql c
                                      , nonDestructiveForce      = False
                                      , nonDestructiveInTxn      = False
                                      , nonDestructiveCustomConn = Nothing
                                      , destructiveSql           = Nothing
                                      , destructiveInTxn         = True
                                      }
                                  )
                                  ["BEGIN", "BEGIN; BEGIN; SELECT 1;"]

                              goodMigs = map
                                  (\c -> SqlMigration
                                      { migrationName = "0000-begin.sql"
                                      , nonDestructiveSql = Just $ mkValidSql c
                                      , nonDestructiveForce      = False
                                      , nonDestructiveInTxn      = False
                                      , nonDestructiveCustomConn = Nothing
                                      , destructiveSql           = Nothing
                                      , destructiveInTxn         = True
                                      }
                                  )
                                  [ "BEGIN;ROLLBACK"
                                  , "BEGIN;COMMIT"
                                  , "BEGIN; BEGIN; SELECT 1; ROLLBACK; SELECT 1;"
                                  , "BEGIN; BEGIN; SELECT 1; COMMIT"
                                  ]

                          forM_ badMigs $ \mig ->
                              checkMigrationSimpleWorkflow mig
                                  `shouldSatisfy` \case
                                                      Right (MigrationCheckSimpleWorkflow (Just _))
                                                          -> True
                                                      _ -> False

                          forM_ goodMigs $ \mig ->
                              checkMigrationSimpleWorkflow mig
                                  `shouldSatisfy` \case
                                                      Right (MigrationCheckSimpleWorkflow Nothing)
                                                          -> True
                                                      _ -> False

                aroundFreshDatabase
                    $ it
                          "Non-destructive section containing COMMIT detected correctly"
                    $ \emptyTestDbInfo -> do
                          let
                              commitTxnMig = SqlMigration
                                  { migrationName            = "0000-commit.sql"
                                  , nonDestructiveSql        = Just
                                      $ mkValidSql "COMMIT;"
                                  , nonDestructiveForce      = False
                                  , nonDestructiveInTxn      = True
                                  , nonDestructiveCustomConn = Nothing
                                  , destructiveSql           = Nothing
                                  , destructiveInTxn         = True
                                  }
                          isNonDestTxnClosing
                              <$>            (runStdoutLoggingT $ checkMigration
                                                 emptyTestDbInfo
                                                 commitTxnMig
                                             )
                              `shouldReturn` True
                          checkMigrationSimpleWorkflow commitTxnMig
                              `shouldSatisfy` \case
                                                  Right (MigrationCheckSimpleWorkflow (Just _))
                                                      -> True
                                                  _ -> False
                aroundFreshDatabase
                    $ it
                          "Non-destructive section containing ROLLBACK detected correctly"
                    $ \emptyTestDbInfo -> do
                          let
                              rollbackTxnMig = SqlMigration
                                  { migrationName = "0000-rollback.sql"
                                  , nonDestructiveSql        = Just
                                      $ mkValidSql "ROLLBACK;"
                                  , nonDestructiveForce      = False
                                  , nonDestructiveInTxn      = True
                                  , nonDestructiveCustomConn = Nothing
                                  , destructiveSql           = Nothing
                                  , destructiveInTxn         = True
                                  }
                          isNonDestTxnClosing
                              <$>            (runStdoutLoggingT $ checkMigration
                                                 emptyTestDbInfo
                                                 rollbackTxnMig
                                             )
                              `shouldReturn` True
                          checkMigrationSimpleWorkflow rollbackTxnMig
                              `shouldSatisfy` \case
                                                  Right (MigrationCheckSimpleWorkflow (Just _))
                                                      -> True
                                                  _ -> False
                aroundFreshDatabase
                    $ it
                          "Destructive section that ends transactions when non-destructive section also ends Transactions detected correctly"
                    $ \emptyTestDbInfo ->
                          let
                              rollbackTxnMig = SqlMigration
                                  { migrationName = "0000-rollback.sql"
                                  , nonDestructiveSql        = Just
                                      $ mkValidSql "ROLLBACK;"
                                  , nonDestructiveForce      = False
                                  , nonDestructiveInTxn      = True
                                  , nonDestructiveCustomConn = Nothing
                                  , destructiveSql           = Just
                                      $ mkValidSql "ROLLBACK;"
                                  , destructiveInTxn         = True
                                  }
                          in
                              isDestTxnClosing
                              <$>            (runStdoutLoggingT $ checkMigration
                                                 emptyTestDbInfo
                                                 rollbackTxnMig
                                             )
                              `shouldReturn` True
                aroundFreshDatabase
                    $ it
                          "Destructive section that does not end transactions when non-destructive section also ends Transactions detected correctly"
                    $ \emptyTestDbInfo ->
                          let
                              rollbackTxnMig = SqlMigration
                                  { migrationName = "0000-rollback.sql"
                                  , nonDestructiveSql        = Just
                                      $ mkValidSql "ROLLBACK;"
                                  , nonDestructiveForce      = False
                                  , nonDestructiveInTxn      = True
                                  , nonDestructiveCustomConn = Nothing
                                  , destructiveSql           = Nothing
                                  , destructiveInTxn         = True
                                  }
                          in
                              isDestTxnClosing
                              <$>            runStdoutLoggingT
                                                 (checkMigration emptyTestDbInfo
                                                                 rollbackTxnMig
                                                 )
                              `shouldReturn` False
