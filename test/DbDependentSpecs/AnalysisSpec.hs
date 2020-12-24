module DbDependentSpecs.AnalysisSpec where

import Codd (withDbAndDrop)
import Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration)
import Codd.Types (CoddSettings(..), SqlMigration(..), AddedSqlMigration(..))
import Control.Monad (when)
import Control.Monad.Logger (runStdoutLoggingT)
import DbUtils (aroundFreshDatabase, aroundDatabaseWithMigs, getIncreasingTimestamp)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Data.Text (unpack)
import Test.Hspec

createTableMig, addColumnMig, dropColumnMig, dropTableMig :: AddedSqlMigration
createTableMig = AddedSqlMigration SqlMigration {
                    migrationName = "0001-create-table.sql"
                    , nonDestructiveSql = Just "CREATE TABLE anytable ();"
                    , nonDestructiveForce = False
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                } (getIncreasingTimestamp 1)
addColumnMig = AddedSqlMigration SqlMigration {
                    migrationName = "0002-add-column.sql"
                    , nonDestructiveSql = Just "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;"
                    , nonDestructiveForce = False
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                } (getIncreasingTimestamp 2)
dropColumnMig = AddedSqlMigration SqlMigration {
                    migrationName = "0003-drop-column.sql"
                    , nonDestructiveSql = Just "ALTER TABLE anytable DROP COLUMN anycolumn;"
                    , nonDestructiveForce = True
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                } (getIncreasingTimestamp 3)
dropTableMig = AddedSqlMigration SqlMigration {
                    migrationName = "0004-drop-table.sql"
                    , nonDestructiveSql = Just "DROP TABLE anytable;"
                    , nonDestructiveForce = True
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                } (getIncreasingTimestamp 4)

isMigrationDestructive :: MigrationCheck -> Bool
isMigrationDestructive (MigrationCheck nonDestCheck _) = nonDestSectionIsDestructive nonDestCheck

isNonDestTxnClosing :: MigrationCheck -> Bool
isNonDestTxnClosing (MigrationCheck nonDestCheck _) = nonDestSectionEndsTransaction nonDestCheck

isDestTxnClosing :: MigrationCheck -> Bool
isDestTxnClosing (MigrationCheck _ destCheck) = destSectionEndsTransaction destCheck

spec :: Spec
spec = do
    let
        mkDbInfo baseDbInfo migs = baseDbInfo {
            sqlMigrations = Right migs
            }
    describe "DbDependentSpecs" $ do
        describe "Analysis tests" $ do
            context "No False positives for destructiveness checks" $ do
                aroundFreshDatabase $
                    it "Create Table is non-destructive" $
                        \emptyTestDbInfo ->
                            isMigrationDestructive <$> (runStdoutLoggingT $ checkMigration emptyTestDbInfo (addedSqlMig createTableMig)) `shouldReturn` False

                aroundDatabaseWithMigs [ createTableMig ] $
                    it "Adding columns is non-destructive" $
                        \dbInfo ->
                            isMigrationDestructive <$> (runStdoutLoggingT $ checkMigration dbInfo (addedSqlMig addColumnMig)) `shouldReturn` False

            context "Obviously destructive actions detected as such" $ do
                aroundFreshDatabase $
                    it "Dropping columns is destructive" $
                        \emptyTestDbInfo ->
                            isMigrationDestructive <$> (runStdoutLoggingT $ checkMigration (mkDbInfo emptyTestDbInfo [ createTableMig, addColumnMig ]) (addedSqlMig dropColumnMig)) `shouldReturn` True

                aroundFreshDatabase $
                    it "Dropping tables is destructive" $
                        \emptyTestDbInfo -> do
                            isMigrationDestructive <$> (runStdoutLoggingT $ checkMigration (mkDbInfo emptyTestDbInfo [ createTableMig, addColumnMig ]) (addedSqlMig dropTableMig)) `shouldReturn` True
                            isMigrationDestructive <$> (runStdoutLoggingT $ checkMigration (mkDbInfo emptyTestDbInfo [ createTableMig, addColumnMig, dropColumnMig ]) (addedSqlMig dropTableMig)) `shouldReturn` True

            context "Transaction ending migrations" $ do
                aroundFreshDatabase $
                    it "Non-destructive section containing COMMIT detected correctly" $
                        \emptyTestDbInfo ->
                            let
                                commitTxnMig = SqlMigration {
                                    migrationName = "0000-commit.sql"
                                    , nonDestructiveSql = Just "COMMIT;"
                                    , nonDestructiveForce = False
                                    , nonDestructiveInTxn = True
                                    , destructiveSql = Nothing
                                    , destructiveInTxn = True
                                }
                            in
                            isNonDestTxnClosing <$> (runStdoutLoggingT $ checkMigration emptyTestDbInfo commitTxnMig) `shouldReturn` True
                aroundFreshDatabase $
                    it "Non-destructive section containing ROLLBACK detected correctly" $
                        \emptyTestDbInfo ->
                            let
                                rollbackTxnMig = SqlMigration {
                                    migrationName = "0000-rollback.sql"
                                    , nonDestructiveSql = Just "ROLLBACK;"
                                    , nonDestructiveForce = False
                                    , nonDestructiveInTxn = True
                                    , destructiveSql = Nothing
                                    , destructiveInTxn = True
                                }
                            in
                            isNonDestTxnClosing <$> (runStdoutLoggingT $ checkMigration emptyTestDbInfo rollbackTxnMig) `shouldReturn` True
                aroundFreshDatabase $
                    it "Destructive section that ends transactions when non-destructive section also ends Transactions detected correctly" $
                        \emptyTestDbInfo ->
                            let
                                rollbackTxnMig = SqlMigration {
                                    migrationName = "0000-rollback.sql"
                                    , nonDestructiveSql = Just "ROLLBACK;"
                                    , nonDestructiveForce = False
                                    , nonDestructiveInTxn = True
                                    , destructiveSql = Just "ROLLBACK;"
                                    , destructiveInTxn = True
                                }
                            in
                            isDestTxnClosing <$> (runStdoutLoggingT $ checkMigration emptyTestDbInfo rollbackTxnMig) `shouldReturn` True
                aroundFreshDatabase $
                    it "Destructive section that does not end transactions when non-destructive section also ends Transactions detected correctly" $
                        \emptyTestDbInfo ->
                            let
                                rollbackTxnMig = SqlMigration {
                                    migrationName = "0000-rollback.sql"
                                    , nonDestructiveSql = Just "ROLLBACK;"
                                    , nonDestructiveForce = False
                                    , nonDestructiveInTxn = True
                                    , destructiveSql = Nothing
                                    , destructiveInTxn = True
                                }
                            in
                            isDestTxnClosing <$> (runStdoutLoggingT $ checkMigration emptyTestDbInfo rollbackTxnMig) `shouldReturn` False