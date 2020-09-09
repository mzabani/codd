module AnalysisSpec where

import Codd (withDbAndDrop)
import Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration)
import Codd.Types (DbVcsInfo(..), SqlMigration(..), ApplyMigrations(..))
import Control.Monad (when)
import DbUtils (testConnInfo)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Data.Text (unpack)
import Test.Hspec

createTableMig, addColumnMig, dropColumnMig, dropTableMig :: SqlMigration
createTableMig = SqlMigration {
                    migrationName = "0001-create-table.sql"
                    , nonDestructiveSql = Just "CREATE TABLE anytable ();"
                    , nonDestructiveForce = False
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                }
addColumnMig = SqlMigration {
                    migrationName = "0002-add-column.sql"
                    , nonDestructiveSql = Just "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;"
                    , nonDestructiveForce = False
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                }
dropColumnMig = SqlMigration {
                    migrationName = "0003-drop-column.sql"
                    , nonDestructiveSql = Just "ALTER TABLE anytable DROP COLUMN anycolumn;"
                    , nonDestructiveForce = True
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                }
dropTableMig = SqlMigration {
                    migrationName = "0004-drop-table.sql"
                    , nonDestructiveSql = Just "DROP TABLE anytable;"
                    , nonDestructiveForce = True
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                }

isMigrationDestructive :: MigrationCheck -> Bool
isMigrationDestructive (MigrationCheck nonDestCheck _) = nonDestSectionIsDestructive nonDestCheck

isNonDestTxnClosing :: MigrationCheck -> Bool
isNonDestTxnClosing (MigrationCheck nonDestCheck _) = nonDestSectionEndsTransaction nonDestCheck

isDestTxnClosing :: MigrationCheck -> Bool
isDestTxnClosing (MigrationCheck _ destCheck) = destSectionEndsTransaction destCheck

spec :: Spec
spec = do
    let
        superUserConnString = testConnInfo
        emptyTestDbInfo = DbVcsInfo {
            superUserConnString = superUserConnString
            , dbName = "codd-test-db"
            , appUser = "postgres"
            , sqlMigrations = Right []
        }
        mkDbInfo migs = emptyTestDbInfo {
            sqlMigrations = Right migs
        }
    describe "Analysis tests" $ do
        context "No False positives for destructiveness checks" $ do
            it "Create Table is non-destructive" $ do
                withDbAndDrop emptyTestDbInfo OnlyNonDestructive $ \_ -> isMigrationDestructive <$> (checkMigration emptyTestDbInfo createTableMig) `shouldReturn` False

            it "Adding columns is non-destructive" $ do
                withDbAndDrop emptyTestDbInfo OnlyNonDestructive $ \_ -> do
                    isMigrationDestructive <$> (checkMigration (mkDbInfo [ createTableMig ]) addColumnMig) `shouldReturn` False

        context "Obviously destructive actions detected as such" $ do
            it "Dropping columns is destructive" $ do
                withDbAndDrop emptyTestDbInfo OnlyNonDestructive $ \_ -> do
                    isMigrationDestructive <$> (checkMigration (mkDbInfo [ createTableMig, addColumnMig ]) dropColumnMig) `shouldReturn` True

            it "Dropping tables is destructive" $ do
                withDbAndDrop emptyTestDbInfo OnlyNonDestructive $ \_ -> do
                    isMigrationDestructive <$> (checkMigration (mkDbInfo [ createTableMig, addColumnMig ]) dropTableMig) `shouldReturn` True
                    isMigrationDestructive <$> (checkMigration (mkDbInfo [ createTableMig, addColumnMig, dropColumnMig ]) dropTableMig) `shouldReturn` True

        context "Transaction ending migrations" $ do
            it "Non-destructive section containng COMMIT detected correctly" $ do
                withDbAndDrop emptyTestDbInfo OnlyNonDestructive $ \_ ->
                    let
                        commitTxnMig = SqlMigration {
                            migrationName = "0000-commit.sql"
                            , nonDestructiveSql = Just "COMMIT;"
                            , nonDestructiveForce = False
                            , nonDestructiveInTxn = True
                            , destructiveSql = Nothing
                            , destructiveInTxn = True
                        }
                        rollbackTxnMig = SqlMigration {
                            migrationName = "0000-rollback.sql"
                            , nonDestructiveSql = Just "ROLLBACK;"
                            , nonDestructiveForce = False
                            , nonDestructiveInTxn = True
                            , destructiveSql = Nothing
                            , destructiveInTxn = True
                        }
                    in
                    isNonDestTxnClosing <$> (checkMigration emptyTestDbInfo commitTxnMig) `shouldReturn` True
            it "Non-destructive section containng ROLLBACK detected correctly" $ do
                withDbAndDrop emptyTestDbInfo OnlyNonDestructive $ \_ ->
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
                    isNonDestTxnClosing <$> (checkMigration emptyTestDbInfo rollbackTxnMig) `shouldReturn` True
            it "Destructive section that ends transactions when non-destructive section also ends Transactions detected correctly" $ do
                withDbAndDrop emptyTestDbInfo OnlyNonDestructive $ \_ ->
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
                    isDestTxnClosing <$> (checkMigration emptyTestDbInfo rollbackTxnMig) `shouldReturn` True
            it "Destructive section that does not end transactions when non-destructive section also ends Transactions detected correctly" $ do
                withDbAndDrop emptyTestDbInfo OnlyNonDestructive $ \_ ->
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
                    isDestTxnClosing <$> (checkMigration emptyTestDbInfo rollbackTxnMig) `shouldReturn` False