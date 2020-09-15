module DbDependentSpecs.ApplicationSpec where

import Codd (withDbAndDrop, applyMigrations)
import Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration)
import Codd.Types (DbVcsInfo(..), SqlMigration(..), ApplyMigrations(..))
import Codd.Hashing.Types (DbHashes(..))
import Control.Monad (when, void)
import qualified Data.Map.Strict as Map
import DbUtils (aroundFreshDatabase)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Data.Text (unpack)
import Test.Hspec
import Test.Hspec.Expectations
import Test.QuickCheck

placeHoldersMig :: SqlMigration
placeHoldersMig = SqlMigration {
                    migrationName = "0000-placeholders.sql"
                    , nonDestructiveSql = Just "CREATE TABLE any_table();\n-- ? $1 $2 ? ? ?"
                    , nonDestructiveForce = False
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                }

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Application tests" $ do
            aroundFreshDatabase $ do
                it "SQL containing characters typical to placeholders does not throw" $
                    \emptyTestDbInfo -> do
                        void @IO $ applyMigrations (emptyTestDbInfo { sqlMigrations = Right [ placeHoldersMig ] }) OnlyNonDestructive False

                it "Bogus on-disk hashes makes applying migrations fail" $
                    \emptyTestDbInfo -> do
                        let
                            bogusDbHashes = DbHashes Map.empty
                        void @IO $ do
                            applyMigrations (emptyTestDbInfo { sqlMigrations = Right [ placeHoldersMig ], onDiskHashes = Right bogusDbHashes }) OnlyNonDestructive True
                                `shouldThrow`
                                    anyIOException