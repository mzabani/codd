module DbDependentSpecs.ApplicationSpec where

import Codd (withDbAndDrop, applyMigrations)
import Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration)
import Codd.Types (DbVcsInfo(..), SqlMigration(..), ApplyMigrations(..))
import Control.Monad (when, void)
import DbUtils (aroundFreshDatabase)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Data.Text (unpack)
import Test.Hspec
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
            aroundFreshDatabase $
                it "SQL containing characters typical to placeholders does not throw" $
                    \emptyTestDbInfo -> do
                        void @IO $ applyMigrations (emptyTestDbInfo { sqlMigrations = Right [ placeHoldersMig ] }) OnlyNonDestructive