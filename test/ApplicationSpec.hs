module ApplicationSpec where

import Codd (withDbAndDrop)
import Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration)
import Codd.Types (DbVcsInfo(..), SqlMigration(..), ApplyMigrations(..))
import Control.Monad (when)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Data.Text (unpack)
import Test.Hspec

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
    let
        superUserConnString = DB.defaultConnectInfo { connectHost = "localhost", connectUser = "postgres", connectDatabase = "postgres", connectPort = 5432 }
        emptyTestDbInfo = DbVcsInfo {
            superUserConnString = superUserConnString
            , dbName = "codd-test-db"
            , appUser = "postgres"
            , sqlMigrations = Right []
        }
        mkDbInfo migs = emptyTestDbInfo {
            sqlMigrations = Right migs
        }
    describe "Application tests" $ do
        it "SQL containing characters typical to placeholders is not a problem" $ do
            withDbAndDrop (emptyTestDbInfo { sqlMigrations = Right [ placeHoldersMig ] }) OnlyNonDestructive (const $ return True)
                `shouldReturn` True