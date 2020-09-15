module DbUtils where

import Codd (withDbAndDrop)
import Codd.Types (DbVcsInfo(..), SqlMigration(..), ApplyMigrations(..))
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import Test.Hspec
import UnliftIO (MonadIO)
import UnliftIO.Environment (getEnv)

testConnInfo :: MonadIO m => m ConnectInfo
testConnInfo = getEnv "PGPORT" >>= \portStr -> return defaultConnectInfo { connectHost = "localhost", connectUser = "postgres", connectDatabase = "postgres", connectPort = read portStr }

aroundConnInfo :: SpecWith ConnectInfo -> Spec
aroundConnInfo = around $ \act -> do
    cinfo <- testConnInfo
    act cinfo

aroundFreshDatabase :: SpecWith DbVcsInfo -> Spec
aroundFreshDatabase = aroundDatabaseWithMigs []

aroundDatabaseWithMigs :: [SqlMigration] -> SpecWith DbVcsInfo -> Spec
aroundDatabaseWithMigs startingMigs = around $ \act -> do
    connInfo <- testConnInfo
    let
        emptyTestDbInfo = DbVcsInfo {
            superUserConnString = connInfo
            , dbName = "codd-test-db"
            , appUser = "postgres"
            , sqlMigrations = Right startingMigs
            , onDiskHashes = Left ""
        }
    withDbAndDrop (emptyTestDbInfo { sqlMigrations = Right [ ] }) OnlyNonDestructive (\_ -> act emptyTestDbInfo)