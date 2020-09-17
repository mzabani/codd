module DbUtils where

import Codd (withDbAndDrop)
import Codd.Types (DbVcsInfo(..), AddedSqlMigration(..), DeploymentWorkflow(..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime, NominalDiffTime)
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import qualified Database.PostgreSQL.Simple.Time as DB
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

aroundDatabaseWithMigs :: [AddedSqlMigration] -> SpecWith DbVcsInfo -> Spec
aroundDatabaseWithMigs startingMigs = around $ \act -> do
    connInfo <- testConnInfo
    let
        dbInfo = DbVcsInfo {
            superUserConnString = connInfo
            , dbName = "codd-test-db"
            , appUser = "postgres"
            , sqlMigrations = Right startingMigs
            , onDiskHashes = Left ""
            , deploymentWorkflow = SimpleDeployment
        }
    withDbAndDrop dbInfo (\_ -> act dbInfo)

-- | Returns a Postgres UTC Timestamp that increases with its input parameter.
getIncreasingTimestamp :: NominalDiffTime -> DB.UTCTimestamp
getIncreasingTimestamp n = DB.Finite $ addUTCTime n $ UTCTime (fromGregorian 2020 1 1) 0