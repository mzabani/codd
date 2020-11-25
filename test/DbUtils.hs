module DbUtils where

import Codd (withDbAndDrop)
import Codd.Internal (connectAndDispose)
import Codd.Types (CoddSettings(..), AddedSqlMigration(..), SqlMigration(..), DeploymentWorkflow(..), Include(..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime, NominalDiffTime)
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Time as DB
import Test.Hspec
import UnliftIO (MonadIO(..), finally)
import UnliftIO.Environment (getEnv)

testConnInfo :: MonadIO m => m ConnectInfo
testConnInfo = getEnv "PGPORT" >>= \portStr -> return defaultConnectInfo { connectHost = "localhost", connectUser = "postgres", connectDatabase = "postgres", connectPort = read portStr }

aroundConnInfo :: SpecWith ConnectInfo -> Spec
aroundConnInfo = around $ \act -> do
    cinfo <- testConnInfo
    act cinfo

testCoddSettings :: MonadIO m => [AddedSqlMigration] -> m CoddSettings
testCoddSettings migs = do
    connInfo <- testConnInfo
    -- In all our tests, we simulate a scenario where one App User already exists
    let
        migTimestamp = getIncreasingTimestamp (-1000)
        createTestUserMig = AddedSqlMigration SqlMigration {
            migrationName = show migTimestamp <> "-create-test-user.sql"
            , nonDestructiveSql = Just $ "DO\n"
<> "$do$\n"
<> "BEGIN\n"
<> "   IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-test-user') THEN\n"
<> "      CREATE USER \"codd-test-user\";\n"
<> "   END IF;\n"
<> "END\n"
<> "$do$; GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\";"
            , nonDestructiveForce = True
            , nonDestructiveInTxn = True
            , destructiveSql = Nothing
            , destructiveInTxn = True
        } migTimestamp
    pure CoddSettings {
            superUserConnString = connInfo
            , dbName = "codd-test-db"
            , sqlMigrations = Right (createTestUserMig : migs)
            , onDiskHashes = Left ""
            , deploymentWorkflow = SimpleDeployment
            , schemasToHash = Exclude [] -- Hash every possible internal PG schema too to make our tests tougher ;)
            , extraRolesToHash = Include [ "codd-test-user", "extra-codd-test-user" ] -- Important for HashingSpec
        }

-- | Doesn't create a Database, doesn't create anything. Just supplies the Test CoddSettings from Env Vars to your test.
aroundTestDbInfo :: SpecWith CoddSettings -> Spec
aroundTestDbInfo = around $ \act -> do
    coddSettings <- testCoddSettings []
    act coddSettings

aroundFreshDatabase :: SpecWith CoddSettings -> Spec
aroundFreshDatabase = aroundDatabaseWithMigs []

aroundDatabaseWithMigs :: [AddedSqlMigration] -> SpecWith CoddSettings -> Spec
aroundDatabaseWithMigs startingMigs = around $ \act -> do
    coddSettings <- testCoddSettings startingMigs
    withDbAndDrop coddSettings $ \_ ->
        act coddSettings 
            `finally`
                connectAndDispose
                    (superUserConnString coddSettings)
                    -- Some things aren't associated to a Schema and not even to a Database; they belong under the entire DB instance. So we reset these things here, always.
                    (\conn -> liftIO $ DB.execute conn "ALTER ROLE postgres RESET ALL; ALTER ROLE \"codd-test-user\" RESET ALL; DROP ROLE IF EXISTS \"extra-codd-test-user\";" ())

-- | Returns a Postgres UTC Timestamp that increases with its input parameter.
getIncreasingTimestamp :: NominalDiffTime -> DB.UTCTimestamp
getIncreasingTimestamp n = DB.Finite $ addUTCTime n $ UTCTime (fromGregorian 2020 1 1) 0