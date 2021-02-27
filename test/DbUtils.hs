module DbUtils where

import           Codd                           ( withDbAndDrop )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( withConnection )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , ParsedSql(..)
                                                , SqlMigration(..)
                                                , parseSqlPieces
                                                )
import           Codd.Types                     ( DeploymentWorkflow(..)
                                                , Include(..)
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Data.Text                      ( Text )
import           Data.Time.Calendar             ( fromGregorian )
import           Data.Time.Clock                ( NominalDiffTime
                                                , UTCTime(..)
                                                , addUTCTime
                                                )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..)
                                                , defaultConnectInfo
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import           Test.Hspec
import           UnliftIO                       ( MonadIO(..)
                                                , finally
                                                , liftIO
                                                )
import           UnliftIO.Environment           ( getEnv )

testConnInfo :: MonadIO m => m ConnectInfo
testConnInfo = getEnv "PGPORT" >>= \portStr -> return defaultConnectInfo
    { connectHost     = "localhost"
    , connectUser     = "postgres"
    , connectDatabase = "postgres"
    , connectPort     = read portStr
    }

aroundConnInfo :: SpecWith ConnectInfo -> Spec
aroundConnInfo = around $ \act -> do
    cinfo <- testConnInfo
    act cinfo

mkValidSql :: Text -> ParsedSql
mkValidSql t = case parseSqlPieces t of
    Left  e   -> error e -- Probably best to fail early if sql is invalid inside test code
    Right pcs -> WellParsedSql t pcs

testCoddSettings :: MonadIO m => [AddedSqlMigration] -> m CoddSettings
testCoddSettings migs = do
    connInfo <- testConnInfo
    -- In all our tests, we simulate a scenario where one App User already exists
    let migTimestamp      = getIncreasingTimestamp (-1000)
        createTestUserMig = AddedSqlMigration
            SqlMigration
                { migrationName = show migTimestamp <> "-create-test-user.sql"
                , nonDestructiveSql   =
                    Just
                    $  mkValidSql
                    $  "DO\n"
                    <> "$do$\n"
                    <> "BEGIN\n"
                    <> "   IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-test-user') THEN\n"
                    <> "      CREATE USER \"codd-test-user\";\n"
                    <> "   END IF;\n"
                    <> "END\n"
                    <> "$do$; GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\";"
                , nonDestructiveForce = True
                , nonDestructiveInTxn = True
                , destructiveSql      = Nothing
                , destructiveInTxn    = True
                }
            migTimestamp
    pure CoddSettings
        { superUserConnString = connInfo
        , dbName              = "codd-test-db"
        , sqlMigrations       = Right (createTestUserMig : migs)
        , onDiskHashes        = Left ""
        , deploymentWorkflow  = SimpleDeployment
        , schemasToHash       = Include ["public", "codd-extra-mapped-schema"]
        , extraRolesToHash = Include ["codd-test-user", "extra-codd-test-user"] -- Important for HashingSpec
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
    runStdoutLoggingT $ withDbAndDrop coddSettings $ \_ ->
        liftIO (act coddSettings) `finally` withConnection
            (superUserConnString coddSettings)
                    -- Some things aren't associated to a Schema and not even to a Database; they belong under the entire DB instance. So we reset these things here, always.
            (\conn -> liftIO $ DB.execute
                conn
                "ALTER ROLE postgres RESET ALL; ALTER ROLE \"codd-test-user\" RESET ALL; DROP ROLE IF EXISTS \"extra-codd-test-user\";"
                ()
            )

-- | Returns a Postgres UTC Timestamp that increases with its input parameter.
getIncreasingTimestamp :: NominalDiffTime -> DB.UTCTimestamp
getIncreasingTimestamp n =
    DB.Finite $ addUTCTime n $ UTCTime (fromGregorian 2020 1 1) 0
