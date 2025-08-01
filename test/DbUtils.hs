module DbUtils where

import Codd (applyMigrationsNoCheck)
import Codd.Environment (CoddSettings (..))
import Codd.Internal
  ( dbIdentifier,
    withConnection,
  )
import Codd.Logging
  ( CoddLogger,
    runCoddLogger,
  )
import Codd.Parsing
  ( AddedSqlMigration (..),
    EnvVars,
    ParsedSql (..),
    PureStream,
    SqlMigration (..),
    parseSqlMigration,
    parseSqlPiecesStreaming,
  )
import Codd.Query
  ( NotInTxn,
    execvoid_,
    query,
    unsafeQuery1,
  )
import Codd.Types (ConnectionString (..), SchemaAlgo (..), SchemaSelection (..), TxnIsolationLvl (..), singleTryPolicy)
import Control.Monad
  ( forM_,
    void,
    when,
  )
import Control.Monad.Trans.Resource (MonadThrow)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
  ( DiffTime,
    UTCTime (..),
    addUTCTime,
    secondsToDiffTime,
  )
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Time as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import qualified Streaming.Prelude as Streaming
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO
  ( MonadIO (..),
    MonadUnliftIO,
    bracket,
    finally,
    liftIO,
  )
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import UnliftIO.Environment (getEnv)

testConnInfo :: (MonadIO m) => m ConnectionString
testConnInfo =
  getEnv "PGPORT" >>= \portStr ->
    return
      ConnectionString
        { -- It is strange, but IPv6 support in Github Actions seems not to be there yet.
          -- https://github.com/actions/virtual-environments/issues/668
          hostname = "/tmp",
          user = "postgres",
          database = "codd-test-db",
          password = "",
          port = read portStr,
          options = Nothing
        }

-- | A default connection timeout of 5 seconds.
--   It serves tests well.
testConnTimeout :: DiffTime
testConnTimeout = secondsToDiffTime 5

aroundConnInfo :: SpecWith ConnectionString -> Spec
aroundConnInfo = around $ \act -> do
  cinfo <- testConnInfo
  act cinfo

mkValidSql :: (MonadThrow m) => Text -> ParsedSql m
mkValidSql = WellParsedSql . parseSqlPiecesStreaming . Streaming.yield

-- | Brings a Database up to date just like `applyMigrations`, executes the supplied action passing it a Connection String for the Super User and DROPs the Database
-- afterwards.
withCoddDbAndDrop ::
  (MonadUnliftIO m, CoddLogger m, MonadThrow m, EnvVars m, NotInTxn m) =>
  [AddedSqlMigration m] ->
  (ConnectionString -> m a) ->
  m a
withCoddDbAndDrop migs f = do
  coddSettings@CoddSettings {migsConnString} <- testCoddSettings
  bracket
    ( do
        bootstrapMig <- createTestUserMig
        applyMigrationsNoCheck
          coddSettings
          (Just $ bootstrapMig : migs)
          testConnTimeout
          (const $ pure ())
    )
    (dropDb migsConnString)
    (const $ f migsConnString)
  where
    dropDb migsConnString _ = do
      withConnection
        migsConnString
          { database = "postgres",
            user = "postgres"
          }
        testConnTimeout
        $ \conn ->
          void $
            liftIO $
              DB.execute_ conn $
                "DROP DATABASE IF EXISTS "
                  <> dbIdentifier
                    (Text.pack $ database migsConnString)
                  <> "(FORCE)"

-- | Runs an action and drops a database afterwards in a finally block.
finallyDrop :: (MonadIO m, MonadUnliftIO m) => Text -> m a -> m a
finallyDrop dbName f = f `finally` dropDb
  where
    dropDb = do
      connInfo <- testConnInfo
      withConnection
        connInfo
          { database = "postgres",
            user = "postgres"
          }
        testConnTimeout
        $ \conn ->
          void $
            liftIO $
              DB.execute_ conn $
                "DROP DATABASE IF EXISTS "
                  <> dbIdentifier dbName
                  <> "(FORCE)"

createTestUserMig ::
  forall m. (MonadIO m, MonadThrow m) => m (AddedSqlMigration m)
createTestUserMig = createTestUserMigPol @m

-- | A version of "createTestUser" that is more polymorphic in the Monad of the
-- returned migration.
createTestUserMigPol ::
  forall n m. (MonadThrow n, MonadIO m) => m (AddedSqlMigration n)
createTestUserMigPol = do
  let migTimestamp = getIncreasingTimestamp (-1000)
  cinfo <- testConnInfo

  let psql =
        -- IMPORTANT: If you change this migration, also change the 2000-01-01-00-00-00-bootstrap-but-fail.sql file and the test that uses it
        mkValidSql $
          "DO\n"
            <> "$do$\n"
            <> "BEGIN\n"
            <> "   IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-test-user') THEN\n"
            <> "      CREATE USER \"codd-test-user\";\n"
            <> "   END IF;\n"
            <> "END\n"
            <> "$do$;\n"
            <> "CREATE DATABASE \"codd-test-db\" WITH OWNER=\"codd-test-user\";\n"
            <> "GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\";"
  pure $
    AddedSqlMigration
      SqlMigration
        { migrationName = "bootstrap-test-db-and-user.sql",
          migrationSql = psql,
          migrationInTxn = False,
          migrationRequiresCoddSchema = False,
          -- A custom connection string is necessary because the DB doesn't yet exist
          migrationCustomConnInfo =
            Just
              cinfo
                { user = "postgres",
                  database = "postgres"
                },
          migrationEnvVars = mempty
        }
      migTimestamp

testCoddSettings :: (MonadIO m) => m CoddSettings
testCoddSettings = do
  connInfo <- testConnInfo
  pure
    CoddSettings
      { migsConnString = connInfo,
        sqlMigrations = [],
        onDiskReps = Left "",
        namespacesToCheck = AllNonInternalSchemas,
        extraRolesToCheck = ["codd-test-user", "extra-codd-test-user"], -- Important for SchemaVerificationSpec
        retryPolicy = singleTryPolicy,
        txnIsolationLvl = DbDefault,
        schemaAlgoOpts = SchemaAlgo False False False
      }

-- | Doesn't create a Database, doesn't create anything. Just supplies the Test CoddSettings from Env Vars to your test.
-- This does cleanup if you create any databases or tables in the `postgres` DB, though.
aroundNoDatabases :: SpecWith CoddSettings -> Spec
aroundNoDatabases = around $ \act -> do
  coddSettings <- testCoddSettings
  act coddSettings `finally` cleanupAfterTest

-- | Applies a migration that creates the "codd-test-db" database and the
-- "codd-test-user" user for the test.
aroundCoddTestDb :: SpecWith CoddSettings -> Spec
aroundCoddTestDb = aroundCoddTestDbAnd []

-- | Applies a migration that creates the "codd-test-db" database and the
-- "codd-test-user" user, and also applies any other migrations supplied
-- by the caller.
aroundCoddTestDbAnd ::
  (forall m. (MonadThrow m) => [AddedSqlMigration m]) ->
  SpecWith CoddSettings ->
  Spec
aroundCoddTestDbAnd startingMigs = around $ \act -> do
  coddSettings <- testCoddSettings

  runCoddLogger
    ( do
        bootstrapMig <- createTestUserMig
        applyMigrationsNoCheck
          coddSettings
          (Just $ bootstrapMig : startingMigs)
          testConnTimeout
          (const $ pure ())
        liftIO (act coddSettings)
    )
    `finally` cleanupAfterTest

aroundCoddTestDbAndAndPgCron ::
  (forall m. (MonadThrow m) => [AddedSqlMigration m]) ->
  SpecWith CoddSettings ->
  Spec
aroundCoddTestDbAndAndPgCron startingMigs = around $ \act -> do
  coddSettings <- testCoddSettings
  cinfo <- testConnInfo
  let superUserConnInfo = cinfo {user = "postgres"}
      createPgCronExtMig =
        AddedSqlMigration
          SqlMigration
            { migrationName = "0000-create-ext-pg_cron.sql",
              migrationSql =
                mkValidSql
                  "CREATE EXTENSION pg_cron",
              migrationInTxn = True,
              migrationRequiresCoddSchema = False,
              migrationCustomConnInfo = Just superUserConnInfo,
              migrationEnvVars = mempty
            }
          (getIncreasingTimestamp 0)
  runCoddLogger
    ( do
        bootstrapMig <- createTestUserMig
        applyMigrationsNoCheck
          coddSettings
          (Just $ bootstrapMig : createPgCronExtMig : startingMigs)
          testConnTimeout
          (const $ pure ())
        liftIO (act coddSettings)
    )
    `finally` cleanupAfterTest

cleanupAfterTest :: IO ()
cleanupAfterTest = do
  CoddSettings {migsConnString} <- testCoddSettings
  withConnection
    migsConnString
      { user = "postgres",
        database = "postgres"
      }
    testConnTimeout
    -- Some things aren't associated to a Schema and not even to a Database; they belong under the entire DB/postgres instance.
    -- So we reset these things here, with the goal of getting the DB in the same state as it would be before even "createUserTestMig"
    -- from "testCoddSettings" runs, so that each test is guaranteed the same starting DB environment.
    ( \conn -> do
        execvoid_ conn "ALTER ROLE postgres RESET ALL; SET client_min_messages='warning'; DROP EXTENSION IF EXISTS pg_cron; RESET client_min_messages;"
        dbs :: [String] <-
          map DB.fromOnly
            <$> query
              conn
              "SELECT datname FROM pg_catalog.pg_database WHERE datname NOT IN ('postgres', 'template0', 'template1')"
              ()
        forM_ dbs $ \db ->
          execvoid_ conn $ "DROP DATABASE \"" <> fromString db <> "\" (FORCE)"

        allRoles :: [String] <-
          map DB.fromOnly
            <$> query
              conn
              "SELECT rolname FROM pg_roles WHERE rolname NOT IN ('postgres') AND rolname NOT LIKE 'pg_%' ORDER BY rolname DESC"
              ()
        forM_ allRoles $ \role -> do
          let escapedRole = fromString ("\"" <> role <> "\"")
          execvoid_ conn $
            "DROP OWNED BY "
              <> escapedRole
              -- <> "; REVOKE ALL ON ALL TABLES IN SCHEMA public FROM "
              -- <> escapedRole
              -- <> "; REVOKE ALL ON ALL SEQUENCES IN SCHEMA public FROM "
              -- <> escapedRole
              -- <> "; REVOKE ALL ON ALL FUNCTIONS IN SCHEMA public FROM "
              -- <> escapedRole
              -- <> "; REVOKE ALL ON SCHEMA public FROM "
              -- <> escapedRole
              -- <> "; REVOKE ALL ON DATABASE \"codd-test-db\" FROM "
              -- <> escapedRole
              <> "; DROP ROLE "
              <> escapedRole

        createdTables :: [(String, String)] <-
          query
            conn
            "SELECT schemaname, tablename FROM pg_catalog.pg_tables WHERE schemaname NOT IN ('pg_catalog', 'information_schema')"
            ()
        forM_ createdTables $ \(schema, tbl) ->
          execvoid_ conn $
            "DROP TABLE \""
              <> fromString schema
              <> "\".\""
              <> fromString tbl
              <> "\" CASCADE"
    )

-- | Returns a Postgres UTC Timestamp that increases with its input parameter.
getIncreasingTimestamp :: DiffTime -> DB.UTCTimestamp
getIncreasingTimestamp n =
  DB.Finite $ addUTCTime (realToFrac n) $ UTCTime (fromGregorian 2020 1 1) 0

-- | Changes every added migrations's timestamp so they're applied in the order of the list.
fixMigsOrder :: [AddedSqlMigration m] -> [AddedSqlMigration m]
fixMigsOrder =
  zipWith
    ( \i (AddedSqlMigration mig _) ->
        AddedSqlMigration mig $
          getIncreasingTimestamp $
            secondsToDiffTime $
              fromIntegral i
    )
    [(0 :: Int) ..]

shouldBeStrictlySortedOn :: (Show a, Ord b) => [a] -> (a -> b) -> Expectation
shouldBeStrictlySortedOn xs f =
  zip xs (drop 1 xs) `shouldSatisfy` all (\(a1, a2) -> f a1 < f a2)

-- | Specialized version of `parseSqlMigration` that helps type inference.
parseSqlMigrationIO ::
  String -> PureStream IO -> IO (Either String (SqlMigration IO))
parseSqlMigrationIO = parseSqlMigration

getEmptyTempDir :: (MonadIO m) => m FilePath
getEmptyTempDir = do
  tmp <- getTemporaryDirectory
  complement :: UUID <- liftIO UUIDv4.nextRandom
  let emptyDir = tmp </> UUID.toString complement
  createDirectoryIfMissing True emptyDir
  pure emptyDir
