module DbUtils where

import           Codd                           ( applyMigrations
                                                , applyMigrationsNoCheck
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( dbIdentifier
                                                , withConnection
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , ParsedSql(..)
                                                , SqlMigration(..)
                                                , parseSqlPieces
                                                )
import           Codd.Query                     ( execvoid_
                                                , query
                                                )
import           Codd.Types                     ( ChecksumAlgo(..)
                                                , Include(..)
                                                , TxnIsolationLvl(..)
                                                , singleTryPolicy
                                                )
import           Control.Monad                  ( forM_
                                                , void
                                                )
import           Control.Monad.Logger           ( MonadLogger
                                                , runStdoutLoggingT
                                                )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time.Calendar             ( fromGregorian )
import           Data.Time.Clock                ( DiffTime
                                                , UTCTime(..)
                                                , addUTCTime
                                                , secondsToDiffTime
                                                )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..)
                                                , defaultConnectInfo
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import           Test.Hspec
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
                                                , bracket
                                                , finally
                                                , liftIO
                                                )
import           UnliftIO.Environment           ( getEnv )

testConnInfo :: MonadIO m => m ConnectInfo
testConnInfo = getEnv "PGPORT" >>= \portStr -> return defaultConnectInfo
    {
        -- It is strange, but IPv6 support in Github Actions seems not to be there yet.
        -- https://github.com/actions/virtual-environments/issues/668
      connectHost     = "127.0.0.1"
    , connectUser     = "postgres"
    , connectDatabase = "codd-test-db"
    , connectPort     = read portStr
    }

-- | A default connection timeout of 5 seconds.
--   It serves tests well.
testConnTimeout :: DiffTime
testConnTimeout = secondsToDiffTime 5

aroundConnInfo :: SpecWith ConnectInfo -> Spec
aroundConnInfo = around $ \act -> do
    cinfo <- testConnInfo
    act cinfo

mkValidSql :: Text -> ParsedSql
mkValidSql t = case parseSqlPieces t of
    Left  e   -> error e -- Probably best to fail early if sql is invalid inside test code
    Right pcs -> WellParsedSql t pcs

-- | Brings a Database up to date just like `applyMigrations`, executes the supplied action passing it a Connection String for the Super User and DROPs the Database
-- afterwards.
withDbAndDrop
    :: (MonadUnliftIO m, MonadLogger m)
    => CoddSettings
    -> (ConnectInfo -> m a)
    -> m a
withDbAndDrop dbInfo@CoddSettings { migsConnString } f = bracket
    (applyMigrationsNoCheck dbInfo testConnTimeout (const $ pure ()))
    dropDb
    (const $ f migsConnString)
  where
    dropDb _ = do
        withConnection
                migsConnString { connectDatabase = "postgres"
                               , connectUser     = "postgres"
                               }
                testConnTimeout
            $ \conn ->
                  void
                      $  liftIO
                      $  DB.execute_ conn
                      $  "DROP DATABASE IF EXISTS "
                      <> dbIdentifier
                             (Text.pack $ connectDatabase migsConnString)


-- | Runs an action and drops a database afterwards in a finally block.
finallyDrop :: (MonadIO m, MonadUnliftIO m) => Text -> m a -> m a
finallyDrop dbName f = f `finally` dropDb
  where
    dropDb = do
        connInfo <- testConnInfo
        withConnection
                connInfo { connectDatabase = "postgres"
                         , connectUser     = "postgres"
                         }
                testConnTimeout
            $ \conn ->
                  void
                      $  liftIO
                      $  DB.execute_ conn
                      $  "DROP DATABASE IF EXISTS "
                      <> dbIdentifier dbName

testCoddSettings :: MonadIO m => [AddedSqlMigration] -> m CoddSettings
testCoddSettings migs = do
    connInfo <- testConnInfo
    -- In all our tests, we simulate a scenario where one App User already exists
    let migTimestamp      = getIncreasingTimestamp (-1000)
        createTestUserMig = AddedSqlMigration
            SqlMigration
                { migrationName           = "bootstrap-test-db-and-user.sql"
                , migrationSql            =
                    Just
                    $  mkValidSql
                    $  "DO\n"
                    <> "$do$\n"
                    <> "BEGIN\n"
                    <> "   IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-test-user') THEN\n"
                    <> "      CREATE USER \"codd-test-user\";\n"
                    <> "   END IF;\n"
                    <> "END\n"
                    <> "$do$;\n"
                    <> "CREATE DATABASE \"codd-test-db\" WITH OWNER=\"codd-test-user\";\n"
                    <> "GRANT CONNECT ON DATABASE \"codd-test-db\" TO \"codd-test-user\";"
                , migrationInTxn          = False
-- A custom connection string is necessary because the DB doesn't yet exist
                , migrationCustomConnInfo = Just connInfo
                                                { connectUser     = "postgres"
                                                , connectDatabase = "postgres"
                                                }
                }
            migTimestamp
    pure CoddSettings
        { migsConnString   = connInfo
        , sqlMigrations    = Right (createTestUserMig : migs)
        , onDiskHashes     = Left ""
        , schemasToHash    = Include ["public", "codd-extra-mapped-schema"]
        , extraRolesToHash = Include ["codd-test-user", "extra-codd-test-user"] -- Important for HashingSpec
        , retryPolicy      = singleTryPolicy
        , txnIsolationLvl  = DbDefault
        , checksumAlgo     = ChecksumAlgo False False False
        , hashedChecksums  = True
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
    coddSettings@CoddSettings { migsConnString } <- testCoddSettings
        startingMigs

    runStdoutLoggingT
        $ (applyMigrationsNoCheck coddSettings testConnTimeout (const $ pure ())
          >> liftIO (act coddSettings)
          )
        `finally` withConnection
                      migsConnString { connectUser     = "postgres"
                                     , connectDatabase = "postgres"
                                     }
                      testConnTimeout
                        -- Some things aren't associated to a Schema and not even to a Database; they belong under the entire DB/postgres instance.
                        -- So we reset these things here, with the goal of getting the DB in the same state as it would be before even "createUserTestMig"
                        -- from "testCoddSettings" runs, so that each test is guaranteed the same starting DB environment.
                      (\conn -> do
                          execvoid_ conn "ALTER ROLE postgres RESET ALL;"
                          execvoid_ conn "DROP DATABASE \"codd-test-db\";"

                          allRoles :: [String] <-
                              map DB.fromOnly
                                  <$> query
                                          conn
                                          "SELECT rolname FROM pg_roles WHERE rolname NOT IN ('postgres') AND rolname NOT LIKE 'pg_%' ORDER BY rolname DESC"
                                          ()
                          forM_ allRoles $ \role -> do
                              let escapedRole =
                                      fromString ("\"" <> role <> "\"")
                              execvoid_ conn
                                  $  "DROP OWNED BY "
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
                      )

-- | Returns a Postgres UTC Timestamp that increases with its input parameter.
getIncreasingTimestamp :: DiffTime -> DB.UTCTimestamp
getIncreasingTimestamp n =
    DB.Finite $ addUTCTime (realToFrac n) $ UTCTime (fromGregorian 2020 1 1) 0

-- | Changes every added migrations's timestamp so they're applied in the order of the list.
fixMigsOrder :: [AddedSqlMigration] -> [AddedSqlMigration]
fixMigsOrder = zipWith
    (\i (AddedSqlMigration mig _) ->
        AddedSqlMigration mig
            $ getIncreasingTimestamp
            $ secondsToDiffTime
            $ fromIntegral i
    )
    [(0 :: Int) ..]

shouldBeStrictlySortedOn :: (Show a, Ord b) => [a] -> (a -> b) -> Expectation
shouldBeStrictlySortedOn xs f =
    zip xs (drop 1 xs) `shouldSatisfy` all (\(a1, a2) -> f a1 < f a2)
