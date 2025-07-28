module DbDependentSpecs.RetrySpec where

import Codd (applyMigrationsNoCheck)
import Codd.Environment (CoddSettings (..))
import Codd.Internal
  ( NoTxnMigrationApplicationFailure,
    withConnection,
  )
import Codd.Internal.MultiQueryStatement
  ( SqlStatementException,
  )
import Codd.Logging
  ( LoggingT (runLoggingT),
    Newline (..),
  )
import Codd.Parsing
  ( AddedSqlMigration (..),
    SqlMigration (..),
  )
import Codd.Query (unsafeQuery1)
import Codd.Types
  ( RetryBackoffPolicy (..),
    RetryPolicy (..),
  )
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT (..))
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime)
import qualified Database.PostgreSQL.Simple as DB
import DbUtils
  ( aroundCoddTestDb,
    aroundNoDatabases,
    getIncreasingTimestamp,
    mkValidSql,
    testConnTimeout,
  )
import Test.Hspec
import UnliftIO
  ( MonadIO,
    SomeException,
    hFlush,
    stdout,
  )
import UnliftIO.Concurrent
  ( MVar,
    modifyMVar_,
    newMVar,
    readMVar,
  )

spec :: Spec
spec = do
  describe "DbDependentSpecs" $ do
    describe "Retry tests" $ do
      aroundCoddTestDb
        $ it
          "In-txn migrations with failure in COPY statement are handled nicely"
        $ \dbInfo0 -> do
          -- This test might seem useless, but postgresql-simple support for COPY errors is not great,
          -- and we do quite a bit of sqlError exception handling that means we might forget to ROLLBACK
          -- in some cases
          let dbInfo =
                dbInfo0
                  { retryPolicy =
                      RetryPolicy
                        2
                        ( ExponentialBackoff (realToFrac @Double 0.001)
                        )
                  }
          logsmv <- newMVar []
          runMVarLogger
            logsmv
            ( applyMigrationsNoCheck
                dbInfo
                  { sqlMigrations =
                      [ "test/migrations/in-txn-application-error-with-COPY"
                      ]
                  }
                Nothing
                testConnTimeout
                (const $ pure ())
            )
            `shouldThrow` ( \(e :: SqlStatementException) ->
                              "duplicate key"
                                `List.isInfixOf` show e
                          )
          nonBootstrapAppliedMigs :: [(String, Int, Bool)] <-
            withConnection
              (migsConnString dbInfo)
              testConnTimeout
              ( \conn ->
                  DB.query
                    conn
                    "SELECT name, num_applied_statements, no_txn_failed_at IS NULL from codd.sql_migrations order by id OFFSET 1 -- Skip the bootstrap migration"
                    ()
              )
          nonBootstrapAppliedMigs `shouldBe` []
          logs <- readMVar logsmv
          length (filter ("ROLLBACK" `Text.isInfixOf`) logs)
            `shouldBe` 3
          length (filter ("BEGIN" `Text.isInfixOf`) logs)
            `shouldBe` 3
          length (filter ("COMMIT" `Text.isInfixOf`) logs)
            `shouldBe` 0

      aroundCoddTestDb
        $ it
          "In-txn migrations with failure in COMMIT are handled nicely"
        $ \dbInfo0 -> do
          let dbInfo =
                dbInfo0
                  { retryPolicy =
                      RetryPolicy
                        2
                        ( ExponentialBackoff (realToFrac @Double 0.001)
                        )
                  }
          logsmv <- newMVar []
          runMVarLogger
            logsmv
            ( applyMigrationsNoCheck
                dbInfo
                  { sqlMigrations =
                      [ "test/migrations/in-txn-application-error-on-COMMIT"
                      ]
                  }
                Nothing
                testConnTimeout
                (const $ pure ())
            )
            `shouldThrow` ( \(e :: SqlStatementException) ->
                              "duplicate key"
                                `List.isInfixOf` show e
                          )
          nonBootstrapAppliedMigs :: [(String, Int, Bool)] <-
            withConnection
              (migsConnString dbInfo)
              testConnTimeout
              ( \conn ->
                  DB.query
                    conn
                    "SELECT name, num_applied_statements, no_txn_failed_at IS NULL from codd.sql_migrations order by id OFFSET 1 -- Skip the bootstrap migration"
                    ()
              )
          nonBootstrapAppliedMigs `shouldBe` []
          logs <- readMVar logsmv
          length (filter ("ROLLBACK" `Text.isInfixOf`) logs)
            `shouldBe` 0 -- Instead of ROLLBACK, we see "COMMIT failed" messages
          length (filter ("COMMIT failed" `Text.isInfixOf`) logs)
            `shouldBe` 3
          length
            ( filter
                ( "duplicate key value violates unique constraint" `Text.isInfixOf`
                )
                logs
            )
            `shouldBe` 3
          length (filter ("BEGIN" `Text.isInfixOf`) logs)
            `shouldBe` 3

      aroundCoddTestDb
        $ it
          "No-txn migration with failure in statement not in explicit transaction block retries from the right place"
        $ \dbInfo0 -> do
          -- We want retries to ensure applied statements are not being applied more than once
          let dbInfo =
                dbInfo0
                  { retryPolicy =
                      RetryPolicy
                        2
                        ( ExponentialBackoff (realToFrac @Double 0.001)
                        )
                  }
          logsmv <- newMVar []
          runMVarLogger
            logsmv
            ( applyMigrationsNoCheck
                dbInfo
                  { sqlMigrations =
                      [ "test/migrations/no-txn-partial-application-error-outside-txn"
                      ]
                  }
                Nothing
                testConnTimeout
                (const $ pure ())
            )
            `shouldThrow` ( \(e :: NoTxnMigrationApplicationFailure) ->
                              "duplicate key"
                                `List.isInfixOf` show e
                          )
          ( appliedMigs :: [(String, Int, Bool)],
            datatable ::
              [ DB.Only
                  Int
              ]
            ) <-
            withConnection
              (migsConnString dbInfo)
              testConnTimeout
              ( \conn ->
                  (,)
                    <$> DB.query
                      conn
                      "SELECT name, num_applied_statements, no_txn_failed_at IS NULL from codd.sql_migrations order by id"
                      ()
                    <*> DB.query
                      conn
                      "SELECT id from somedata order by id"
                      ()
              )
          logs <- readMVar logsmv
          appliedMigs
            `shouldContain` [ ( "2000-01-01-00-00-00-create-table-with-unique-id.sql",
                                2,
                                True
                              ),
                              ( "2001-01-01-00-00-00-insert-duplicate-not-in-explicit-transaction.sql",
                                1,
                                False
                              )
                            ]
          map DB.fromOnly datatable `shouldBe` [1, 2, 3, 4]
          length
            ( filter
                ( "duplicate key value violates unique constraint" `Text.isInfixOf`
                )
                logs
            )
            `shouldBe` 3

      aroundCoddTestDb
        $ it
          "No-txn migration with failure in statement inside BEGIN..COMMIT retries from the right place"
        $ \dbInfo0 -> do
          -- We want retries to ensure applied statements are not being applied more than once
          let dbInfo =
                dbInfo0
                  { retryPolicy =
                      RetryPolicy
                        2
                        ( ExponentialBackoff (realToFrac @Double 0.001)
                        )
                  }
          logsmv <- newMVar []
          runMVarLogger
            logsmv
            ( applyMigrationsNoCheck
                dbInfo
                  { sqlMigrations =
                      [ "test/migrations/no-txn-partial-application-error-inside-txn"
                      ]
                  }
                Nothing
                testConnTimeout
                (const $ pure ())
            )
            `shouldThrow` ( \(e :: NoTxnMigrationApplicationFailure) ->
                              "duplicate key"
                                `List.isInfixOf` show e
                          )
          ( appliedMigs :: [(String, Int, Bool)],
            datatable ::
              [ DB.Only
                  Int
              ]
            ) <-
            withConnection
              (migsConnString dbInfo)
              testConnTimeout
              ( \conn ->
                  (,)
                    <$> DB.query
                      conn
                      "SELECT name, num_applied_statements, no_txn_failed_at IS NULL from codd.sql_migrations order by id"
                      ()
                    <*> DB.query
                      conn
                      "SELECT id from somedata order by id"
                      ()
              )
          logs <- readMVar logsmv
          appliedMigs
            `shouldContain` [ ( "2000-01-01-00-00-00-create-table-with-unique-id.sql",
                                2,
                                True
                              ),
                              ( "2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql",
                                1,
                                False
                              )
                            ]
          map DB.fromOnly datatable `shouldBe` [1, 2, 3, 4]
          length
            ( filter
                ( "duplicate key value violates unique constraint" `Text.isInfixOf`
                )
                logs
            )
            `shouldBe` 3

      aroundCoddTestDb
        $ it
          "No-txn migration with failure in COMMIT statement retries from the right place, and so does a new `codd up`"
        $ \dbInfo0 -> do
          -- We want retries to ensure applied statements are not being applied more than once
          let dbInfo =
                dbInfo0
                  { retryPolicy =
                      RetryPolicy
                        2
                        ( ExponentialBackoff (realToFrac @Double 0.001)
                        )
                  }
          logsmv <- newMVar []
          runMVarLogger
            logsmv
            ( applyMigrationsNoCheck
                dbInfo
                  { sqlMigrations =
                      [ "test/migrations/no-txn-partial-application-error-on-commit"
                      ]
                  }
                Nothing
                testConnTimeout
                (const $ pure ())
            )
            `shouldThrow` ( \(e :: NoTxnMigrationApplicationFailure) ->
                              "duplicate key"
                                `List.isInfixOf` show e
                          )
          ( appliedMigs :: [(String, Int, Bool)],
            datatable ::
              [ DB.Only
                  Int
              ],
            othertableExists :: DB.Only Bool
            ) <-
            withConnection
              (migsConnString dbInfo)
              testConnTimeout
              ( \conn ->
                  (,,)
                    <$> DB.query
                      conn
                      "SELECT name, num_applied_statements, no_txn_failed_at IS NULL from codd.sql_migrations order by id"
                      ()
                    <*> DB.query
                      conn
                      "SELECT id from somedata order by id"
                      ()
                    <*> unsafeQuery1
                      conn
                      "SELECT EXISTS (SELECT FROM pg_tables WHERE tablename='othertablenotexists')"
                      ()
              )
          appliedMigs
            `shouldContain` [ ( "2000-01-01-00-00-00-create-table-with-unique-id.sql",
                                2,
                                True
                              ),
                              ( "2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql",
                                3,
                                False
                              )
                            ]
          map DB.fromOnly datatable `shouldBe` [1, 2, 3, 4, 5, 6]
          othertableExists `shouldBe` DB.Only False
          logs <- readMVar logsmv

          -- We want the statement that failed printed, a line saying how many statements had been applied and also saying from where it will be resumed (these statement numbers _will_ differ in this test)
          logs
            `shouldSatisfy` any
              ( \line ->
                  "COMMIT;"
                    `Text.isInfixOf` line
                    && "23505"
                      `Text.isInfixOf` line
              )
          length
            ( filter
                ( \line ->
                    "7 statements"
                      `Text.isInfixOf` line
                      && "8th failed"
                        `Text.isInfixOf` line
                      && "4th statement"
                        `Text.isInfixOf` line
                )
                logs
            )
            `shouldBe` 3 -- Total amount of attempts

          -- Now we resume application with a new `codd up` invocation, changing the failed no-txn migration from the 8th statements onwards so it can complete. If codd tries to apply statements that already have been applied, we'd get duplicate key violation exceptions
          logsSecondCoddUp <- newMVar []
          runMVarLogger logsSecondCoddUp $
            applyMigrationsNoCheck
              dbInfo
              ( Just
                  [ AddedSqlMigration
                      SqlMigration
                        { migrationName =
                            "2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql",
                          migrationSql =
                            mkValidSql
                              -- This needs to match 2001-01-01-insert-duplicate-inside-explicit-transaction.sql up to the statement right before BEGIN
                              "SELECT 4;\n\
                              \\n\
                              \-- Some comment\n\
                              \COPY somedata FROM STDIN WITH (FORMAT csv);\n\
                              \4\n\
                              \5\n\
                              \6\n\
                              \\\.\n\
                              \\n\
                              \-- Another comment\n\
                              \\n\
                              \SELECT 7;\n\
                              \BEGIN; SELECT 3; CREATE TABLE othertablenowwillexist(); COPY somedata FROM STDIN WITH (FORMAT csv);\n7\n8\n9\n\\.\nCOMMIT;",
                          migrationInTxn = False,
                          migrationRequiresCoddSchema = False,
                          migrationCustomConnInfo = Nothing,
                          migrationEnvVars = mempty
                        }
                      (getIncreasingTimestamp 2)
                  ]
              )
              testConnTimeout
              (const $ pure ())
          ( appliedMigsSecondTime :: [(String, Int, Bool)],
            datatableSecondTime ::
              [ DB.Only
                  Int
              ],
            othertableExistsSecondTime :: DB.Only Bool
            ) <-
            withConnection
              (migsConnString dbInfo)
              testConnTimeout
              ( \conn ->
                  (,,)
                    <$> DB.query
                      conn
                      "SELECT name, num_applied_statements, no_txn_failed_at IS NULL from codd.sql_migrations order by id"
                      ()
                    <*> DB.query
                      conn
                      "SELECT id from somedata order by id"
                      ()
                    <*> unsafeQuery1
                      conn
                      "SELECT EXISTS (SELECT FROM pg_tables WHERE tablename='othertablenowwillexist')"
                      ()
              )
          appliedMigsSecondTime
            `shouldContain` [ ( "2000-01-01-00-00-00-create-table-with-unique-id.sql",
                                2,
                                True
                              ),
                              ( "2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql",
                                8,
                                True
                              )
                            ]
          map DB.fromOnly datatableSecondTime
            `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]
          othertableExistsSecondTime `shouldBe` DB.Only True
          logsSecondTime <- readMVar logsSecondCoddUp

          -- We want the statement that says where we're resuming from to appear exactly once
          length
            ( filter
                ( \line ->
                    "Resuming application of partially applied"
                      `Text.isInfixOf` line
                      && "Skipping the first 3 SQL statements"
                        `Text.isInfixOf` line
                      && "starting application from the 4th statement"
                        `Text.isInfixOf` line
                )
                logsSecondTime
            )
            `shouldBe` 1

      aroundNoDatabases
        $ it
          "Bootstrapping no-txn migration still gets registered if it makes default connection string accessible before failing"
        $ \emptyTestDbInfo -> do
          -- Migration will make it accessible despite throwing an exception, and thus codd should _still_
          -- register that it was partially applied
          withConnection
            (migsConnString emptyTestDbInfo)
            testConnTimeout
            (const $ pure ())
            `shouldThrow` ( \(e :: SomeException) ->
                              "database \"codd-test-db\" does not exist"
                                `List.isInfixOf` show e
                          )
          logsmv <- newMVar []
          runMVarLogger
            logsmv
            ( applyMigrationsNoCheck
                ( emptyTestDbInfo
                    { retryPolicy =
                        RetryPolicy
                          3
                          ( ExponentialBackoff
                              (realToFrac @Double 0.001)
                          ),
                      sqlMigrations =
                        [ "test/migrations/bootstrap-no-txn-fails-but-makes-default-conn-accessible"
                        ]
                    }
                )
                Nothing
                testConnTimeout
                (const $ pure ())
            )
            `shouldThrow` ( \(e :: NoTxnMigrationApplicationFailure) ->
                              "division by zero"
                                `List.isInfixOf` show e
                                && "SELECT 1/0"
                                  `List.isInfixOf` show e
                          )

          withConnection
            (migsConnString emptyTestDbInfo)
            testConnTimeout
            $ \conn -> do
              (apname, apnstmts, apat, apfailedat) <-
                unsafeQuery1
                  conn
                  "SELECT name, num_applied_statements, applied_at, no_txn_failed_at FROM codd.sql_migrations"
                  ()
              (apname, apnstmts, apat)
                `shouldBe` ( "2000-01-01-00-00-00-bootstrap-but-fail.sql" :: String,
                             5 :: Int,
                             Nothing :: Maybe UTCTime
                           )
              apfailedat
                `shouldNotBe` (Nothing :: Maybe UTCTime)

      describe "Side-effect-less migrations" $ aroundCoddTestDb $ do
        let testRetries inTxn emptyTestDbInfo = do
              logsmv <- newMVar []
              runMVarLogger
                logsmv
                ( applyMigrationsNoCheck
                    ( emptyTestDbInfo
                        { retryPolicy =
                            RetryPolicy
                              7
                              ( ExponentialBackoff
                                  (realToFrac @Double 0.001)
                              ),
                          sqlMigrations =
                            [ if inTxn
                                then
                                  "test/migrations/retry-policy-test-in-txn/"
                                else
                                  "test/migrations/retry-policy-test-no-txn/"
                            ]
                        }
                    )
                    Nothing
                    testConnTimeout
                    (const $ pure ())
                )
                `shouldThrow` ( \(e :: SomeException) ->
                                  "division by zero"
                                    `List.isInfixOf` show e
                                    && "SELECT 7/0"
                                      `List.isInfixOf` show e
                              )
              logs <- readMVar logsmv
              length
                ( filter
                    ("before next try" `Text.isInfixOf`)
                    logs
                )
                `shouldBe` 7
              -- The last attempt isn't logged because we don't catch exceptions for it
              length
                ( filter
                    ("division by zero" `Text.isInfixOf`)
                    logs
                )
                `shouldBe` 8 -- The error appears once before each retry, but also once when the final exception is thrown
              forM_ [1 :: Int, 2, 4, 8, 16, 32, 64] $ \delay ->
                length
                  ( filter
                      ( ( "Waiting "
                            <> Text.pack (show delay)
                            <> "ms"
                        )
                          `Text.isInfixOf`
                      )
                      logs
                  )
                  `shouldBe` 1
        it "For in-txn migrations" $ testRetries True
        it "For no-txn migrations" $ testRetries False

runMVarLogger :: (MonadIO m) => MVar [Text] -> LoggingT m a -> m a
runMVarLogger logsmv m =
  runReaderT
    (runLoggingT m)
    ( \newline msg ->
        modifyMVar_
          logsmv
          ( \l -> do
              case newline of
                NoNewline -> do
                  Text.hPutStr stdout msg
                  hFlush stdout
                WithNewline -> Text.hPutStrLn stdout msg
              pure $ l ++ [msg]
          ),
      const True,
      True
    )
