module DbDependentSpecs.RetrySpec where

import           Codd                           ( applyMigrationsNoCheck )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( NoTxnMigrationApplicationFailure
                                                , withConnection
                                                )
import           Codd.Logging                   ( LoggingT(runLoggingT)
                                                , Newline(..)
                                                , runCoddLogger
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                )
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Types                     ( RetryBackoffPolicy(..)
                                                , RetryPolicy(..)
                                                )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import qualified Data.List                     as List
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( UTCTime )
import qualified Database.PostgreSQL.Simple    as DB
import           DbUtils                        ( aroundFreshDatabase
                                                , aroundTestDbInfo
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                , testConnTimeout
                                                )
import           Test.Hspec
import           UnliftIO                       ( MonadIO
                                                , SomeException
                                                , hFlush
                                                , stdout
                                                )
import           UnliftIO.Concurrent            ( MVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )

alwaysPassingMig, createTableMig, addColumnMig, alwaysFailingMig
    :: MonadThrow m => AddedSqlMigration m
alwaysPassingMig = AddedSqlMigration
    SqlMigration { migrationName           = "0001-always-passing.sql"
                 , migrationSql            = mkValidSql "SELECT 99"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 1)
createTableMig = AddedSqlMigration
    SqlMigration { migrationName           = "0002-create-table.sql"
                 , migrationSql = mkValidSql "CREATE TABLE anytable ();"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 2)
addColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0003-add-column.sql"
        , migrationSql            = mkValidSql
                                        "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 3)
alwaysFailingMig = AddedSqlMigration
    SqlMigration { migrationName           = "0004-always-failing.sql"
                 , migrationSql            = mkValidSql "SELECT 5/0"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 4)

changeConnUser
    :: CoddSettings -> String -> AddedSqlMigration m -> AddedSqlMigration m
changeConnUser dbInfo newUser mig = mig
    { addedSqlMig = (addedSqlMig mig)
                        { migrationCustomConnInfo =
                            let cinfo = fromMaybe
                                    (migsConnString dbInfo)
                                    (migrationCustomConnInfo (addedSqlMig mig))
                            in  Just cinfo { DB.connectUser = newUser }
                        }
    }

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Retry tests" $ do
            aroundFreshDatabase
                $ it
                      "In-txn migrations in same database as the default connection string get registered in the same transaction even for a different user"
                $ \dbInfo -> do
                      -- To test this we put three consecutive in-txn migrations on the default database under a different user, where the last migration always fails.
                      -- Neither of the three migrations should be registered in this scenario, as they were all rolled back.
                      runCoddLogger
                              (applyMigrationsNoCheck
                                  dbInfo
                                  (Just
                                      [ alwaysPassingMig -- Make sure the default connection is available and yet we're not to use it for registering the 3 migrations below
                                      , changeConnUser dbInfo
                                                       "codd-test-user"
                                                       createTableMig
                                      , changeConnUser dbInfo
                                                       "codd-test-user"
                                                       addColumnMig
                                      , changeConnUser dbInfo
                                                       "codd-test-user"
                                                       alwaysFailingMig
                                      ]
                                  )
                                  testConnTimeout
                                  (const $ pure ())
                              )
                          `shouldThrow` (\(_ :: SomeException) -> True)
                      allRegisteredMigs :: [String] <-
                          map DB.fromOnly <$> withConnection
                              (migsConnString dbInfo)
                              testConnTimeout
                              (\conn -> DB.query
                                  conn
                                  "SELECT name from codd_schema.sql_migrations"
                                  ()
                              )
                      allRegisteredMigs
                          `shouldNotContain` [ migrationName
                                                   (addedSqlMig @IO
                                                       createTableMig
                                                   )
                                             ]
                      allRegisteredMigs
                          `shouldNotContain` [ migrationName
                                                   (addedSqlMig @IO addColumnMig
                                                   )
                                             ]
                      allRegisteredMigs
                          `shouldNotContain` [ migrationName
                                                   (addedSqlMig @IO
                                                       alwaysFailingMig
                                                   )
                                             ]
            -- aroundTestDbInfo
            --     $ it
            --           "In-txn migrations in non-default database get registered after all said migrations are committed, not after each one is applied inside a transaction"
            --     $ error "TODO"
            aroundFreshDatabase
                $ it
                      "No-txn migration with failure in COMMIT statement retries from the right place, and so does a new `codd up`"
                $ \dbInfo0 -> do
                      -- We want retries to ensure applied statements are not being applied more than once
                      let
                          dbInfo = dbInfo0
                              { retryPolicy = RetryPolicy
                                  2
                                  (ExponentialBackoff (realToFrac @Double 0.001)
                                  )
                              }
                      logsmv <- newMVar []
                      runMVarLogger
                              logsmv
                              (applyMigrationsNoCheck
                                  dbInfo
                                      { sqlMigrations =
                                          [ "test/migrations/no-txn-partial-application-test"
                                          ]
                                      }
                                  Nothing
                                  testConnTimeout
                                  (const $ pure ())
                              )
                          `shouldThrow` (\(_ :: SomeException) -> True)
                      (appliedMigs :: [(String, Int, Bool)], datatable :: [ DB.Only
                                Int
                          ], othertableExists :: DB.Only Bool) <-
                          withConnection
                              (migsConnString dbInfo)
                              testConnTimeout
                              (\conn ->
                                  (,,)
                                      <$> DB.query
                                              conn
                                              "SELECT name, num_applied_statements, no_txn_failed_at IS NULL from codd_schema.sql_migrations order by id"
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
                          `shouldContain` [ ( "2000-01-01-00-00-00-create-table-with-unique-id.sql"
                                            , 2
                                            , True
                                            )
                                          , ( "2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql"
                                            , 3
                                            , False
                                            )
                                          ]
                      map DB.fromOnly datatable `shouldBe` [1, 2, 3, 4, 5, 6]
                      othertableExists `shouldBe` DB.Only False
                      logs <- readMVar logsmv

                      -- We want the statement that failed printed, a line saying how many statements had been applied and also saying from where it will be resumed (these statement numbers _will_ differ in this test)
                      logs
                          `shouldSatisfy` any
                                              (\line ->
                                                  "COMMIT;"
                                                      `Text.isInfixOf` line
                                                      &&               "23505"
                                                      `Text.isInfixOf` line
                                              )
                      length
                              (filter
                                  (\line ->
                                      "7 statements"
                                          `Text.isInfixOf` line
                                          &&               "8th failed"
                                          `Text.isInfixOf` line
                                          &&               "4th statement"
                                          `Text.isInfixOf` line
                                  )
                                  logs
                              )
                          `shouldBe` 3 -- Total amount of attempts

                      -- Now we resume application with a new `codd up` invocation, changing the failed no-txn migration from the 8th statements onwards so it can complete. If codd tries to apply statements that already have been applied, we'd get duplicate key violation exceptions
                      logsSecondCoddUp <- newMVar []
                      runMVarLogger logsSecondCoddUp $ applyMigrationsNoCheck
                          dbInfo
                          (Just
                              [ AddedSqlMigration
                                    SqlMigration
                                        { migrationName           =
                                            "2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql"
                                        , migrationSql            =
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
\BEGIN; SELECT 3; CREATE TABLE othertablenowwillexist(); COPY somedata FROM STDIN WITH (FORMAT csv);\n7\n8\n9\n\\.\nCOMMIT;"
                                        , migrationInTxn          = False
                                        , migrationCustomConnInfo = Nothing
                                        , migrationEnvVars        = mempty
                                        }
                                    (getIncreasingTimestamp 2)
                              ]
                          )
                          testConnTimeout
                          (const $ pure ())
                      (appliedMigsSecondTime :: [(String, Int, Bool)], datatableSecondTime :: [ DB.Only
                                Int
                          ], othertableExistsSecondTime :: DB.Only Bool) <-
                          withConnection
                              (migsConnString dbInfo)
                              testConnTimeout
                              (\conn ->
                                  (,,)
                                      <$> DB.query
                                              conn
                                              "SELECT name, num_applied_statements, no_txn_failed_at IS NULL from codd_schema.sql_migrations order by id"
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
                          `shouldContain` [ ( "2000-01-01-00-00-00-create-table-with-unique-id.sql"
                                            , 2
                                            , True
                                            )
                                          , ( "2001-01-01-00-00-00-insert-duplicate-inside-explicit-transaction.sql"
                                            , 8
                                            , True
                                            )
                                          ]
                      map DB.fromOnly datatableSecondTime
                          `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]
                      othertableExistsSecondTime `shouldBe` DB.Only True
                      logsSecondTime <- readMVar logsSecondCoddUp

                      -- We want the statement that says where we're resuming from to appear exactly once
                      length
                              (filter
                                  (\line ->
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


            -- aroundTestDbInfo
            --     $ it
            --           "No-txn migrations with COPY have countable-runnable statements skipped correctly"
            --     $ error "TODO"

            aroundTestDbInfo
                $ it
                      "Bootstrapping no-txn migration still gets registered if it makes default connection string accessible before failing"
                $ \emptyTestDbInfo -> do
                      -- Migration will make it accessible despite throwing an exception, and thus codd should _still_
                      -- register that it was partially applied
                      withConnection (migsConnString emptyTestDbInfo)
                                     testConnTimeout
                                     (const $ pure ())
                          `shouldThrow` (\(e :: SomeException) ->
                                            "database \"codd-test-db\" does not exist"
                                                `List.isInfixOf` show e
                                        )
                      logsmv <- newMVar []
                      runMVarLogger
                              logsmv
                              (applyMigrationsNoCheck
                                  (emptyTestDbInfo
                                      { retryPolicy   = RetryPolicy
                                          3
                                          (ExponentialBackoff
                                              (realToFrac @Double 0.001)
                                          )
                                      , sqlMigrations =
                                          [ "test/migrations/bootstrap-no-txn-fails-but-makes-default-conn-accessible"
                                          ]
                                      }
                                  )
                                  Nothing
                                  testConnTimeout
                                  (const $ pure ())
                              )
                          `shouldThrow` (\(e :: NoTxnMigrationApplicationFailure) ->
                                            "division by zero"
                                                `List.isInfixOf` show e

                                                &&               "SELECT 1/0"
                                                `List.isInfixOf` show e
                                        )


                      withConnection (migsConnString emptyTestDbInfo)
                                     testConnTimeout
                          $ \conn -> do
                                (apname, apnstmts, apat, apfailedat) <-
                                    unsafeQuery1
                                        conn
                                        "SELECT name, num_applied_statements, applied_at, no_txn_failed_at FROM codd_schema.sql_migrations"
                                        ()
                                (apname, apnstmts, apat)
                                    `shouldBe` ( "2000-01-01-00-00-00-bootstrap-but-fail.sql" :: String
                                               , 5 :: Int
                                               , Nothing :: Maybe UTCTime
                                               )
                                apfailedat
                                    `shouldNotBe` (Nothing :: Maybe UTCTime)

            describe "Side-effect-less migrations" $ aroundFreshDatabase $ do
                let
                    testRetries inTxn emptyTestDbInfo = do
                        logsmv <- newMVar []
                        runMVarLogger
                                logsmv
                                (applyMigrationsNoCheck
                                    (emptyTestDbInfo
                                        { retryPolicy   = RetryPolicy
                                            7
                                            (ExponentialBackoff
                                                (realToFrac @Double 0.001)
                                            )
                                        , sqlMigrations =
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
                            `shouldThrow` (\(e :: SomeException) ->
                                              "division by zero"
                                                  `List.isInfixOf` show e

                                                  &&               "SELECT 7/0"
                                                  `List.isInfixOf` show e
                                          )
                        logs <- readMVar logsmv
                        length
                                (filter ("before next try" `Text.isInfixOf`)
                                        logs
                                )
                            `shouldBe` 7
                        -- The last attempt isn't logged because we don't catch exceptions for it
                        length
                                (filter
                                    ("division by zero" `Text.isInfixOf`)
                                    logs
                                )
                            `shouldBe` 8 -- The error appears once before each retry, but also once when the final exception is thrown
                        forM_ [1 :: Int, 2, 4, 8, 16, 32, 64] $ \delay ->
                            length
                                    (filter
                                        ((  "Waiting "
                                         <> Text.pack (show delay)
                                         <> "ms"
                                         ) `Text.isInfixOf`
                                        )
                                        logs
                                    )
                                `shouldBe` 1
                it "For in-txn migrations" $ testRetries True
                it "For no-txn migrations" $ testRetries False

runMVarLogger :: MonadIO m => MVar [Text] -> LoggingT m a -> m a
runMVarLogger logsmv m = runReaderT
    (runLoggingT m)
    ( \newline msg -> modifyMVar_
        logsmv
        (\l -> do
            case newline of
                NoNewline -> do
                    Text.hPutStr stdout msg
                    hFlush stdout
                WithNewline -> Text.hPutStrLn stdout msg
            pure $ l ++ [msg]
        )
    , const True
    , True
    )
