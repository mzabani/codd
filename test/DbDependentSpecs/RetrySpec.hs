module DbDependentSpecs.RetrySpec where

import           Codd                           ( applyMigrationsNoCheck )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( MigrationApplicationFailure
                                                , withConnection
                                                )
import           Codd.Logging                   ( LoggingT(runLoggingT)
                                                , Newline(..)
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
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( UTCTime )
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

createTableMig, addColumnMig :: MonadThrow m => AddedSqlMigration m
createTableMig = AddedSqlMigration
    SqlMigration { migrationName           = "0001-create-table.sql"
                 , migrationSql = mkValidSql "CREATE TABLE anytable ();"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 1)
addColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0002-add-column.sql"
        , migrationSql            = mkValidSql
                                        "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 2)

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Retry tests" $ do
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
                          `shouldThrow` (\(e :: MigrationApplicationFailure) ->
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
                            `shouldThrow` (\(e :: MigrationApplicationFailure) ->
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
