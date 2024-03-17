module DbDependentSpecs.RetrySpec where

import           Codd                           ( applyMigrationsNoCheck )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( MigrationApplicationFailure )
import           Codd.Logging                   ( LoggingT(runLoggingT)
                                                , Newline(..)
                                                )
import           Codd.Types                     ( RetryBackoffPolicy(..)
                                                , RetryPolicy(..)
                                                )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Reader           ( ReaderT(..) )
import qualified Data.List                     as List
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           DbUtils                        ( aroundFreshDatabase
                                                , testConnTimeout
                                                )
import           Test.Hspec
import           UnliftIO                       ( MonadIO
                                                , hFlush
                                                , stdout
                                                )
import           UnliftIO.Concurrent            ( MVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Retry tests" $ aroundFreshDatabase $ do
            let
                testRetries inTxn emptyTestDbInfo = do
                    -- Kind of ugly.. we test behaviour by analyzing logs and
                    -- trust that threadDelay is called.
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
                    length (filter ("before next try" `Text.isInfixOf`) logs)
                        `shouldBe` 7
                    -- The last attempt isn't logged because we don't catch exceptions for it
                    length (filter ("division by zero" `Text.isInfixOf`) logs)
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
