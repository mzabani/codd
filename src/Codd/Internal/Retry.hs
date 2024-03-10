module Codd.Internal.Retry
    ( RetryIteration(..)
    , retryFold
    ) where

import           Codd.Logging                   ( CoddLogger
                                                , logError
                                                , logWarn
                                                )
import           Codd.Types                     ( RetryPolicy(..)
                                                , retryPolicyIterate
                                                )
import           Data.Maybe                     ( isNothing )
import qualified Data.Text                     as Text
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Exception             ( Exception
                                                , catch
                                                )

data RetryIteration a = RetryIteration
    { isLastTry     :: Bool
    , tryNumber     :: Int
    -- ^ 0-indexed try number. E.g. 0 is the first try, 1 is the first *retry*.
    , lastException :: Maybe a
    -- ^ If this is a retry, the exception that caused it
    }

-- | Retries an action as many times and with wait intervals according
-- to the supplied `RetryPolicy`, but only retries in case of synchronous
-- exceptions. Provides fold-like behavior for an accumulator
-- for each try, including the first one.
retryFold
    :: forall e m b a
     . (MonadUnliftIO m, CoddLogger m, Exception e)
    => RetryPolicy
    -> (b -> RetryIteration e -> m b)
    -- ^ Accumulating function. This runs even for the first try.
    -> b
    -- ^ Initial value of the accumulator.
    -> (b -> m a)
    -- ^ Action to retry. Any exceptions of the chosen type are caught and logged as errors.
    -- Retries don't happen in case no exceptions are thrown.
    -> m a
retryFold initialPol accf acc0 f = go initialPol acc0 0 Nothing
  where
    go rpol previousAcc tryNumber lastException = do
        let mNextPol = retryPolicyIterate rpol
            thisIter = RetryIteration { isLastTry     = isNothing mNextPol
                                      , tryNumber
                                      , lastException
                                      }
        thisAcc <- accf previousAcc thisIter
        case mNextPol of
            Nothing                    -> f thisAcc
            Just (waitIfFail, nextPol) -> catch (f thisAcc) $ \(ex :: e) -> do
                let waitTimeMS :: Int =
                        truncate $ (realToFrac waitIfFail :: Float) * 1000
                -- It would be more reasonable if this retryFold function didn't print anything, letting
                -- its callers do that. Maybe in the future.
                logError $ "Got SQL Error: " <> Text.pack (show ex)
                logWarn
                    $  "Waiting "
                    <> Text.pack (show waitTimeMS)
                    <> "ms before next try"
                threadDelay (1000 * waitTimeMS)
                go nextPol thisAcc (tryNumber + 1) (Just ex)
