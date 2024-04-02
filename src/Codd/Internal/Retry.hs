module Codd.Internal.Retry
    ( RetryIteration(..)
    , retryFold
    ) where

import           Codd.Logging                   ( CoddLogger
                                                , logWarn
                                                )
import           Codd.Types                     ( RetryPolicy(..)
                                                , retryPolicyIterate
                                                )
import           Data.Maybe                     ( isNothing )
import qualified Data.Text                     as Text
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Concurrent            ( threadDelay )

data RetryIteration e = RetryIteration
    { isLastTry :: Bool
    , tryNumber :: Int
    -- ^ 0-indexed try number. E.g. 0 is the first try, 1 is the first *retry*.
    , lastError :: Maybe e
    -- ^ If this is a retry, the error that caused it
    }

-- | Retries an action as many times and with wait intervals according
-- to the supplied `RetryPolicy`, but uses result types to know when to retry, so exceptions
-- will just bubble up - this will not handle them at all. Provides fold-like behavior for an accumulator
-- for each try, including the first one.
retryFold
    :: forall e m b a
     . (MonadUnliftIO m, CoddLogger m)
    => RetryPolicy
    -> (b -> RetryIteration e -> m b)
    -- ^ Accumulating function. This runs even for the first try.
    -> b
    -- ^ Initial value of the accumulator.
    -> (Either e a -> m a)
    -- ^ Called after the action succeeds or after all retries fail.
    -> (b -> m (Either e a))
    -- ^ Action to attempt/retry, which will return a `Left` if it needs to be retried and a `Right` if it succeeded.
    -> m a
retryFold initialPol accf acc0 final f = go initialPol acc0 0 Nothing
  where
    go rpol previousAcc tryNumber lastError = do
        let mNextPol = retryPolicyIterate rpol
            thisIter = RetryIteration { isLastTry = isNothing mNextPol
                                      , tryNumber
                                      , lastError
                                      }
        thisAcc <- accf previousAcc thisIter
        case mNextPol of
            Nothing                    -> f thisAcc >>= final
            Just (waitIfFail, nextPol) -> do
                ret <- f thisAcc
                case ret of
                    Left e -> do
                        let
                            waitTimeMS :: Int =
                                truncate
                                    $ (realToFrac waitIfFail :: Float)
                                    * 1000
                        -- It would be more reasonable if this retryFold function didn't print anything, letting
                        -- its callers do that. Maybe in the future.
                        logWarn
                            $  "Waiting "
                            <> Text.pack (show waitTimeMS)
                            <> "ms before next try"
                        threadDelay (1000 * waitTimeMS)
                        go nextPol thisAcc (tryNumber + 1) (Just e)
                    Right a -> final $ Right a
