module Codd.Internal.Retry
    ( RetryIteration(..)
    , retry_
    , retryFold
    ) where

import           Codd.Types                     ( RetryPolicy(..)
                                                , retryPolicyIterate
                                                )
import           Control.Monad.Logger           ( MonadLogger
                                                , logErrorN
                                                , logWarnN
                                                )
import           Data.Maybe                     ( isNothing )
import qualified Data.Text                     as Text
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Exception             ( catchAny )

data RetryIteration = RetryIteration
    { isLastTry :: Bool
    , tryNumber :: Int
    -- ^ 0-indexed try number. E.g. 0 is the first try, 1 is the first *retry*.
    }

retry_ :: (MonadUnliftIO m, MonadLogger m) => RetryPolicy -> m c -> m c
retry_ rpol f = retryFold rpol accf () (const f) where accf () _ = pure ()

-- | Retries an action as many times and with wait intervals according
-- to the supplied `RetryPolicy`, but only retries in case of synchronous
-- exceptions. Provides fold-like behavior for an accumulator
-- for each try, including the first one.
retryFold
    :: (MonadUnliftIO m, MonadLogger m)
    => RetryPolicy
    -> (b -> RetryIteration -> m b)
    -- ^ Accumulating function. This runs even for the first try.
    -> b
    -- ^ Initial value of the accumulator.
    -> (b -> m a)
    -- ^ Action to retry. Any synchronous exceptions are caught and logged.
    -- Retries don't happen in case no exceptions are thrown.
    -> m a
retryFold initialPol accf acc0 f = go initialPol acc0 0
  where
    go rpol previousAcc tryNumber = do
        let mNextPol = retryPolicyIterate rpol
            thisIter =
                RetryIteration { isLastTry = isNothing mNextPol, tryNumber }
        thisAcc <- accf previousAcc thisIter
        case mNextPol of
            Nothing                    -> f thisAcc
            -- UnliftIO's `catchAny` does not catch async exceptions,
            -- which is what we want here.
            Just (waitIfFail, nextPol) -> catchAny (f thisAcc) $ \e -> do
                let waitTimeMS :: Int =
                        truncate $ (realToFrac waitIfFail :: Float) * 1000
                logErrorN $ "Got SQL Error: " <> Text.pack (show e)
                logWarnN
                    $  "Waiting "
                    <> Text.pack (show waitTimeMS)
                    <> "ms before next try"
                threadDelay (1000 * waitTimeMS)
                go nextPol thisAcc (tryNumber + 1)
