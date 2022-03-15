module Codd.Internal.Retry
    ( RetryIteration(..)
    , retry
    , retry_
    ) where

import           Codd.Types                     ( RetryPolicy(..)
                                                , retryPolicyIterate
                                                )
import           Control.Monad.Logger           ( MonadLogger
                                                , logWarnN
                                                )
import qualified Data.Text                     as Text
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Exception             ( catchAny )

data RetryIteration = RetryIteration
    { isLastTry :: Bool
    , tryNumber :: Int
    -- ^ 0-indexed try number. E.g. 0 is the first try, 1 is the first *retry*.
    }

retry
    :: (MonadUnliftIO m, MonadLogger m)
    => RetryPolicy
    -> (RetryIteration -> m c)
    -> m c
retry initialPol f = go initialPol 0
  where
    go rpol tryNum = case retryPolicyIterate rpol of
        Nothing                    -> f $ RetryIteration True tryNum
        Just (waitIfFail, nextPol) -> do
            catchAny (f $ RetryIteration False tryNum) $ \e -> do
                let waitTimeMS :: Int =
                        truncate $ (realToFrac waitIfFail :: Float) * 1000
                logWarnN $ "Got SQL Error: " <> Text.pack (show e)
                logWarnN
                    $  "Waiting "
                    <> Text.pack (show waitTimeMS)
                    <> "ms before next try"
                threadDelay (1000 * waitTimeMS)
                logWarnN "Retrying"
                go nextPol (tryNum + 1)

retry_ :: (MonadUnliftIO m, MonadLogger m) => RetryPolicy -> m c -> m c
retry_ rpol f = retry rpol (const f)
