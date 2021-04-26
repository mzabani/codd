module Codd.Internal.Retry
    ( retry
    ) where

import           Codd.Types                     ( RetryPolicy(..)
                                                , retryPolicyIterate
                                                )
import           Control.Monad.Logger           ( MonadLogger
                                                , logWarnN
                                                )
import qualified Data.Text                     as Text
import           Data.Time                      ( nominalDiffTimeToSeconds )
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Exception             ( catchAny )

retry :: (MonadUnliftIO m, MonadLogger m) => RetryPolicy -> m c -> m c
retry rpol f = case retryPolicyIterate rpol of
    Nothing                    -> f
    Just (waitIfFail, nextPol) -> do
        catchAny f $ \e -> do
            let waitTimeMS :: Int =
                    truncate $ nominalDiffTimeToSeconds waitIfFail * 1000
            logWarnN $ "Got SQL Error: " <> Text.pack (show e)
            logWarnN
                $  "Waiting "
                <> Text.pack (show waitTimeMS)
                <> "ms before next try"
            threadDelay (1000 * waitTimeMS)
            logWarnN "Retrying"
            retry nextPol f
