module Types (Verbosity(..), runVerbosityLogger) where

import Control.Monad.Logger (LoggingT(..), runStdoutLoggingT)
import UnliftIO (MonadIO)

data Verbosity = Verbose | NonVerbose

runVerbosityLogger :: MonadIO m => Verbosity -> LoggingT m a -> m a
runVerbosityLogger Verbose m = runStdoutLoggingT m
runVerbosityLogger NonVerbose m = runLoggingT m (\_ _ _ _ -> pure ())