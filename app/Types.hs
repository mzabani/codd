module Types
    ( Verbosity(..)
    , runVerbosityLogger
    ) where

import           Control.Monad.Logger           ( LogLevel
                                                    ( LevelDebug
                                                    , LevelInfo
                                                    )
                                                , LoggingT(..)
                                                , defaultLogStr
                                                , fromLogStr
                                                , runStdoutLoggingT
                                                )
import qualified Data.ByteString.Char8         as S8
import           UnliftIO                       ( MonadIO )

data Verbosity = Verbose | NonVerbose

runVerbosityLogger :: MonadIO m => Verbosity -> LoggingT m a -> m a
runVerbosityLogger Verbose    m = runStdoutLoggingT m
runVerbosityLogger NonVerbose m = runLoggingT
    m
    (\loc source level str -> case level of
        LevelDebug -> pure ()
        LevelInfo -> pure ()
        _ -> S8.putStrLn $ fromLogStr $ defaultLogStr loc source level str
    )
