module Types
    ( Verbosity(..)
    , runVerbosityLogger
    , runErrorsOnlyLogger
    ) where

import           Control.Monad.Logger           ( LogLevel(..)
                                                , LoggingT(..)
                                                , defaultLogStr
                                                , fromLogStr
                                                , runStdoutLoggingT
                                                )
import qualified Data.ByteString.Char8         as S8
import           UnliftIO                       ( MonadIO
                                                , stderr
                                                )

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

-- |Logs Errors only, and to stderr.
runErrorsOnlyLogger :: MonadIO m => LoggingT m a -> m a
runErrorsOnlyLogger m = runLoggingT
    m
    (\loc source level str -> case level of
        LevelError -> S8.hPutStrLn stderr $ fromLogStr $ defaultLogStr
            loc
            source
            level
            str
        _ -> pure ()
    )
