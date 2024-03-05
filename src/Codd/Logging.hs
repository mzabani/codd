module Codd.Logging
    ( MonadLogger
    , LoggingT(..)
    , LogLevel(..)
    , Newline(..)
    , logInfoNoNewline
    , logInfoAlways
    , logDebug
    , logInfo
    , logWarn
    , logError
    , runCoddLogger
    , runErrorsOnlyLogger
    , runCoddLoggerLevelFilter
    ) where

import           Control.Monad.Reader           ( MonadReader(..)
                                                , MonadTrans(..)
                                                , ReaderT(..)
                                                , when
                                                )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           System.Console.ANSI            ( Color(..)
                                                , ColorIntensity(..)
                                                , ConsoleLayer(..)
                                                , SGR(..)
                                                , hSupportsANSIColor
                                                )
import           System.Console.ANSI.Codes      ( setSGRCode )
import           UnliftIO                       ( Handle
                                                , MonadIO
                                                , MonadUnliftIO
                                                , hFlush
                                                , liftIO
                                                , stderr
                                                , stdout
                                                )
import           UnliftIO.Resource              ( MonadResource
                                                , ResourceT
                                                )

{-|
One might ask why not use monad-logger instead of developing our own equivalent. It's a fair question.

I started with monad-logger, but at some point wanted to add colors and also print without a newline at the end. All of this, of course, without resorting directly to `putStr` and/or `putStrLn`, because then we'd lose the ability to filter out some of the output
when e.g. the user has `--quiet` set, among other cases.

To support colors we'd have to force a `MonadIO m` constraint on every caller of the `log*` functions so that they can detect terminal
color support. One alternative that wouldn't require that constraint would be to strip away color escape codes in our `MonadLogger` instance, but Stack Overflow answers suggest this would be pretty hard to do well.

Printing without a newline also has no support in monad-logger AFAICT, so we previously had to add an uncommon prefix to strings and
detect and strip that away internally to differentiate it from printing with a newline. A big hack.

So by not using monad-logger, we can reduce the amount of monkey patching. We are more free to develop things like detecting "<GREEN>some text</GREEN>" and users of codd-the-library wouldn't be surprised by such custom behaviour, unlike what would happen if someone
tried to `runStdoutLoggingT`.
-}

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError
  deriving stock (Eq, Ord)

class MonadLogger m where
    logNoNewline :: LogLevel -> Text -> m ()
    logLine :: LogLevel -> Text -> m ()
    logLineAlways :: LogLevel -> Text -> m ()

instance (Monad m, MonadLogger m) => MonadLogger (ResourceT m) where
    logNoNewline l msg = lift $ logNoNewline l msg
    logLine l msg = lift $ logLine l msg
    -- | Logs a line regardless of the user's level filters
    logLineAlways l msg = lift $ logLineAlways l msg

logInfoNoNewline :: MonadLogger m => Text -> m ()
logInfoNoNewline = do
    logNoNewline LevelInfo

logInfo :: MonadLogger m => Text -> m ()
logInfo = logLine LevelInfo
logInfoAlways :: MonadLogger m => Text -> m ()
logInfoAlways = logLineAlways LevelInfo
logDebug :: MonadLogger m => Text -> m ()
logDebug = logLine LevelDebug
logWarn :: MonadLogger m => Text -> m ()
logWarn = logLine LevelWarn
logError :: MonadLogger m => Text -> m ()
logError = logLine LevelError

data Newline = WithNewline | NoNewline
newtype LoggingT m a = LoggingT { runLoggingT :: ReaderT (Newline -> Text -> IO (), LogLevel -> Bool, Bool) m a }
    deriving newtype (Applicative, Functor, Monad, MonadFail, MonadIO, MonadResource, MonadThrow, MonadTrans, MonadUnliftIO)

instance MonadIO m => MonadLogger (LoggingT m) where
    logNoNewline l msg = do
        (printFunc, logFilter, suppColor) <- LoggingT ask
        when (logFilter l) $ printLogMsg suppColor l msg (printFunc NoNewline)

    logLine l msg = do
        (printFunc, logFilter, suppColor) <- LoggingT ask
        when (logFilter l) $ printLogMsg suppColor l msg (printFunc WithNewline)

    logLineAlways l msg = do
        (printFunc, _, suppColor) <- LoggingT ask
        printLogMsg suppColor l msg (printFunc WithNewline)

printLogMsg :: MonadIO m => Bool -> LogLevel -> Text -> (Text -> IO ()) -> m ()
printLogMsg suppColor level msg printFunc = liftIO $ do
    case level of
        LevelWarn ->
            printFunc $ colorReplace $ "<YELLOW>Warn:</YELLOW> " <> msg
        LevelError -> printFunc $ colorReplace $ "<RED>Error:</RED> " <> msg
        _          -> printFunc $ colorReplace msg

  where
    cyan    = Text.pack $ setSGRCode [SetColor Foreground Dull Cyan]
    green   = Text.pack $ setSGRCode [SetColor Foreground Dull Green]
    magenta = Text.pack $ setSGRCode [SetColor Foreground Dull Magenta]
    red     = Text.pack $ setSGRCode [SetColor Foreground Dull Red]
    yellow  = Text.pack $ setSGRCode [SetColor Foreground Dull Yellow]
    reset   = Text.pack $ setSGRCode [Reset]
    colorReplace =
        Text.replace "<CYAN>" (if suppColor then cyan else "")
            . Text.replace "</CYAN>" (if suppColor then reset else "")
            . Text.replace "<GREEN>" (if suppColor then green else "")
            . Text.replace "</GREEN>" (if suppColor then reset else "")
            . Text.replace "<MAGENTA>" (if suppColor then magenta else "")
            . Text.replace "</MAGENTA>" (if suppColor then reset else "")
            . Text.replace "<RED>" (if suppColor then red else "")
            . Text.replace "</RED>" (if suppColor then reset else "")
            . Text.replace "<YELLOW>" (if suppColor then yellow else "")
            . Text.replace "</YELLOW>" (if suppColor then reset else "")

-- | Does not print Debug and Info if non-verbose, prints everything else otherwise.
-- Prefixes Warn and Error messages with "Warn: " and "Error: " and uses Yellow and Red colors
-- when the terminal supports them for those. Also, by convention, parses special strings such as <GREEN>some text</GREEN>
-- to use colors when the terminal supports them.
runCoddLogger :: MonadIO m => LoggingT m a -> m a
runCoddLogger = runCoddLoggerLevelFilter (const True)

runCoddLoggerLevelFilter
    :: MonadIO m => (LogLevel -> Bool) -> LoggingT m a -> m a
runCoddLoggerLevelFilter levelFilter m = do
    suppColor <- liftIO $ hSupportsANSIColor stdout
    runReaderT (runLoggingT m) (logPrinter stdout, levelFilter, suppColor)

-- | Logs Errors only, and to stderr. Useful if some command needs to produce output to be consumed
-- by other programs, such as JSON output, so that they can `putStrLn` themselves and not worry about
-- anything else going to stdout.
runErrorsOnlyLogger :: MonadIO m => LoggingT m a -> m a
runErrorsOnlyLogger m = do
    suppColor <- liftIO $ hSupportsANSIColor stdout
    runReaderT (runLoggingT m) (logPrinter stderr, (>= LevelError), suppColor)

logPrinter :: Handle -> Newline -> Text -> IO ()
logPrinter handle n msg = case n of
    NoNewline -> do
        Text.hPutStr handle msg
        hFlush handle
    WithNewline -> Text.hPutStrLn handle msg
