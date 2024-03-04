module Codd.Logging
    ( Verbosity(..)
    , logInfoNoNewline
    , runCoddLogger
    , runErrorsOnlyLogger
    ) where

import           Control.Monad.Logger           ( LogLevel(..)
                                                , LogStr
                                                , LoggingT(..)
                                                , MonadLogger
                                                , fromLogStr
                                                , logInfoN
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
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
                                                , hFlush
                                                , liftIO
                                                , stderr
                                                , stdout
                                                )

{-|
Our usage of monad-logger is a travesty. Codd calls `logInfoN` and other methods with string markers inside the message itself
that are replaced by colours or are stripped away and interpreted as a signal not to add a newline when printing.

The problem is that all of these string markers will be printed ipsis litteris by any other monad-logger that is not `runCoddLogger`,
including for example `runStdoutLoggingT` and `runStderrLoggingT`, but also any custom monad-logger instances in the world out there.

We could instead use ANSI color escape codes, but we don't want to include them in contexts where colors aren't supported, which is hard without a custom instance of monad-logger.

One approach would be to require (MonadIO m, MonadLogger m) everywhere and make codd use new functions we create here, such as a
`logCoddInfoN`. We could then detect support for colors and conditionally replace string markers.

While this would be much cleaner, that MonadIO constraint would be overkill for some parts of our codebase.

So the proper thing to do would probably be to stop using monad-logger altogether, and create our own `MonadLogger` class. 
-}
data Verbosity = Verbose | NonVerbose

printLogMsg
    :: MonadIO m => Bool -> Verbosity -> LogLevel -> LogStr -> Handle -> m ()
printLogMsg suppColor v level (decodeUtf8 . fromLogStr -> msg) handle =
    liftIO $ do
        let (printFunc, finalMsg) =
                case unlikelyCharSequence `Text.stripPrefix` msg of
                    Just noNewLineMsg ->
                        ( \t -> do
                            Text.hPutStr handle $ colorReplace t
                            hFlush handle
                        , noNewLineMsg
                        )
                    Nothing -> (Text.hPutStrLn handle . colorReplace, msg)
        case (v, level) of
            (NonVerbose, LevelDebug) -> pure ()
            (NonVerbose, LevelInfo) -> pure ()
            (_, LevelWarn) -> printFunc $ "<YELLOW>Warn:</YELLOW> " <> finalMsg
            (_, LevelError) -> printFunc $ "<RED>Error:</RED> " <> finalMsg
            _ -> printFunc finalMsg

  where
    cyan    = fromString $ setSGRCode [SetColor Foreground Dull Cyan]
    green   = fromString $ setSGRCode [SetColor Foreground Dull Green]
    magenta = fromString $ setSGRCode [SetColor Foreground Dull Magenta]
    red     = fromString $ setSGRCode [SetColor Foreground Dull Red]
    yellow  = fromString $ setSGRCode [SetColor Foreground Dull Yellow]
    reset   = fromString $ setSGRCode [Reset]
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
runCoddLogger :: MonadIO m => Verbosity -> LoggingT m a -> m a
runCoddLogger v m = do
    suppColor <- liftIO $ hSupportsANSIColor stdout
    runLoggingT
        m
        (\_loc _source level msg -> printLogMsg suppColor v level msg stdout)

unlikelyCharSequence :: IsString a => a
unlikelyCharSequence = "      -----$$$$$+++++%%%%%######@@@@@"

logInfoNoNewline :: MonadLogger m => Text -> m ()
logInfoNoNewline msg = logInfoN $ unlikelyCharSequence <> msg -- We cheat and prefix with an uncommon character sequence that indicates to our printer not to use a newline. Very ugly, but really practical.

-- | Logs Errors only, and to stderr. Useful if some command needs to produce output to be consumed
-- by other programs, such as JSON output, so that they can `putStrLn` themselves and not worry about
-- anything else going to stdout.
runErrorsOnlyLogger :: MonadIO m => LoggingT m a -> m a
runErrorsOnlyLogger m = do
    suppColor <- liftIO $ hSupportsANSIColor stdout
    runLoggingT
        m
        (\_loc _source level msg -> case level of
            LevelError -> printLogMsg suppColor Verbose level msg stderr
            _          -> pure ()
        )
