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

data Verbosity = Verbose | NonVerbose

printLogMsg :: MonadIO m => Verbosity -> LogLevel -> LogStr -> Handle -> m ()
printLogMsg v level (decodeUtf8 . fromLogStr -> msg) handle = liftIO $ do
    suppColor <- hSupportsANSIColor stdout
    let (printFunc, finalMsg) =
            case unlikelyCharSequence `Text.stripPrefix` msg of
                Just noNewLineMsg ->
                    ( \t -> do
                        Text.hPutStr handle $ colorReplace suppColor t
                        hFlush handle
                    , noNewLineMsg
                    )
                Nothing ->
                    (Text.hPutStrLn handle . colorReplace suppColor, msg)
    case (v, level) of
        (NonVerbose, LevelDebug) -> pure ()
        (NonVerbose, LevelInfo ) -> pure ()
        (_, LevelWarn) -> printFunc $ "<YELLOW>Warn:</YELLOW> " <> finalMsg
        (_         , LevelError) -> printFunc $ "<RED>Error:</RED> " <> finalMsg
        _                        -> printFunc finalMsg

  where
    cyan    = fromString $ setSGRCode [SetColor Foreground Dull Cyan]
    green   = fromString $ setSGRCode [SetColor Foreground Dull Green]
    magenta = fromString $ setSGRCode [SetColor Foreground Dull Magenta]
    red     = fromString $ setSGRCode [SetColor Foreground Dull Red]
    yellow  = fromString $ setSGRCode [SetColor Foreground Dull Yellow]
    reset   = fromString $ setSGRCode [Reset]
    colorReplace suppColor =
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
runCoddLogger v m =
    runLoggingT m (\_loc _source level msg -> printLogMsg v level msg stdout)

unlikelyCharSequence :: IsString a => a
unlikelyCharSequence = "      -----$$$$$+++++%%%%%######@@@@@"

logInfoNoNewline :: MonadLogger m => Text -> m ()
logInfoNoNewline msg = logInfoN $ unlikelyCharSequence <> msg -- We cheat and prefix with an uncommon character sequence that indicates to our printer not to use a newline. Very ugly, but really practical.

-- | Logs Errors only, and to stderr. Useful if some command needs to produce output to be consumed
-- by other programs, such as JSON output, so that they can `putStrLn` themselves and not worry about
-- anything else going to stdout.
runErrorsOnlyLogger :: LoggingT m a -> m a
runErrorsOnlyLogger m = runLoggingT
    m
    (\_loc _source level msg -> case level of
        LevelError -> printLogMsg Verbose level msg stderr
        _          -> pure ()
    )
