module SystemResourcesSpecs.OpenFilesSpec where

import Codd
  ( VerifySchemas (..),
    applyMigrations,
  )
import Codd.Environment (CoddSettings (..))
import Codd.Logging (runCoddLogger)
import Control.Applicative ((<|>))
import Control.Monad
  ( foldM,
    forM_,
    void,
  )
import Control.Monad.Trans.Resource (MonadThrow (..))
import qualified Data.Attoparsec.Text as P
import qualified Data.Char as Char
import Data.Either (isLeft)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import DbUtils
  ( aroundCoddTestDb,
    testConnTimeout,
  )
import Test.Hspec
import UnliftIO
  ( SomeException,
    try,
  )

spec :: Spec
spec = do
  describe "SystemResourcesSpecs" $ do
    aroundCoddTestDb
      $ it
        "RUNNING - At most one .sql migration file and one on-disk representation file open at a time"
      $ \emptyTestDbInfo -> do
        void @IO $
          runCoddLogger $
            applyMigrations
              emptyTestDbInfo
                { sqlMigrations =
                    ["test/migrations/open-files-limit/"],
                  onDiskReps = Left "./expected-schema"
                }
              Nothing
              testConnTimeout
              LaxCheck -- This will output an error but will not throw. What matters is that on-disk reps are read
              -- This test must run wrapped in a specific strace incantation as it'll read the output log of that
              -- to assert that at most one migration file is opened at a time
    it
      "CHECKING - At most one .sql migration file and one on-disk representation file open at a time"
      $ do
        contentsE <-
          try $
            Text.readFile
              "/tmp/strace-codd-system-resources-test.log"
        case contentsE of
          Left (ex :: SomeException) -> do
            putStrLn
              "Error reading /tmp/strace-codd-system-resources-test.log. Are you running this with the runfile target or are you running this test directly? This test needs to run after a very specific `strace` wrapped command that you'll find in our Runfile test targets, or it doesn't work."
            throwM ex
          Right contents -> do
            let openAndCloseLines =
                  filter
                    ( \l ->
                        -- The filter below eliminates all lines that aren't open or close calls
                        not
                          ( "+++ exited with "
                              `Text.isInfixOf` l
                          )
                    )
                    $ Text.lines contents
            let isSystemFile filepath = any (`List.isPrefixOf` filepath) ["/proc", "/sys", "/var", "/etc", "/dev", "/nix"]
                addOpenFile :: Fd -> FilePath -> Map Fd FilePath -> Map Fd FilePath
                addOpenFile (Fd (-1)) _ m = m -- -1 is ENOENT/file not found, meaning a file isn't really open
                addOpenFile fd filepath m = Map.insert fd filepath m
            (_, openFdsAtEnd, atLeastOneNonSystemFileOpened) <-
              foldM
                ( \(bookkeepingPerPid, openFds, atLeastOneFile) line ->
                    case P.parseOnly (Left <$> openParser <|> Right <$> closeParser) line of
                      Right currentCall ->
                        let (newOpenFds, newBookkeeping) =
                              Map.alterF
                                ( \mPreviousCall ->
                                    let previousCall = fromMaybe (Right (CloseCallCompleted undefined undefined)) mPreviousCall
                                     in case (previousCall, currentCall) of
                                          -- My understanding is that "resuming" a syscall can only happen for the same process/pid
                                          -- but the same fd can be opened by one pid and closed by another
                                          (Left OpenCallUnfinished {}, Left OpenCallUnfinished {}) -> error "Consecutive unfinished Open-then-Open calls for the same pid"
                                          (Left OpenCallUnfinished {}, Left OpenCallCompleted {}) -> error "Unfinished Open not followed by open-resumed for the same pid"
                                          (Left OpenCallUnfinished {}, Right CloseCallUnfinished {}) -> error "Consecutive unfinished Open-then-Close calls for the same pid"
                                          (Right CloseCallUnfinished {}, Right CloseCallUnfinished {}) -> error "Consecutive unfinished Close-then-Close calls for the same pid"
                                          (Right CloseCallUnfinished {}, Right CloseCallCompleted {}) -> error "Unfinished Close not followed by close-resumed for the same pid"
                                          (Right CloseCallUnfinished {}, Left OpenCallUnfinished {}) -> error "Consecutive unfinished Close-then-Open calls for the same pid"
                                          (Left (OpenCallUnfinished filepath _), Left (OpenCallResumed _ fd)) -> (addOpenFile fd filepath openFds, Just currentCall)
                                          (Right (CloseCallUnfinished _ fd), Right (CloseCallResumed _)) -> (Map.delete fd openFds, Just currentCall)
                                          (_, Right (CloseCallCompleted _ fd)) -> (Map.delete fd openFds, Just currentCall)
                                          (_, Right (CloseCallUnfinished _ _)) -> (openFds, Just currentCall)
                                          (_, Left (OpenCallCompleted filepath _ fd)) -> (addOpenFile fd filepath openFds, Just currentCall)
                                          (_, Left (OpenCallUnfinished _ fd)) -> (openFds, Just currentCall)
                                          s -> error $ "Unhandled match: " ++ show s
                                )
                                (either openCallPid closeCallPid currentCall)
                                bookkeepingPerPid
                            allOpenFiles :: [FilePath]
                            allOpenFiles = filter (not . isSystemFile) $ Map.elems newOpenFds
                            numFilesOpenByCodd :: Int
                            numFilesOpenByCodd = length allOpenFiles
                         in if numFilesOpenByCodd > 1
                              then do
                                putStrLn
                                  "More than one simultaneously open migration or on-disk representation! Here's the strace log:"
                                forM_
                                  openAndCloseLines
                                  Text.putStrLn
                                error $
                                  "More than one file open simultaneously (" ++ show numFilesOpenByCodd ++ "). Test failed. strace output printed above.\nOpen files are: " ++ show allOpenFiles ++ "\nBookkeeping is: " ++ show newBookkeeping
                              else
                                pure (newBookkeeping, newOpenFds, atLeastOneFile || numFilesOpenByCodd > 0)
                      Left e ->
                        error $
                          "Found strace line that could not be parsed due to '"
                            ++ show e
                            ++ "': "
                            ++ show line
                )
                (mempty :: Map Pid (Either OpenCall CloseCall), mempty :: Map Fd FilePath, False)
                openAndCloseLines
            openFdsAtEnd `shouldBe` mempty -- Sanity check: every file opened by the process must be closed
            atLeastOneNonSystemFileOpened `shouldBe` True -- Sanity check: otherwise we might be stracing different processes

newtype Pid = Pid Int
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype Fd = Fd Int
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data OpenCall = OpenCallUnfinished FilePath Pid | OpenCallCompleted FilePath Pid Fd | OpenCallResumed Pid Fd
  deriving stock (Show)

data CloseCall = CloseCallUnfinished Pid Fd | CloseCallCompleted Pid Fd | CloseCallResumed Pid
  deriving stock (Show)

openCallPid :: OpenCall -> Pid
openCallPid = \case
  OpenCallUnfinished _ pid -> pid
  OpenCallCompleted _ pid _ -> pid
  OpenCallResumed pid _ -> pid

closeCallPid :: CloseCall -> Pid
closeCallPid = \case
  CloseCallUnfinished pid _ -> pid
  CloseCallCompleted pid _ -> pid
  CloseCallResumed pid -> pid

-- | Parses both glibc's `openat` and musl's `open` syscalls from a `strace -f -o` output line.
openParser :: P.Parser OpenCall
openParser =
  openResumedParser <|> do
    pid <- Pid <$> P.signed P.decimal
    P.skipWhile Char.isSpace
    void $ P.string "openat(AT_FDCWD, \"" <|> P.string "open(\"" <|> fail "Could not find openat or open call" -- glibc and musl use different syscalls. The former is glibc's and the latter is musl's.
    filepath <- Text.unpack <$> P.takeWhile1 ('"' /=)
    void $ P.string "\"," <|> fail "Missing comma"
    P.skipWhile Char.isSpace
    P.skipWhile (\c -> c /= ')' && c /= ' ') -- Skip all flags, such as O_RDONLY|O_NOCTTY|O_NONBLOCK and others
    finished <- Left <$> P.string " <unfinished ...>" <|> Right <$> P.string ")" <|> fail "No unfinished nor closing paren"
    case finished of
      Left _ -> pure $ OpenCallUnfinished filepath pid
      Right _ -> do
        P.skipWhile Char.isSpace
        void $ P.string "=" <|> fail "No equal sign"
        P.skipWhile Char.isSpace
        fd <- Fd <$> P.signed P.decimal
        pure $ OpenCallCompleted filepath pid fd
  where
    openResumedParser = do
      pid <- Pid <$> P.signed P.decimal
      P.skipWhile Char.isSpace
      void $ P.string "<... open resumed>)" <|> P.string "<... openat resumed>)"
      P.skipWhile Char.isSpace
      void $ P.string "="
      P.skipWhile Char.isSpace
      fd <- Fd <$> P.signed P.decimal
      pure $ OpenCallResumed pid fd

-- | Parses a `close` strace output line.
closeParser :: P.Parser CloseCall
closeParser =
  closeResumedParser <|> closeUnfinishedParser <|> do
    pid <- Pid <$> P.signed P.decimal
    P.skipWhile Char.isSpace
    void $ P.string "close("
    fd <- Fd <$> P.signed P.decimal
    P.skipWhile Char.isSpace
    void $ P.string ")"
    pure $ CloseCallCompleted pid fd
  where
    closeResumedParser = do
      pid <- Pid <$> P.signed P.decimal
      P.skipWhile Char.isSpace
      void $ P.string "<... close resumed>)"
      pure $ CloseCallResumed pid
    closeUnfinishedParser = do
      pid <- Pid <$> P.signed P.decimal
      P.skipWhile Char.isSpace
      void $ P.string "close("
      fd <- Fd <$> P.signed P.decimal
      void $ P.string " <unfinished ...>"
      pure $ CloseCallUnfinished pid fd
