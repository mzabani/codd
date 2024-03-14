module SystemResourcesSpecs.OpenFilesSpec where

import           Codd                           ( VerifySchemas(..)
                                                , applyMigrations
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Logging                   ( runCoddLogger )
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( foldM
                                                , forM_
                                                , void
                                                )
import           Control.Monad.Trans.Resource   ( MonadThrow(..) )
import qualified Data.Attoparsec.Text          as P
import qualified Data.Char                     as Char
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           DbUtils                        ( aroundFreshDatabase
                                                , testConnTimeout
                                                )
import           Test.Hspec
import           UnliftIO                       ( SomeException
                                                , try
                                                )

spec :: Spec
spec = do
    describe "SystemResourcesSpecs" $ do
        aroundFreshDatabase
            $ it
                  "RUNNING - At most one .sql migration file and one on-disk representation file open at a time"
            $ \emptyTestDbInfo -> do
                  void @IO $ runCoddLogger $ applyMigrations
                      emptyTestDbInfo
                          { sqlMigrations =
                              ["test/migrations/open-files-limit/"]
                          , onDiskReps    = Left "./expected-schema"
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
                      try $ Text.readFile
                          "/tmp/strace-codd-system-resources-test.log"
                  case contentsE of
                      Left (ex :: SomeException) -> do
                          putStrLn
                              "Error reading /tmp/strace-codd-system-resources-test.log. Are you running this with the runfile target or are you running this test directly? This test needs to run after a very specific `strace` wrapped command that you'll find in our Runfile test targets, or it doesn't work."
                          throwM ex
                      Right contents -> do
                          let openAndCloseLines =
                                  filter
                                          (\l ->
                                              -- We test both migrations and on-disk representations
                                              "migrations/open-files-limit"
                                                  `Text.isInfixOf` l
                                                  || "expected-schema/"
                                                  `Text.isInfixOf` l
                                                  ||               "close("
                                                  `Text.isInfixOf` l
                                          )
                                      $ Text.lines contents
                          -- forM_ (Text.lines contents) Text.putStrLn
                          (openFilesAtEnd, atLeastOneMigrationWasOpened) <-
                              foldM
                                  (\(openFiles, atLeastOneMig) line -> do
                                      case P.parseOnly openParser line of
                                          Right (fp, fd) ->
                                              if Map.size openFiles > 0
                                                  then do
                                                      putStrLn
                                                          "More than one simultaneously open migration or on-disk representation! Here's the strace log:"
                                                      forM_
                                                          openAndCloseLines
                                                          Text.putStrLn
                                                      error
                                                          "More than one file open simultaneously. Test failed."
                                                  else do
                                                      pure
                                                          ( Map.insert
                                                              fd
                                                              fp
                                                              openFiles
                                                          , True
                                                          )
                                          Left _ -> do
                                              case
                                                      P.parseOnly
                                                          closeParser
                                                          line
                                                  of
                                                      Left e ->
                                                          error
                                                              $ "Found strace line that could not be parsed due to '"
                                                              ++ show e
                                                              ++ "': "
                                                              ++ show line
                                                      Right fd ->
                                                          pure
                                                              ( Map.delete
                                                                  fd
                                                                  openFiles
                                                              , atLeastOneMig
                                                              )
                                  )
                                  (Map.empty :: Map.Map Int FilePath, False)
                                  openAndCloseLines
                          openFilesAtEnd `shouldBe` Map.empty
                          atLeastOneMigrationWasOpened `shouldBe` True -- Otherwise we might be stracing different processes. This is a good sanity check.

-- | Parses both glibc's `openat` and musl's `open` syscalls from a `strace -f -o` output line and returns the opened file and file descriptor.
openParser :: P.Parser (FilePath, Int)
openParser = do
    void $ P.decimal @Int
    P.skipWhile Char.isSpace
    void $ P.string "openat(AT_FDCWD, \"" <|> P.string "open(\"" -- glibc and musl use different syscalls. The former is glibc's and the latter is musl's.
    fp <- P.takeWhile1 ('"' /=)
    void $ P.string "\","
    P.skipWhile Char.isSpace
    P.skipWhile (')' /=) -- Skip all flags, such as O_RDONLY|O_NOCTTY|O_NONBLOCK and others
    void $ P.string ")"
    P.skipWhile Char.isSpace
    void $ P.string "="
    P.skipWhile Char.isSpace
    fd <- P.decimal @Int
    pure (Text.unpack fp, fd)


-- | Parses a `close` strace output line and returns the closed file descriptor.
closeParser :: P.Parser Int
closeParser = do
    void $ P.decimal @Int
    P.skipWhile Char.isSpace
    void $ P.string "close("
    fd <- P.decimal
    void $ P.string ")"
    pure fd
