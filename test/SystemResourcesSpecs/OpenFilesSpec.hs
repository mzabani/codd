module SystemResourcesSpecs.OpenFilesSpec where

import           Codd                           ( VerifySchemas(..)
                                                , applyMigrations
                                                , applyMigrationsNoCheck
                                                )
import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( baseApplyMigsBlock
                                                , collectAndApplyMigrations
                                                , withConnection
                                                )
import           Codd.Internal.MultiQueryStatement
                                                ( SqlStatementException )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                , hoistAddedSqlMigration
                                                )
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Representations           ( readRepresentationsFromDbWithSettings
                                                )
import           Codd.Representations.Types     ( DbRep(..) )
import           Codd.Types                     ( RetryBackoffPolicy(..)
                                                , RetryPolicy(..)
                                                , TxnIsolationLvl(..)
                                                )
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( foldM
                                                , forM_
                                                , void
                                                , when
                                                )
import           Control.Monad.Logger           ( LogStr
                                                , LoggingT(runLoggingT)
                                                , fromLogStr
                                                , runStdoutLoggingT
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Resource   ( MonadThrow(..) )
import qualified Data.Aeson                    as Aeson
import qualified Data.Attoparsec.Text          as P
import qualified Data.Char                     as Char
import           Data.Functor                   ( (<&>) )
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( UTCTime
                                                , diffUTCTime
                                                , secondsToDiffTime
                                                , secondsToNominalDiffTime
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           DbUtils                        ( aroundFreshDatabase
                                                , createTestUserMig
                                                , createTestUserMigPol
                                                , finallyDrop
                                                , fixMigsOrder
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                , shouldBeStrictlySortedOn
                                                , testCoddSettings
                                                , testConnInfo
                                                , testConnTimeout
                                                )
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.QuickCheck
import qualified Test.QuickCheck               as QC
import           UnliftIO                       ( MonadIO
                                                , SomeException
                                                , liftIO
                                                , stdout
                                                , try
                                                )
import           UnliftIO.Concurrent            ( MVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )

spec :: Spec
spec = do
    describe "SystemResourcesSpecs" $ do
        describe "Open files limit" $ aroundFreshDatabase $ do
            it
                    "At most one .sql migration file and one on-disk representation file open at a time"
                $ \emptyTestDbInfo -> do
                      void @IO $ runStdoutLoggingT $ applyMigrations
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
                      contentsE <-
                          try $ Text.readFile
                              "/tmp/strace-codd-system-resources-test.log"
                      case contentsE of
                          Left (ex :: SomeException) -> do
                              putStrLn
                                  "Error reading /tmp/strace-codd-system-resources-test.log. Are you running this with the runfile target or are you running this test directly? This test needs to run under a very specific `strace` command that you'll find in our Runfile test targets, or it doesn't work."
                              throwM ex
                          Right contents -> do
                              let
                                  openAndCloseLines =
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
                                                              -- pure
                                                              --     ( openFiles
                                                              --     , atLeastOneMig
                                                              --     )
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
    void P.decimal
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
    fd <- P.decimal
    pure (Text.unpack fp, fd)


-- | Parses a `close` strace output line and returns the closed file descriptor.
closeParser :: P.Parser Int
closeParser = do
    void P.decimal
    P.skipWhile Char.isSpace
    void $ P.string "close("
    fd <- P.decimal
    void $ P.string ")"
    pure fd
