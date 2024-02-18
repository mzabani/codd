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
import           Control.Monad                  ( foldM_
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
import           UnliftIO.Directory             ( removeFile )

spec :: Spec
spec = do
    describe "SystemResourcesSpecs" $ do
        describe "Open files limit" $ aroundFreshDatabase $ do
            it "At most one .sql migration file open at a time"
                $ \emptyTestDbInfo -> do
                      -- This test doesn't assert anything. It must run with `strace` and then the output of that
                      -- will inspect the number of concurrently open .sql files
                      void @IO $ runStdoutLoggingT $ applyMigrationsNoCheck
                          emptyTestDbInfo
                              { sqlMigrations =
                                  ["test/migrations/open-files-limit/"]
                              }
                          Nothing
                          testConnTimeout
                          (const $ pure ())
                      contentsE <- try $ Text.readFile "/tmp/strace.log"
                      case contentsE of
                          Left (ex :: SomeException) -> do
                              putStrLn
                                  "Error reading /tmp/strace.log. Are you running this with the runfile target or are you running this test directly? This test needs to run under a very specific `strace` command that you'll find in our Runfile test targets, or it doesn't work."
                              throwM ex
                          Right contents -> do
                              removeFile "/tmp/strace.log"
                              -- TODO: also test on-disk representations
                              let
                                  openAndCloseLines =
                                      filter
                                              (\l ->
                                                  "openat("
                                                      `Text.isInfixOf` l
                                                      && "migrations/open-files-limit"
                                                      `Text.isInfixOf` l
                                                      ||               "close("
                                                      `Text.isInfixOf` l
                                              )
                                          $ Text.lines contents
                              forM_ openAndCloseLines Text.putStrLn
                              openFilesAtEnd <- foldM
                                  (\openFiles line -> do
                                      case P.parseOnly openatParser line of
                                          Right (fp, fd) ->
                                              if Map.size openFiles > 1
                                                  then
                                                      error
                                                          "More than one open migration!"
                                                  else pure $ Map.insert
                                                      fd
                                                      fp
                                                      openFiles
                                          Left _ ->
                                              case
                                                      P.parseOnly
                                                          closeParser
                                                          line
                                                  of
                                                      Left _ -> pure openFiles
                                                      Right fd ->
                                                          pure $ Map.delete
                                                              fd
                                                              openFiles
                                  )
                                  (Map.empty :: Map.Map Int FilePath)
                                  openAndCloseLines
                              Map.size openFilesAtEnd `shouldBe` 0
                      pure ()

openatParser :: P.Parser (FilePath, Int)
openatParser = do
    void $ P.string "[pid "
    void P.decimal
    void $ P.string "] openat(AT_FDCWD, \""
    fp <- P.takeTill ('"' ==)
    void $ P.string ", O_RDONLY|O_NOCTTY|O_NONBLOCK) = "
    fd <- P.decimal
    pure (Text.unpack fp, fd)


closeParser :: P.Parser Int
closeParser = do
    void $ P.string "[pid "
    void P.decimal
    void $ P.string "] close("
    fd <- P.decimal
    void $ P.string ")"
    pure fd
