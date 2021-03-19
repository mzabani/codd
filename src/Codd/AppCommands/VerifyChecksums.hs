module Codd.AppCommands.VerifyChecksums
  ( verifyChecksums
  ) where

import           Codd.Environment               ( CoddSettings(..)
                                                , superUserInAppDatabaseConnInfo
                                                )
import           Codd.Hashing                   ( logHashDifferences
                                                , readHashesFromDatabaseWithSettings
                                                , readHashesFromDisk
                                                )
import           Codd.Hashing.Types             ( DbHashes )
import           Codd.Internal                  ( withConnection )
import           Control.Monad                  ( when )
import           Control.Monad.Logger           ( MonadLoggerIO
                                                , logInfoN
                                                )
import           Data.Aeson                     ( decode )
import           Data.ByteString.Lazy           ( hGetContents )
import           Data.Maybe                     ( fromMaybe )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.IO                      ( hSetBinaryMode )
import           UnliftIO                       ( MonadUnliftIO
                                                , liftIO
                                                , stdin
                                                )

verifyChecksums
  :: (MonadUnliftIO m, MonadLoggerIO m) => CoddSettings -> Bool -> m ()
verifyChecksums dbInfoWithAllMigs@CoddSettings { onDiskHashes } fromStdin = do
  let dbInfoDontApplyAnything = dbInfoWithAllMigs { sqlMigrations = Right [] }
      adminConnInfo = superUserInAppDatabaseConnInfo dbInfoDontApplyAnything
  expectedChecksums :: DbHashes <- if fromStdin
    then do
      liftIO $ hSetBinaryMode stdin True
      inputs <- liftIO $ hGetContents stdin
      pure
        $ fromMaybe
            (error
              "Could not decode the JSON input as a DB-checksum representation. Make sure it is the output of 'codd write-checksums --to-stdout' and that the versions of codd are exactly the same."
            )
        $ decode inputs
    else either readHashesFromDisk pure onDiskHashes
  dbHashes <- withConnection
    adminConnInfo
    (readHashesFromDatabaseWithSettings dbInfoDontApplyAnything)
  when (dbHashes /= expectedChecksums) $ do
    logHashDifferences dbHashes expectedChecksums
    liftIO $ exitWith (ExitFailure 1)
  logInfoN "Database and expected checksums match."
