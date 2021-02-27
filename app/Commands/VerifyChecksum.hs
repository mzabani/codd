module Commands.VerifyChecksum
  ( verifyChecksum
  ) where

import           Codd.Environment               ( CoddSettings(..)
                                                , superUserInAppDatabaseConnInfo
                                                )
import           Codd.Hashing                   ( hashDifferences
                                                , readHashesFromDatabaseWithSettings
                                                , readHashesFromDisk
                                                )
import           Codd.Internal                  ( withConnection )
import           Control.Monad                  ( when )
import           Control.Monad.Logger           ( MonadLoggerIO
                                                , logErrorN
                                                , logInfoN
                                                )
import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Text.Encoding             ( decodeUtf8 )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           UnliftIO                       ( MonadUnliftIO
                                                , liftIO
                                                )

verifyChecksum :: (MonadUnliftIO m, MonadLoggerIO m) => CoddSettings -> m ()
verifyChecksum dbInfoWithAllMigs@CoddSettings { onDiskHashes } = do
  let dbInfoDontApplyAnything = dbInfoWithAllMigs { sqlMigrations = Right [] }
  let adminConnInfo = superUserInAppDatabaseConnInfo dbInfoDontApplyAnything
  onDiskHashesDir <- either
    pure
    (error
      "This functionality needs a directory to read checksums from. Report this as a bug."
    )
    onDiskHashes
  dbHashes <- withConnection
    adminConnInfo
    (readHashesFromDatabaseWithSettings dbInfoDontApplyAnything)
  diskHashes <- readHashesFromDisk onDiskHashesDir
  when (dbHashes /= diskHashes) $ do
    -- Urgh.. UTF-8 Text as output from Aeson would be perfect here..
    logErrorN $ decodeUtf8 $ toStrict $ encode $ hashDifferences dbHashes
                                                                 diskHashes
    logErrorN
      "DB and on-disk checksums do not match. Differences right above this message. Left is Database, Right is on-disk."
    logErrorN
      "Some names will not be strictly the names of Database objects, because codd may add extra characters and strings due to DB name overloading."
    liftIO $ exitWith (ExitFailure 1)
  logInfoN "DB and on-disk checksums match."
