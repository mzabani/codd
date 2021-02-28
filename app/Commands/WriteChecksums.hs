module Commands.WriteChecksums
  ( WriteChecksumsOpts(..)
  , writeChecksums
  ) where

import           Codd.Environment               ( CoddSettings )
import qualified Codd.Environment              as Codd
import qualified Codd.Hashing                  as Codd
import qualified Codd.Internal                 as Codd
import           Control.Monad.IO.Unlift        ( MonadIO(..)
                                                , MonadUnliftIO
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.IO                  as Text
import           Types                          ( runErrorsOnlyLogger )
data WriteChecksumsOpts = WriteToStdout | WriteToDisk (Maybe FilePath)

writeChecksums
  :: (MonadUnliftIO m, MonadIO m) => CoddSettings -> WriteChecksumsOpts -> m ()
writeChecksums dbInfo opts = case opts of
  WriteToDisk mdest -> runStdoutLoggingT $ do
    checksum <- Codd.withConnection
      (Codd.superUserInAppDatabaseConnInfo dbInfo)
      (Codd.readHashesFromDatabaseWithSettings dbInfo)
    let
      dirToSave = case mdest of
        Just d  -> d
        Nothing -> case Codd.onDiskHashes dbInfo of
          Right _ ->
            error
              "This functionality needs a directory to write checksum to. Report this as a bug."
          Left d -> d

    Codd.persistHashesToDisk checksum dirToSave
  WriteToStdout -> runErrorsOnlyLogger $ do
    checksums <- Codd.withConnection
      (Codd.superUserInAppDatabaseConnInfo dbInfo)
      (Codd.readHashesFromDatabaseWithSettings dbInfo)

    -- Urgh.. UTF-8 Text as output from Aeson would be perfect here..
    liftIO $ Text.putStr $ decodeUtf8 $ toStrict $ encode checksums
