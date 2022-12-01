module Codd.AppCommands.WriteSchema
  ( WriteSchemaOpts(..)
  , writeSchema
  ) where

import           Codd.Environment               ( CoddSettings(..) )
import qualified Codd.Environment              as Codd
import qualified Codd.Hashing                  as Codd
import           Codd.Hashing                   ( detEncodeJSON )
import qualified Codd.Internal                 as Codd
import           Codd.Logging                   ( runErrorsOnlyLogger )
import           Control.Monad.IO.Unlift        ( MonadIO(..)
                                                , MonadUnliftIO
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( secondsToDiffTime )

data WriteSchemaOpts = WriteToStdout | WriteToDisk (Maybe FilePath)

writeSchema
  :: (MonadUnliftIO m, MonadIO m) => CoddSettings -> WriteSchemaOpts -> m ()
writeSchema dbInfo@CoddSettings { migsConnString } opts = case opts of
  WriteToDisk mdest -> runStdoutLoggingT $ do
    checksum <- Codd.withConnection
      migsConnString
      (secondsToDiffTime 5)
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
      migsConnString
      (secondsToDiffTime 5)
      (Codd.readHashesFromDatabaseWithSettings dbInfo)

    liftIO $ Text.putStr $ detEncodeJSON checksums
