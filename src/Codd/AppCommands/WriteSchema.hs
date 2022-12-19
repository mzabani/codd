module Codd.AppCommands.WriteSchema
  ( WriteSchemaOpts(..)
  , writeSchema
  ) where

import           Codd.Environment               ( CoddSettings(..) )
import qualified Codd.Environment              as Codd
import qualified Codd.Internal                 as Codd
import           Codd.Logging                   ( runErrorsOnlyLogger )
import qualified Codd.Representations          as Codd
import           Codd.Representations           ( detEncodeJSON )
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
    dbSchema <- Codd.withConnection
      migsConnString
      (secondsToDiffTime 5)
      (Codd.readRepresentationsFromDbWithSettings dbInfo)
    let
      dirToSave = case mdest of
        Just d  -> d
        Nothing -> case Codd.onDiskReps dbInfo of
          Right _ ->
            error
              "This functionality needs a directory to write representations to. Report this as a bug."
          Left d -> d

    Codd.persistRepsToDisk dbSchema dirToSave
  WriteToStdout -> runErrorsOnlyLogger $ do
    dbSchema <- Codd.withConnection
      migsConnString
      (secondsToDiffTime 5)
      (Codd.readRepresentationsFromDbWithSettings dbInfo)

    liftIO $ Text.putStr $ detEncodeJSON dbSchema
