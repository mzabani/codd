module Codd.AppCommands.WriteSchema
    ( WriteSchemaOpts(..)
    , writeSchema
    ) where

import           Codd.Environment               ( CoddSettings(..) )
import qualified Codd.Environment              as Codd
import qualified Codd.Internal                 as Codd
import           Codd.Logging                   ( Verbosity(..)
                                                , runCoddLogger
                                                , runErrorsOnlyLogger
                                                )
import           Codd.Query                     ( NotInTxn )
import qualified Codd.Representations          as Codd
import           Codd.Representations           ( detEncodeJSON )
import           Codd.Representations.Database  ( readRepsFromDbWithNewTxn )
import           Control.Monad.IO.Unlift        ( MonadIO(..)
                                                , MonadUnliftIO
                                                )
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( secondsToDiffTime )

data WriteSchemaOpts = WriteToStdout | WriteToDisk (Maybe FilePath)

writeSchema
    :: (MonadUnliftIO m, NotInTxn m) => CoddSettings -> WriteSchemaOpts -> m ()
writeSchema dbInfo@CoddSettings { migsConnString } opts = case opts of
    WriteToDisk mdest -> runCoddLogger Verbose $ do
        dbSchema <- Codd.withConnection migsConnString
                                        (secondsToDiffTime 5)
                                        (readRepsFromDbWithNewTxn dbInfo)
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
        dbSchema <- Codd.withConnection migsConnString
                                        (secondsToDiffTime 5)
                                        (readRepsFromDbWithNewTxn dbInfo)

        liftIO $ Text.putStr $ detEncodeJSON dbSchema

