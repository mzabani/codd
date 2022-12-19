module Codd.AppCommands.VerifySchema
  ( verifySchema
  ) where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( withConnection )
import           Codd.Representations           ( logSchemasComparison
                                                , readRepresentationsFromDbWithSettings
                                                , readRepsFromDisk
                                                )
import           Codd.Representations.Types     ( DbRep )
import           Control.Monad                  ( when )
import           Control.Monad.Logger           ( MonadLoggerIO
                                                , logInfoN
                                                )
import           Data.Aeson                     ( decode )
import           Data.ByteString.Lazy           ( hGetContents )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time                      ( secondsToDiffTime )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.IO                      ( hSetBinaryMode )
import           UnliftIO                       ( MonadUnliftIO
                                                , liftIO
                                                , stdin
                                                )

verifySchema
  :: (MonadUnliftIO m, MonadLoggerIO m) => CoddSettings -> Bool -> m ()
verifySchema dbInfoWithAllMigs@CoddSettings { onDiskReps, migsConnString } fromStdin
  = do
    let dbInfoDontApplyAnything = dbInfoWithAllMigs { sqlMigrations = [] }
    expectedSchemas :: DbRep <- if fromStdin
      then do
        liftIO $ hSetBinaryMode stdin True
        inputs <- liftIO $ hGetContents stdin
        pure
          $ fromMaybe
              (error
                "Could not decode the JSON input as a DB-schema representation. Make sure it is the output of 'codd write-schema --to-stdout' and that the versions of codd are exactly the same."
              )
          $ decode inputs
      else either readRepsFromDisk pure onDiskReps
    dbSchema <- withConnection
      migsConnString
      (secondsToDiffTime 5)
      (readRepresentationsFromDbWithSettings dbInfoDontApplyAnything)
    when (dbSchema /= expectedSchemas) $ do
      logSchemasComparison dbSchema expectedSchemas
      liftIO $ exitWith (ExitFailure 1)
    logInfoN "Database and expected schemas match."
