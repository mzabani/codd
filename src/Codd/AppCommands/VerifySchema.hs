module Codd.AppCommands.VerifySchema
  ( verifySchema,
  )
where

import Codd.Environment (CoddSettings (..))
import Codd.Internal (withConnection)
import Codd.Logging
  ( CoddLogger,
    logInfo,
  )
import Codd.Query (NotInTxn, queryServerMajorAndFullVersion)
import Codd.Representations
  ( logSchemasComparison,
    readRepsFromDisk,
  )
import Codd.Representations.Database (readRepsFromDbWithNewTxn)
import Codd.Representations.Types (DbRep)
import Codd.Types (PgMajorVersion)
import Control.Monad (when)
import Data.Aeson (decode)
import Data.ByteString.Lazy (hGetContents)
import Data.Maybe (fromMaybe)
import Data.Time (secondsToDiffTime)
import System.Exit
  ( ExitCode (..),
    exitWith,
  )
import System.IO (hSetBinaryMode)
import UnliftIO
  ( Concurrently (..),
    MonadUnliftIO,
    liftIO,
    stdin,
  )

verifySchema ::
  (MonadUnliftIO m, CoddLogger m, NotInTxn m) =>
  CoddSettings ->
  Bool ->
  m ()
verifySchema dbInfoWithAllMigs@CoddSettings {migsConnString} fromStdin =
  do
    let dbInfoDontApplyAnything = dbInfoWithAllMigs {sqlMigrations = []}
    (dbSchema, expectedSchemas) <-
      withConnection
        migsConnString
        (secondsToDiffTime 5)
        ( \conn -> do
            (pgMajorVer, _) <- queryServerMajorAndFullVersion conn
            runConcurrently $ (,) <$> Concurrently (readRepsFromDbWithNewTxn dbInfoDontApplyAnything conn) <*> Concurrently (readExpectedSchema pgMajorVer dbInfoWithAllMigs fromStdin)
        )
    when (dbSchema /= expectedSchemas) $ do
      logSchemasComparison dbSchema expectedSchemas
      liftIO $ exitWith (ExitFailure 1)
    logInfo "Actual and expected schemas <GREEN>match</GREEN>"

readExpectedSchema :: (MonadUnliftIO m) => PgMajorVersion -> CoddSettings -> Bool -> m DbRep
readExpectedSchema pgMajorVer CoddSettings {onDiskReps} fromStdin =
  if fromStdin
    then do
      liftIO $ hSetBinaryMode stdin True
      inputs <- liftIO $ hGetContents stdin
      pure
        $ fromMaybe
          ( error
              "Could not decode the JSON input as a DB-schema representation. Make sure it is the output of 'codd write-schema --to-stdout' and that the versions of codd are exactly the same."
          )
        $ decode inputs
    else either (readRepsFromDisk pgMajorVer) pure onDiskReps
