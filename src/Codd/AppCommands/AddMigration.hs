{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Codd.AppCommands.AddMigration
  ( addMigration,
  )
where

import qualified Codd
import Codd.Analysis
  ( MigrationCheck (..),
    checkMigration,
  )
import Codd.Environment (CoddSettings (..))
import Codd.Internal
  ( CoddSchemaVersion (..),
    closeFileStream,
    collectPendingMigrations,
    delayedOpenStreamFile,
    detectCoddSchema,
    listMigrationsFromDisk,
    pendingMigs,
    withConnection,
  )
import Codd.Logging
  ( CoddLogger,
    logError,
    logInfoAlways,
    runWithoutLogging,
  )
import Codd.Parsing
  ( EnvVars,
    FileStream (..),
    migrationCustomConnInfo,
    parseSqlMigration,
    toMigrationTimestamp,
  )
import Codd.Query (NotInTxn, query, queryMay, queryServerMajorAndFullVersion)
import Codd.Representations
  ( persistRepsToDisk,
    readRepresentationsFromDbWithSettings,
  )
import Codd.Types (ConnectionString (..), SqlFilePath (..))
import Control.Monad
  ( unless,
    void,
    when,
  )
import Control.Monad.Trans.Resource (MonadThrow)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime, secondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Notification as DB
import qualified Database.PostgreSQL.Simple.Time as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import Streaming (Of ((:>)))
import qualified Streaming
import System.Exit
  ( ExitCode (..),
    exitWith,
  )
import System.FilePath (takeFileName, (</>))
import UnliftIO
  ( IOMode (WriteMode),
    MonadIO,
    MonadUnliftIO,
    liftIO,
    withFile,
  )
import UnliftIO.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    removeDirectory,
    removeFile,
  )
import UnliftIO.Exception
  ( SomeException,
    try,
  )
import UnliftIO.Resource (MonadResource, runResourceT)

-- | Returns the file path of the timestamped and added migration, already in the SQL migrations folder.
addMigration ::
  forall m.
  (MonadUnliftIO m, CoddLogger m, MonadThrow m, Codd.Parsing.EnvVars m, NotInTxn m) =>
  CoddSettings ->
  Maybe FilePath ->
  SqlFilePath ->
  m FilePath
addMigration dbInfo@Codd.CoddSettings {onDiskReps, migsConnString = defaultConnInfo, sqlMigrations, txnIsolationLvl} destFolder sqlFp@(SqlFilePath fp) =
  do
    finalDir <- case (destFolder, sqlMigrations) of
      (Just f, _) -> pure f
      (Nothing, []) ->
        error
          "Please specify '--dest-folder' or add at least one path to the CODD_MIGRATION_DIRS environment variable."
      (Nothing, f : _) -> pure f
    onDiskRepsDir <-
      either
        pure
        ( error
            "This functionality needs a directory to write the expected representations to. Please report this as a bug in codd."
        )
        onDiskReps

    migFileExists <- doesFileExist fp
    unless migFileExists $ do
      logError $
        "Could not find migration file \""
          <> Text.pack fp
          <> "\""
      liftIO $ exitWith $ ExitFailure 99

    finalDirExists <- doesDirectoryExist finalDir
    unless finalDirExists $ do
      logError $
        "Could not find destination directory \""
          <> Text.pack finalDir
          <> "\""
      liftIO $ exitWith $ ExitFailure 98

    expectedSchemaDirExists <- doesDirectoryExist onDiskRepsDir
    unless expectedSchemaDirExists $ do
      logError $
        "Could not find directory for expected DB schema representation \""
          <> Text.pack onDiskRepsDir
          <> "\". You should create it for codd to write schema representations to"
      liftIO $ exitWith $ ExitFailure 97

    isFirstMigration <- null <$> listMigrationsFromDisk sqlMigrations []
    runResourceT $ do
      migStream <- delayedOpenStreamFile fp
      parsedSqlMigE <- Codd.Parsing.parseSqlMigration (takeFileName fp) migStream
      case parsedSqlMigE of
        Left err -> do
          logError $ "Could not add migration: " <> Text.pack err
          when
            isFirstMigration
            (printSuggestedFirstMigration defaultConnInfo)
          liftIO $ exitWith $ ExitFailure 96
        Right sqlMig -> do
          let newMigrationConnInfo = fromMaybe defaultConnInfo $ migrationCustomConnInfo sqlMig
          migCheck <- checkMigration sqlMig
          let migError = case migCheck of
                Left err -> Just err
                Right mc -> transactionManagementProblem mc

          case migError of
            Just err -> do
              logError $ "Could not add migration: " <> err
              when
                isFirstMigration
                (printSuggestedFirstMigration defaultConnInfo)
              liftIO $ exitWith $ ExitFailure 95
            Nothing -> do
              -- File stream derived bindings should be consumed linearly. We shadow previous bindings
              -- as a poor replacement of that.
              -- We copy the migration to a temporary directory so interrupting `codd add` doesn't leave
              -- a non-applied migration in a folder in CODD_MIGRATION_DIRS
              closeFileStream migStream
              tempDir <- getEmptyTempDir
              (tempDirMigFile, _) <- getTimestampedAbsoluteDestMigrationPath sqlFp tempDir
              copyFile fp tempDirMigFile

              let connTimeout = secondsToDiffTime 10
              ePendingMigsBeforeAdd <- try @_ @SomeException $ runWithoutLogging $ collectPendingMigrations defaultConnInfo (Left sqlMigrations) txnIsolationLvl connTimeout
              case List.null . pendingMigs <$> ePendingMigsBeforeAdd of
                Right False -> do
                  logInfoAlways "Applying pending migrations before the one you are adding.."
                  Codd.applyMigrationsNoCheck
                    dbInfo
                    Nothing
                    connTimeout
                    (const $ pure ())
                _ -> pure () -- Here we assume no connectivity, but we want to proceed to print a better error message
              addE <- try $ do
                -- Any codd.* functions invoked by the added migration will call NOTIFY and
                -- we'll know if the codd schema is then required.
                -- This seems overkill, but:
                -- 1) The user doesn't need to worry about anything, i.e. "it just works".
                -- 2) Migrations can contain arbitrary statements, including "RESET ALL" and "UNLISTEN *",
                --    making custom settings and listening on the same connection difficult.
                -- 3) Migrations might be no-txn, making "ON COMMIT DROP" tables difficult to use and normal
                --    tables difficult to clean up.
                -- 4) Migrations might begin a new job and synchronously finalize it, then DELETE from
                --    codd._background_jobs all in the same txn, so querying `codd.jobs` before and after the
                --    migration is not sufficient.
                -- 5) A better approach would be to parse all SQL in the migration properly and then detect calls
                --    to codd.* functions, but that's not an implementation I can afford right now.
                --    In fact, a migration SELECTing from codd.jobs without invoking any function will fail to
                --    be detected with the current approach.. oh, well.
                ((pgMajorVer, databaseSchemas), addedMigRequiresCoddSchema, newlyStartedBackgroundJobs) <-
                  withConnection newMigrationConnInfo connTimeout $ \listenConn -> do
                    liftIO $ void $ DB.execute listenConn "LISTEN ?" (DB.Only $ DB.Identifier "codd.___require_codd_schema_channel")
                    maxBackgroundJobIdBefore <- getMaxBackgroundJobId listenConn
                    pgVerAndSchemas <-
                      Codd.applyMigrationsNoCheck
                        dbInfo {sqlMigrations = [tempDir]}
                        Nothing
                        connTimeout
                        ( \conn -> do
                            (pgMajorVer, _) <- queryServerMajorAndFullVersion conn
                            (pgMajorVer,) <$> readRepresentationsFromDbWithSettings dbInfo conn
                        )
                    addedMigRequiresCoddSchema <- liftIO $ checkCoddSchemaFunctionsHaveBeenCalled listenConn
                    newlyStartedJobs <- getAddedJobsStillRunning listenConn maxBackgroundJobIdBefore
                    closeFileStream migStream
                    pure (pgVerAndSchemas, addedMigRequiresCoddSchema, newlyStartedJobs)
                persistRepsToDisk pgMajorVer databaseSchemas onDiskRepsDir

                -- Copy file to target and prepend a special -- codd: requires-codd-schema line when necessary
                let finalMigFile = finalDir </> takeFileName tempDirMigFile
                writeFinalMigrationFile tempDirMigFile finalMigFile addedMigRequiresCoddSchema

                logInfoAlways $
                  "New migration applied and added to "
                    <> Text.pack finalMigFile
                logInfoAlways $
                  "Updated expected DB schema representations in the <MAGENTA>"
                    <> Text.pack onDiskRepsDir
                    <> "</MAGENTA> folder"
                printTipForBackgroundJobs newlyStartedBackgroundJobs

                fileRemoved <- try $ removeFile fp
                case fileRemoved of
                  Right _ -> pure ()
                  Left (_ :: SomeException) ->
                    logError $
                      "Could not remove "
                        <> Text.pack (show sqlFp)
                        <> ", but it has been added successfully so you can remove it yourself if you wish."
                pure finalMigFile
              void $ try @_ @SomeException $ removeDirectory tempDir -- Disk cleanup
              case addE of
                Right finalMigFile -> pure finalMigFile
                Left (e :: SomeException) -> do
                  logError $ "Could not add migration: " <> Text.pack (show e)

                  when
                    isFirstMigration
                    (printSuggestedFirstMigration defaultConnInfo >> liftIO (exitWith (ExitFailure 113)))

                  liftIO $ exitWith $ ExitFailure 1

-- | Writes to the provided file path prepending the special "-- codd: requires-codd-schema"
-- top-level comment if necessary, copying the remainder of the migration next.
writeFinalMigrationFile :: (MonadUnliftIO m, MonadResource m) => FilePath -> FilePath -> Bool -> m ()
writeFinalMigrationFile originalMigFile finalMigFile addedMigRequiresCoddSchema = withFile finalMigFile WriteMode $ \handle -> do
  migStream@Codd.Parsing.FileStream {fileStream} <- delayedOpenStreamFile originalMigFile
  when addedMigRequiresCoddSchema $ liftIO $ do
    Text.hPutStrLn handle "-- codd: requires-codd-schema"
    Text.hPutStrLn handle "-- Comment above added automatically by codd since this migration requires the 'codd' schema to exist. Please don't remove it. You can add more '-- codd:' top-level comments at the top of the file or even below this line. You can also remove this comment as it's purely instructive."
  Streaming.mapsM_ (\(chunk :> x) -> liftIO $ Text.hPutStr handle chunk >> pure x) fileStream
  closeFileStream migStream

printSuggestedFirstMigration :: (CoddLogger m, Monad m) => ConnectionString -> m ()
printSuggestedFirstMigration ConnectionString {database, user} = do
  logInfoAlways "---------------------------------------------"
  logInfoAlways $
    "\n<GREEN>Tip:</GREEN> It looks like this is your first migration. Make sure either the target database of your default connection string already exists, or add a migration that creates your database with a custom connection string. Example:\n\
    \\n<MAGENTA>\
    \    -- codd: no-txn\n\
    \    -- codd-connection: dbname=postgres user=postgres host=localhost\n\
    \    -- Make sure the connection string above works, or change it to one that works.\n\
    \    CREATE DATABASE \""
      <> Text.pack database
      <> "\" OWNER \""
      <> Text.pack user
      <> "\";\n\
         \    -- Also make sure the DB above doesn't exist yet, and that the DB owner does.\n\
         \</MAGENTA>\n\
         \- The migration above looks scary, but it's one of the rare few that will require anything other than plain SQL.\n\
         \- If this is what you need, feel free to copy the migration above into a .sql file, modify it accordingly and add that as your first migration.\n\
         \- If the above doesn't work, you want a more complete example or want to know more, make sure to read https://github.com/mzabani/codd/blob/master/docs/BOOTSTRAPPING.md for more on bootstrapping your database with codd.\n"

-- | Returns the absolute path a .sql file will be added to, including its timestamp,
-- e.g. /home/someuser/someproject/sql-migrations/2025-05-10-18-11-43-some-migration.sql
-- Does not copy or touch the file or disk.
getTimestampedAbsoluteDestMigrationPath ::
  (MonadIO m) => SqlFilePath -> FilePath -> m (FilePath, DB.UTCTimestamp)
getTimestampedAbsoluteDestMigrationPath (unSqlFilePath -> migrationPath) folderToCopyTo =
  do
    -- The only important invariants for naming SQL migrations are:
    --   1. Migrations added by a developer is such that it should come after all existing migrations on disk
    --   2. Chance of naming conflicts with migrations added by other developers is small.
    -- One desirable property, however, is that filenames are human-readable
    -- and convey more or less an idea of when they were added.
    (migTimestamp, dbTimestamp) <- Codd.Parsing.toMigrationTimestamp <$> liftIO getCurrentTime
    let finalName =
          folderToCopyTo
            </> nicerTimestampFormat (iso8601Show migTimestamp)
            ++ "-"
            ++ takeFileName migrationPath
    pure (finalName, dbTimestamp)
  where
    -- Replaces 'T' and colons by a dash and removes 'Z' from UTC timestamps.
    -- This makes them compatible with NTFS and nicer to work with in bash too.
    nicerTimestampFormat =
      Text.unpack
        . Text.replace ":" "-"
        . Text.replace "T" "-"
        . Text.replace "Z" ""
        . Text.pack

getEmptyTempDir :: (MonadIO m) => m FilePath
getEmptyTempDir = do
  tmp <- getTemporaryDirectory
  complement :: UUID <- liftIO UUIDv4.nextRandom
  let emptyDir = tmp </> UUID.toString complement
  createDirectoryIfMissing True emptyDir
  pure emptyDir

checkCoddSchemaFunctionsHaveBeenCalled :: DB.Connection -> IO Bool
checkCoddSchemaFunctionsHaveBeenCalled listenConn = go
  where
    go = do
      mNotif <- DB.getNotificationNonBlocking listenConn
      case mNotif of
        Nothing -> pure False
        Just DB.Notification {notificationChannel} -> if notificationChannel == "codd.___require_codd_schema_channel" then pure True else go

getMaxBackgroundJobId :: (MonadIO m) => DB.Connection -> m (Maybe Int)
getMaxBackgroundJobId conn = do
  coddSchemaVersion <- detectCoddSchema conn
  if coddSchemaVersion < CoddSchemaV5
    then pure Nothing
    else fmap DB.fromOnly <$> queryMay conn "SELECT jobid FROM codd._background_jobs ORDER BY jobid DESC LIMIT 1" ()

-- | Returns at most 2 new jobs with the status 'started', because we don't need
-- to warn users of jobs they added and are finished - that means their stopping conditions
-- have already ran and been tested.
getAddedJobsStillRunning :: (MonadIO m) => DB.Connection -> Maybe Int -> m [Text]
getAddedJobsStillRunning conn maxPrevJobId = do
  -- After adding a migration codd's internal schema must exist, but this check protects us
  -- against running in non-default-db connections
  coddSchemaVersion <- detectCoddSchema conn
  if coddSchemaVersion < CoddSchemaV5
    then pure []
    else
      fmap DB.fromOnly <$> query conn "SELECT quote_literal(jobname) FROM codd._background_jobs WHERE (? IS NULL OR jobid>?) AND status='started' ORDER BY jobid LIMIT 2" (maxPrevJobId, maxPrevJobId)

printTipForBackgroundJobs :: (CoddLogger m, Monad m) => [Text] -> m ()
printTipForBackgroundJobs = \case
  [] -> pure ()
  [singleNewJobName] -> do
    logInfoAlways "---------------------------------------------"
    logInfoAlways $ "<GREEN>Tip:</GREEN> your migration added a background job. Run <MAGENTA>SELECT * FROM codd.jobs</MAGENTA> to see it and do try to call <MAGENTA>SELECT codd.synchronously_finalize_background_job(" <> singleNewJobName <> ", '100 seconds')</MAGENTA> (with a longer timeout if necessary) to ensure your job stopping conditions are sound. Then, run <MAGENTA>SELECT * FROM codd.jobs</MAGENTA> again to see its new status."
  _ -> do
    logInfoAlways "---------------------------------------------"
    logInfoAlways "<GREEN>Tip:</GREEN> your migration added background jobs. Run <MAGENTA>SELECT * FROM codd.jobs</MAGENTA> to see them and do try to call something like <MAGENTA>SELECT codd.synchronously_finalize_background_job('job-name', '100 seconds')</MAGENTA> for each of your jobs to ensure their stopping conditions are sound. Then, run <MAGENTA>SELECT * FROM codd.jobs</MAGENTA> again to see its new status."
