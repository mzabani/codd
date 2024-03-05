module Codd.AppCommands.AddMigration
    ( AddMigrationOptions(..)
    , addMigration
    ) where

import qualified Codd
import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.AppCommands               ( timestampAndMoveMigrationFile
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( delayedOpenStreamFile
                                                , listMigrationsFromDisk
                                                )
import           Codd.Logging                   ( MonadLogger
                                                , logErrorN
                                                , logInfoAlways
                                                , logInfoN
                                                )
import           Codd.Parsing                   ( EnvVars
                                                , parseSqlMigration
                                                )
import           Codd.Query                     ( NotInTxn )
import           Codd.Representations           ( persistRepsToDisk
                                                , readRepresentationsFromDbWithSettings
                                                )
import           Codd.Types                     ( SqlFilePath(..) )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( secondsToDiffTime )
import qualified Database.PostgreSQL.Simple    as DB
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.FilePath                ( takeFileName )
import           UnliftIO                       ( MonadUnliftIO
                                                , liftIO
                                                , stderr
                                                )
import           UnliftIO.Directory             ( copyFile
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , removeFile
                                                )
import           UnliftIO.Exception             ( SomeException
                                                , try
                                                )
import           UnliftIO.Resource              ( runResourceT )

newtype AddMigrationOptions = AddMigrationOptions
  { dontApply :: Bool
  }

addMigration
    :: forall m
     . (MonadUnliftIO m, MonadLogger m, MonadThrow m, EnvVars m, NotInTxn m)
    => CoddSettings
    -> AddMigrationOptions
    -> Maybe FilePath
    -> SqlFilePath
    -> m ()
addMigration dbInfo@Codd.CoddSettings { onDiskReps, migsConnString, sqlMigrations } AddMigrationOptions { dontApply } destFolder sqlFp@(SqlFilePath fp)
    = do
        finalDir <- case (destFolder, sqlMigrations) of
            (Just f, _) -> pure f
            (Nothing, []) ->
                error
                    "Please specify '--dest-folder' or add at least one path to the CODD_MIGRATION_DIRS environment variable."
            (Nothing, f : _) -> pure f
        onDiskRepsDir <- either
            pure
            (error
                "This functionality needs a directory to write the expected representations to. Report this as a bug."
            )
            onDiskReps

        migFileExists <- doesFileExist fp
        unless migFileExists $ do
            logErrorN
                $  "Could not find migration file \""
                <> Text.pack fp
                <> "\""
            liftIO $ exitWith $ ExitFailure 99

        finalDirExists <- doesDirectoryExist finalDir
        unless finalDirExists $ do
            logErrorN
                $  "Could not find destination directory \""
                <> Text.pack finalDir
                <> "\""
            liftIO $ exitWith $ ExitFailure 98

        expectedSchemaDirExists <- doesDirectoryExist onDiskRepsDir
        unless expectedSchemaDirExists $ do
            logErrorN
                $ "Could not find directory for expected DB schema representation \""
                <> Text.pack onDiskRepsDir
                <> "\""
            liftIO $ exitWith $ ExitFailure 97

        isFirstMigration <- null <$> listMigrationsFromDisk sqlMigrations []
        runResourceT $ do
            migStream     <- delayedOpenStreamFile fp
            parsedSqlMigE <- parseSqlMigration (takeFileName fp) migStream
            case parsedSqlMigE of
                Left err -> do
                    logErrorN $ "Could not add migration " <> Text.pack err
                    liftIO $ when
                        isFirstMigration
                        (printSuggestedFirstMigration migsConnString)
                    liftIO $ exitWith $ ExitFailure 96

                Right sqlMig -> do
                    migCheck <- checkMigration sqlMig
                    let migError = case migCheck of
                            Left  err -> Just err
                            Right mc  -> transactionManagementProblem mc

                    case migError of
                        Nothing  -> pure ()
                        Just err -> do
                            logErrorN err
                            liftIO $ when
                                isFirstMigration
                                (printSuggestedFirstMigration migsConnString)
                            liftIO $ exitWith $ ExitFailure 95

            finalMigFile <- timestampAndMoveMigrationFile sqlFp finalDir
            addE         <- try $ do
                unless dontApply $ do
                    databaseSchemas <- Codd.applyMigrationsNoCheck
                        dbInfo
                        Nothing
                        (secondsToDiffTime 5)
                        (readRepresentationsFromDbWithSettings dbInfo)
                    persistRepsToDisk databaseSchemas onDiskRepsDir

                    logInfoAlways
                        $  "New migration applied and added to "
                        <> Text.pack finalMigFile
                    logInfoAlways
                        $ "Updated expected DB schema representations in the <MAGENTA>"
                        <> Text.pack onDiskRepsDir
                        <> "</MAGENTA> folder"
                when dontApply
                    $  logInfoN
                    $  "Migration was NOT applied, but was added to "
                    <> Text.pack finalMigFile
            case addE of
                Right _                    -> pure ()
                Left  (e :: SomeException) -> liftIO $ do
                    -- Print error and move file back to its original directory
                    Text.hPutStrLn stderr $ Text.pack $ show e
                    copyFile finalMigFile fp
                    removeFile finalMigFile

                    when isFirstMigration
                         (printSuggestedFirstMigration migsConnString)

                    exitWith $ ExitFailure 1

printSuggestedFirstMigration :: DB.ConnectInfo -> IO ()
printSuggestedFirstMigration DB.ConnectInfo { connectDatabase, connectUser } =
    putStrLn
        $ "\nTip: It looks like this is your first migration. Make sure either the target database of your default connection string already exists, or add a migration that creates your database with a custom connection string. Example:\n\
                        \\n\
                        \    -- codd: no-txn\n\
                        \    -- codd-connection: dbname=postgres user=postgres host=localhost\n\
                        \    -- Make sure the connection string above works, or change it to one that works.\n\
                        \    CREATE DATABASE \""
        ++ connectDatabase
        ++ "\" OWNER \""
        ++ connectUser
        <> "\";\n\
                        \    -- Also make sure the DB above doesn't exist yet, and that the DB owner does.\n\
                        \\n\
                        \- The migration above looks scary, but it's one of the rare few that will require anything other than plain SQL.\n\
                        \- If this is what you need, feel free to copy the migration above into a .sql file, modify it accordingly and add that as your first migration.\n\
                        \- If the above doesn't work, you want a more complete example or want to know more, make sure to read https://github.com/mzabani/codd/blob/master/docs/BOOTSTRAPPING.md for more on bootstrapping your database with codd.\n"
