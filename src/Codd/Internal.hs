module Codd.Internal where

import           Prelude                 hiding ( readFile )

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( DbHashes
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Internal.MultiQueryStatement
                                                ( InTransaction(..)
                                                , multiQueryStatement_
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , ParsingOptions(..)
                                                , SqlMigration(..)
                                                , parseAddedSqlMigration
                                                )
import           Codd.Query                     ( execvoid_
                                                , query
                                                )
import           Control.Monad                  ( forM
                                                , forM_
                                                , void
                                                , when
                                                , unless
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Logger           ( MonadLogger
                                                , NoLoggingT
                                                , logDebugN
                                                , logWarnN
                                                , runNoLoggingT
                                                )
import           Data.ByteString                ( ByteString
                                                , readFile
                                                )
import           Data.Either                    ( isLeft )
import qualified Data.List                     as List
import           Data.List                      ( find
                                                , sortOn
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( isNothing
                                                , mapMaybe
                                                )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Database.PostgreSQL.Simple    as DB
import           System.FilePath                ( (</>) )
import           UnliftIO                       ( MonadUnliftIO
                                                , toIO
                                                )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Directory             ( listDirectory )
import           UnliftIO.Exception             ( bracket
                                                , catchAny
                                                , finally
                                                , throwIO
                                                , tryAny
                                                )

dbIdentifier :: Text -> DB.Query
dbIdentifier s = "\"" <> fromString (Text.unpack s) <> "\""

-- | Tries to connect until a connection succeeds or until a timeout, executes the supplied action and disposes of the opened Connection.
withConnection
    :: (MonadUnliftIO m, MonadIO m)
    => DB.ConnectInfo
    -> (DB.Connection -> m a)
    -> m a
withConnection connStr action = go (50 :: Int) -- At most 50 * 100ms = 5 seconds
  where
    wrappedAction n eitherConn = do
        case eitherConn of
            Left e -> if n <= 0
                then throwIO e
                else threadDelay (1000 * 100) >> go (n - 1)
            Right conn -> action conn
    go n = bracket (tryAny $ liftIO $ DB.connect connStr)
                   (either (const $ pure ()) (liftIO . DB.close))
                   (wrappedAction n)

beginCommitTxnBracket
    :: (MonadUnliftIO m, MonadIO m) => DB.Connection -> m a -> m a
beginCommitTxnBracket conn f = do
    iof <- toIO f
    liftIO $ DB.withTransaction conn iof

beginRollbackTxnBracket
    :: (MonadUnliftIO m, MonadIO m) => DB.Connection -> m a -> m a
beginRollbackTxnBracket conn f =
    (execvoid_ conn "BEGIN" >> f) `finally` execvoid_ conn "ROLLBACK"

type TxnBracket m = forall a . DB.Connection -> m a -> m a

checkExpectedHashesAfterAction
    :: (MonadLogger m, MonadUnliftIO m, MonadIO m)
    => CoddSettings
    -> Maybe DbHashes
    -> DB.Connection
    -> m ()
checkExpectedHashesAfterAction coddSettings mExpectedHashes conn = do
    case mExpectedHashes of
        Nothing             -> pure ()
        Just expectedHashes -> do
            dbhashes <- readHashesFromDatabaseWithSettings coddSettings conn
            when (dbhashes /= expectedHashes) $ throwIO $ userError
                "DB checksums check failed."

-- | Creates the App's Database and Codd's schema if it does not yet exist.
createEmptyDbIfNecessary
    :: forall m n
     . (MonadUnliftIO m, MonadIO m, n ~ NoLoggingT m)
    => CoddSettings
    -> m ()
createEmptyDbIfNecessary settings = runNoLoggingT
    $ applyMigrationsInternal beginCommitTxnBracket applyZeroMigs settings
    -- Very special case: it's probably not very interesting to print database and Codd schema creation too many times.
    -- Since this function gets called a lot for "just-in-case" parts of the App, let's hide its output.
  where
    applyZeroMigs
        :: DB.Connection -> TxnBracket n -> [NonEmpty MigrationToRun] -> n ()
    applyZeroMigs conn txnBracket _ =
        baseApplyMigsBlock (const $ pure ()) conn txnBracket []

applyMigrationsInternal
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => TxnBracket m
    -> (DB.Connection -> TxnBracket m -> [BlockOfMigrations] -> m a)
    -> CoddSettings
    -> m a
applyMigrationsInternal txnBracket txnApp (coddSettings@CoddSettings { superUserConnString, dbName })
    = do
        let unsafeDbName = dbIdentifier dbName
        logDebugN
            $  "Checking if Database "
            <> dbName
            <> " exists and creating it if necessary..."
        withConnection superUserConnString $ \conn -> do
            dbExists <- isSingleTrue <$> query
                conn
                "SELECT TRUE FROM pg_database WHERE datname = ?"
                (DB.Only dbName)
            unless dbExists
                $  execvoid_ conn
                $  "CREATE DATABASE "
                <> unsafeDbName

        logDebugN
            "Checking if Codd Schema exists and creating it if necessary..."
        let appDbSuperUserConnString =
                superUserConnString { DB.connectDatabase = Text.unpack dbName }
        ret <- withConnection appDbSuperUserConnString $ \conn -> do
            -- TODO: Do we assume a failed transaction below means some other app is creating the table and won the race?
            --       We should find a better way to do this in the future.
            liftIO $ DB.withTransaction conn $ do
                schemaAlreadyExists <- isSingleTrue <$> query
                    conn
                    "SELECT TRUE FROM pg_catalog.pg_namespace WHERE nspname = ?"
                    (DB.Only ("codd_schema" :: String))
                tblAlreadyExists <- isSingleTrue <$> query
                    conn
                    "SELECT TRUE FROM pg_catalog.pg_tables WHERE tablename = ? AND schemaname = ?"
                    ("sql_migrations" :: String, "codd_schema" :: String)
                unless schemaAlreadyExists
                    $ execvoid_ conn
                    $ "CREATE SCHEMA codd_schema"
                unless tblAlreadyExists $ do
                    execvoid_ conn
                        $  "CREATE TABLE codd_schema.sql_migrations ( "
                        <> " migration_timestamp timestamptz not null"
                        <> ", non_dest_section_applied_at timestamptz not null "
                        <> ", dest_section_applied_at timestamptz "
                        <> ", name text not null "
                        <> ", unique (name), unique (migration_timestamp))"

            blocksOfMigsToRun <- collectPendingMigrations coddSettings
            txnApp conn txnBracket blocksOfMigsToRun

        logDebugN $ "All migrations applied to " <> dbName <> " successfully"
        return ret

  where
    isSingleTrue v = v == [DB.Only True]
    _noErrors f = catchAny f (const (pure ()))

-- | Parses on-disk migrations and checks for destructive SQL sections of migrations on the Database to collect which migrations must run,
-- grouped by in-txn/no-txn. The Database must already exist and Codd's schema must have been created.
collectPendingMigrations
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> m [BlockOfMigrations]
collectPendingMigrations (CoddSettings { superUserConnString, dbName, sqlMigrations, deploymentWorkflow })
    = do
        let appDbSuperUserConnString =
                superUserConnString { DB.connectDatabase = Text.unpack dbName }
        withConnection appDbSuperUserConnString $ \conn -> do
            -- Note: there should be no risk of race conditions for the query below, already-run migrations can't be deleted or have its non-null fields set to null again
            logDebugN
                "Checking in the Database which SQL migrations have already run to completion..."
            migsThatHaveRunSomeSection :: [(FilePath, Bool)] <-
                liftIO $ DB.withTransaction conn $ query
                    conn
                    "SELECT name, dest_section_applied_at IS NOT NULL FROM codd_schema.sql_migrations"
                    ()
            let migsCompleted = mapMaybe
                    (\(n, f) -> if f then Just n else Nothing)
                    migsThatHaveRunSomeSection
                migsToRunDestSection = mapMaybe
                    (\(n, f) -> if f then Nothing else Just n)
                    migsThatHaveRunSomeSection

            logDebugN "Parse-checking all pending SQL Migrations..."
            pendingParsedMigrations :: [MigrationToRun] <- either
                (\(sqlDirs :: [FilePath]) -> do
                    allSqlMigrationFiles :: [(FilePath, FilePath)] <-
                        fmap (sortOn fst . concat) $ forM sqlDirs $ \dir -> do
                            filesInDir <- listDirectory dir
                            return $ map (\fn -> (fn, dir </> fn)) $ filter
                                (".sql" `List.isSuffixOf`)
                                filesInDir
                    let
                        pendingSqlMigrationFiles = mapMaybe
                            (\case
                                (n, fp)
                                    | n `elem` migsCompleted -> Nothing
                                    | n `elem` migsToRunDestSection -> Just
                                        ((n, fp), ApplyDestructiveOnly)
                                    | otherwise -> Just
                                        ((n, fp), ApplyNonDestructiveOnly)
                            )
                            allSqlMigrationFiles
                    sqlMigrationsContents :: [ ( (FilePath, ByteString)
                          , ApplySingleMigration
                          )
                        ]                                               <-
                        liftIO
                        $      pendingSqlMigrationFiles
                        `forM` \((fn, fp), ap) ->
                                   readFile fp
                                       >>= (\contents ->
                                               pure ((fn, contents), ap)
                                           )
                    -- TODO: decodeUtf8Lenient ?
                    parsedMigs <-
                        forM sqlMigrationsContents
                            $ \((fn, decodeUtf8 -> sql), ap) -> do
                                  let
                                      parsedMigGood = parseAddedSqlMigration
                                          deploymentWorkflow
                                          DoParse
                                          fn
                                          sql
                                  parsedMigFinal <- do
                                      case parsedMigGood of
                                          Right _ -> pure parsedMigGood
                                          Left  _ -> do
                                              logWarnN
                                                  $ Text.pack fn
                                                  <> " could not be parsed and thus will be considered in is entirety as in-txn"
                                              pure $ parseAddedSqlMigration
                                                  deploymentWorkflow
                                                  NoParse
                                                  fn
                                                  sql
                                  pure
                                      ( fn
                                      , flip MigrationToRun ap
                                          <$> parsedMigFinal
                                      )
                    case find (\(_, m) -> isLeft m) parsedMigs of
                        Just (fn, Left e) ->
                            error
                                $  "Error parsing migration "
                                ++ fn
                                ++ ": "
                                ++ show e
                        _ -> pure ()

                    return
                        $ either
                              (error "Failed to parse-check pending Migrations")
                              id
                        $ traverse snd parsedMigs
                )
                (\(ams :: [AddedSqlMigration]) ->
                    return
                        $ mapMaybe
                              (\case
                                  a@(AddedSqlMigration mig _)
                                      | migrationName mig `elem` migsCompleted -> Nothing
                                      | migrationName mig
                                          `elem` migsToRunDestSection -> Just
                                      $ MigrationToRun
                                            a
                                            ApplyDestructiveOnly
                                      | otherwise -> Just $ MigrationToRun
                                          a
                                          ApplyNonDestructiveOnly
                              )
                        $ sortOn (\(AddedSqlMigration _ ts) -> ts) ams
                )
                sqlMigrations

            -- Run all Migrations now. Group them in blocks of consecutive transactions by in-txn/no-txn.
            return $ NE.groupWith runInTxn pendingParsedMigrations

mainAppApplyMigsBlock
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> Maybe DbHashes
    -> DB.Connection
    -> TxnBracket m
    -> [BlockOfMigrations]
    -> m ()
mainAppApplyMigsBlock coddSettings mExpectedHashes conn txnBracket pendingMigBlocks
    = do
        let actionAfter =
                checkExpectedHashesAfterAction coddSettings mExpectedHashes
        baseApplyMigsBlock actionAfter conn txnBracket pendingMigBlocks

-- | Note: "actionAfter" runs in the same transaction if all supplied migrations are in-txn, or separately, and not in an explicit transaction, otherwise.
baseApplyMigsBlock
    :: forall m a
     . (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => (DB.Connection -> m a)
    -> DB.Connection
    -> TxnBracket m
    -> [BlockOfMigrations]
    -> m a
baseApplyMigsBlock actionAfter conn txnBracket blocksOfMigs = do
    case blocksOfMigs of
        []                   -> actionAfter conn
        [b1] | blockInTxn b1 -> runBlock actionAfter b1
        _                    -> do
            forM_ blocksOfMigs $ runBlock (const (pure ()))
            actionAfter conn

  where
    runMigs migs = forM_ migs $ \(MigrationToRun asqlmig ap) ->
        applySingleMigration conn ap asqlmig
    runBlock :: (DB.Connection -> m b) -> BlockOfMigrations -> m b
    runBlock act migBlock = do
        if blockInTxn migBlock
            then do
                txnBracket conn $ do
                    runMigs migBlock
                    act conn
            else do
                runMigs migBlock
                act conn

data ApplySingleMigration = ApplyDestructiveOnly | ApplyNonDestructiveOnly deriving stock Show

data MigrationToRun = MigrationToRun AddedSqlMigration ApplySingleMigration
    deriving stock Show
runInTxn :: MigrationToRun -> Bool
runInTxn (MigrationToRun (AddedSqlMigration m _) ap) = case ap of
    ApplyDestructiveOnly    -> destructiveInTxn m
    ApplyNonDestructiveOnly -> nonDestructiveInTxn m

type BlockOfMigrations = NonEmpty MigrationToRun
blockInTxn :: BlockOfMigrations -> Bool
blockInTxn (m1 :| _) = runInTxn m1

applySingleMigration
    :: (MonadIO m, MonadLogger m)
    => DB.Connection
    -> ApplySingleMigration
    -> AddedSqlMigration
    -> m ()
applySingleMigration conn ap (AddedSqlMigration sqlMig migTimestamp) = do
    let fn = migrationName sqlMig
    case ap of
        ApplyNonDestructiveOnly -> do
            if isNothing (destructiveSql sqlMig)
                then logDebugN $ "Applying " <> Text.pack fn
                else
                    logDebugN
                    $  "Applying non-destructive section of "
                    <> Text.pack fn

            case nonDestructiveSql sqlMig of
                Nothing -> pure ()
                Just nonDestSql ->
                    let
                        inTxn = if nonDestructiveInTxn sqlMig
                            then InTransaction
                            else NotInTransaction
                    in  multiQueryStatement_ inTxn conn nonDestSql

                                        -- We mark the destructive section as ran if it's empty as well. This goes well with the Simple Deployment workflow,
                                        -- since every migration will have both sections marked as ran sequentially.
            liftIO $ void $ DB.execute
                conn
                "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, non_dest_section_applied_at, dest_section_applied_at) VALUES (?, ?, now(), CASE WHEN ? THEN now() END)"
                (migTimestamp, fn, isNothing (destructiveSql sqlMig))
        ApplyDestructiveOnly -> do
            logDebugN $ "Applying destructive section of " <> Text.pack fn
            case destructiveSql sqlMig of
                Nothing -> pure ()
                Just destSql ->
                    let
                        inTxn = if destructiveInTxn sqlMig
                            then InTransaction
                            else NotInTransaction
                    in  multiQueryStatement_ inTxn conn destSql
            liftIO $ void $ DB.execute
                conn
                "UPDATE codd_schema.sql_migrations SET dest_section_applied_at = now() WHERE name=?"
                (DB.Only fn)
            -- TODO: Assert 1 row was updated
