{-# LANGUAGE BlockArguments, AllowAmbiguousTypes #-}
module Codd.Internal where

import           Prelude                 hiding ( readFile )

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal.MultiQueryStatement
                                                ( InTransaction(..)
                                                , multiQueryStatement_
                                                )
import           Codd.Internal.Retry            ( RetryIteration(..)
                                                , retryFold
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , AppliedMigration(..)
                                                , EnvVars(..)
                                                , FileStream(..)
                                                , ParsedSql(..)
                                                , SqlMigration(..)
                                                , parseAddedSqlMigration
                                                )
import           Codd.Query                     ( InTxn
                                                , InTxnT(..)
                                                , beginCommitTxnBracket
                                                , execvoid_
                                                , hoistInTxn
                                                , query
                                                , unsafeQuery1, CanStartTxn, withTxnIfNecessary, NotInTxn
                                                )
import           Codd.Representations           ( DbRep
                                                , logSchemasComparison
                                                , readRepresentationsFromDbWithSettings
                                                )
import           Codd.Types                     ( RetryPolicy(..)
                                                , TxnIsolationLvl(..)
                                                , singleTryPolicy
                                                )
import           Control.Monad                  ( (>=>)
                                                , foldM
                                                , forM
                                                , forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Logger           ( MonadLogger
                                                , logDebugN
                                                , logErrorN
                                                , logInfoN
                                                , logWarnN
                                                )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import           Data.Functor                   ( (<&>) )
import qualified Data.List                     as List
import           Data.List                      ( sortOn )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromMaybe )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( DiffTime
                                                , UTCTime
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import qualified Streaming.Prelude             as Streaming
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                )
import qualified System.IO                     as IO
import           UnliftIO                       ( MonadUnliftIO
                                                , hClose
                                                )
import           UnliftIO.Concurrent            ( threadDelay )
import           UnliftIO.Directory             ( listDirectory )
import           UnliftIO.Exception             ( IOException
                                                , bracket
                                                , catchJust
                                                , handleJust
                                                , throwIO
                                                , tryJust
                                                )
import           UnliftIO.IO                    ( IOMode(ReadMode)
                                                , openFile
                                                )
import           UnliftIO.MVar                  ( modifyMVar
                                                , newMVar
                                                , readMVar
                                                )
import           UnliftIO.Resource              ( MonadResource
                                                , ReleaseKey
                                                , ResourceT
                                                , allocate
                                                , release
                                                , runResourceT
                                                )

dbIdentifier :: Text -> DB.Query
dbIdentifier s = "\"" <> fromString (Text.unpack s) <> "\""

-- | Tries to connect until a connection succeeds or until a timeout,
--   executes the supplied action and disposes of the opened Connection.
withConnection
  :: (MonadUnliftIO m, MonadIO m)
  => DB.ConnectInfo
  -> DiffTime
  -> (DB.Connection -> m a)
  -> m a
withConnection connStr connectTimeout action = go connectTimeout
 where
  wrappedAction n eitherConn = do
    case eitherConn of
      Left e -> if n <= 0
        then throwIO e
        else
              -- Retries every 100ms
             threadDelay (1000 * 100) >> go (n - realToFrac @Double 0.1)
      Right conn -> action conn
  go n = bracket (tryConnectError $ liftIO $ DB.connect connStr)
                 (either (const $ pure ()) (liftIO . DB.close))
                 (wrappedAction n)
  tryConnectError =
    tryJust $ \e -> if isServerNotAvailableError e then Just e else Nothing

-- | Verifies if a libpq error means the server is not ready to accept connections yet,
-- either by not being listening at all or still being initializing.
isServerNotAvailableError :: IOException -> Bool
isServerNotAvailableError e =
  let err = Text.pack $ show e
  in  "libpq"
        `Text.isInfixOf` err
        &&               ("could not connect to server: Connection refused"
                         `Text.isInfixOf` err
                         || "Is the server running on that host and accepting"
                         `Text.isInfixOf` err
                         ||               "server closed the connection"
                         `Text.isInfixOf` err
                         ||               "the database system is starting up"
                         `Text.isInfixOf` err
                         )

-- | Returns true for errors such as "permission denied for database xxx"
isPermissionDeniedError :: DB.SqlError -> Bool
isPermissionDeniedError e = DB.sqlState e == "42501"

data BootstrapCheck = BootstrapCheck
  { defaultConnAccessible :: Bool
  , coddSchemaExists      :: Bool
  }

-- | Returns info on what kind of bootstrapping will be necessary,
-- waiting up to the time limit for postgres to be up before throwing
-- an exception.
checkNeedsBootstrapping
  :: (MonadUnliftIO m, MonadIO m)
  => DB.ConnectInfo
  -> DiffTime
  -> m BootstrapCheck
checkNeedsBootstrapping connInfo connectTimeout =
  handleJust
      (\e ->
                -- 1. No server available is a big "No", meaning we throw an exception.
             if isServerNotAvailableError e
        then Nothing

                                                                                                                                                                                                                                                                                                                                                                                               -- 2. Maybe the default migration connection string doesn't work because:
                                                                                                                                                                                                                                                                                                                                                                                               -- - The DB does not exist.
                                                                                                                                                                                                                                                                                                                                                                                               -- - CONNECT rights not granted.
                                                                                                                                                                                                                                                                                                                                                                                               -- - User doesn't exist.
                                                                                                                                                                                                                                                                                                                                                                                               -- In any case, it's best to be conservative and consider any libpq errors
                                                                                                                                                                                                                                                                                                                                                                                               -- here as errors that might just require bootstrapping.
        else if isLibPqError e
          then Just BootstrapCheck { defaultConnAccessible = False
                                   , coddSchemaExists      = False
                                   }

        -- 3. Let other exceptions blow up
          else Nothing
      )
      pure
    $ withConnection connInfo
                     connectTimeout
                     (fmap (BootstrapCheck True) . doesCoddSchemaExist)
 where
  isLibPqError :: IOException -> Bool
  isLibPqError e =
    let err = Text.pack $ show e in "libpq: failed" `Text.isInfixOf` err

data PendingMigrations m = PendingMigrations
  { pendingMigs    :: [BlockOfMigrations m]
  , bootstrapCheck :: BootstrapCheck
    -- ^ More information on what kind of bootstrapping is necessary.
  }

collectAndApplyMigrations
  :: ( MonadUnliftIO m
     , MonadIO m
     , MonadLogger m
     , MonadResource m
     , MonadThrow m
     , EnvVars m
     , NotInTxn m
     , txn ~ InTxnT (ResourceT m)
     , CanStartTxn (ResourceT m) txn
     )
  => ([BlockOfMigrations txn]
     -> DB.Connection
     -> txn a
     )
  -> CoddSettings
  -> Maybe [AddedSqlMigration m]
    -- ^ Instead of collecting migrations from disk according to codd settings, use these if they're defined.
  -> DiffTime
  -> m a
collectAndApplyMigrations lastAction settings@CoddSettings { migsConnString, sqlMigrations, txnIsolationLvl } mOverrideMigs connectTimeout
  = do
    let dbName                   = Text.pack $ DB.connectDatabase migsConnString
    let waitTimeInSecs :: Double = realToFrac connectTimeout
    logInfoN
      $  "Checking if database '"
      <> dbName
      <> "' is accessible with the configured connection string... (waiting up to "
      <> Text.pack (show @Int $ truncate waitTimeInSecs)
      <> "sec)"

    let migsToUse = maybe (Left sqlMigrations) Right mOverrideMigs
    pendingMigs <- collectPendingMigrations migsConnString
                                            migsToUse
                                            txnIsolationLvl
                                            connectTimeout
    applyCollectedMigrations lastAction settings pendingMigs connectTimeout

applyCollectedMigrations
  :: forall m a txn. (MonadUnliftIO m, MonadIO m, MonadLogger m, MonadResource m, NotInTxn m, txn ~ InTxnT (ResourceT m), CanStartTxn (ResourceT m) txn)
  => ([BlockOfMigrations txn]
     -> DB.Connection
     -> txn a
     )
  -> CoddSettings
  -> PendingMigrations m
  -> DiffTime
  -> m a
applyCollectedMigrations lastAction CoddSettings { migsConnString, retryPolicy, txnIsolationLvl } PendingMigrations { pendingMigs, bootstrapCheck } connectTimeout
  = do
    let dbName = Text.pack $ DB.connectDatabase migsConnString
        txnApp = baseApplyMigsBlock migsConnString
                                    connectTimeout
                                    retryPolicy
                                    lastAction
                                    txnIsolationLvl


    ApplyMigsResult _ actionAfterResult <- txnApp bootstrapCheck pendingMigs

    logInfoN $ "All migrations applied to " <> dbName <> " successfully"
    return actionAfterResult

isSingleTrue :: [DB.Only Bool] -> Bool
isSingleTrue v = v == [DB.Only True]

doesCoddSchemaExist :: MonadIO m => DB.Connection -> m Bool
doesCoddSchemaExist conn = isSingleTrue <$> query
  conn
  "SELECT TRUE FROM pg_catalog.pg_tables WHERE tablename = ? AND schemaname = ?"
  ("sql_migrations" :: String, "codd_schema" :: String)

createCoddSchema
  :: (MonadUnliftIO m, MonadIO m) => TxnIsolationLvl -> DB.Connection -> m ()
createCoddSchema txnIsolationLvl conn = catchJust
  (\e -> if isPermissionDeniedError e then Just () else Nothing)
  go
  (\() ->
    throwIO
      $ userError
          "Not enough permissions to create codd's internal schema. Please check that your default connection string can create tables, sequences and GRANT them permissions."
  )
 where
  go = liftIO $ beginCommitTxnBracket txnIsolationLvl conn $ do
    schemaAlreadyExists <- doesCoddSchemaExist conn
    unless schemaAlreadyExists $ do
      execvoid_
        conn
        "CREATE SCHEMA codd_schema; GRANT USAGE ON SCHEMA codd_schema TO PUBLIC;"
      execvoid_ conn
        $ "CREATE TABLE codd_schema.sql_migrations ( \
                \  id SERIAL PRIMARY KEY\
                \, migration_timestamp timestamptz not null\
                \, applied_at timestamptz not null \
                \, name text not null \
                \, unique (name), unique (migration_timestamp));"
        <> -- It is not necessary to grant SELECT on the table, but it helps _a lot_ with a test and shouldn't hurt.
             -- SELECT on the sequence enables dumps by unprivileged users
           "GRANT INSERT,SELECT ON TABLE codd_schema.sql_migrations TO PUBLIC;\
                   \GRANT USAGE ,SELECT ON SEQUENCE codd_schema.sql_migrations_id_seq TO PUBLIC;"

-- | Collects pending migrations and separates them according to being bootstrap
--   or not.
collectPendingMigrations
  :: ( MonadUnliftIO m
     , MonadIO m
     , MonadLogger m
     , MonadResource m
     , MonadThrow m
     , EnvVars m
     )
  => DB.ConnectInfo
  -> Either [FilePath] [AddedSqlMigration m]
  -> TxnIsolationLvl
  -> DiffTime
  -> m (PendingMigrations m)
collectPendingMigrations defaultConnString sqlMigrations txnIsolationLvl connectTimeout
  = do
    bootCheck   <- checkNeedsBootstrapping defaultConnString connectTimeout
    pendingMigs <- collect bootCheck

    unless (defaultConnAccessible bootCheck) $ do
      logInfoN
        "Default connection string is not accessible. Codd will run in bootstrap mode, expecting the first migrations will contain custom connection strings and will create/bootstrap the database."

      -- The specific error below isn't necessary at this stage, but it's much more informative
      -- than the errors we'd generate further ahead.
      let bootstrapMigBlocks = takeWhile isDifferentDbMigBlock pendingMigs
      when (null bootstrapMigBlocks) $ do
        logErrorN
          "The earliest existing migration has no custom connection string or there are no migrations at all. Exiting."
        liftIO exitFailure

    pure $ PendingMigrations pendingMigs bootCheck

 where
  isDifferentDbMigBlock (BlockOfMigrations (m1 :| _) _) =
    case migrationCustomConnInfo $ addedSqlMig m1 of
      Nothing -> False
      Just connInfo ->
        DB.connectDatabase defaultConnString /= DB.connectDatabase connInfo
  collect bootCheck = do
    logInfoN "Checking which SQL migrations have already been applied..."
    migsAlreadyApplied :: [FilePath] <- if coddSchemaExists bootCheck
      then withConnection
        defaultConnString
        connectTimeout
        (\conn ->
          liftIO
            $   beginCommitTxnBracket txnIsolationLvl conn
            $   map DB.fromOnly
            <$> query
                  conn
                  "SELECT name FROM codd_schema.sql_migrations WHERE applied_at IS NOT NULL"
                  ()
        )
      else pure []

    logInfoN "Parse-checking headers of all pending SQL Migrations..."
    parseMigrationFiles migsAlreadyApplied sqlMigrations




-- | Opens a UTF-8 file allowing it to be read in streaming fashion.
-- We can't use Streaming.fromHandle because it uses hGetLine, which removes '\n' from lines it
-- reads, but does not append '\n' to the Strings it returns, making it impossible to know 
-- whether the last line had a '\n' after it.
-- See https://hackage.haskell.org/package/streaming-0.2.3.1/docs/src/Streaming.Prelude.html#fromHandle
-- So we copied streaming's implementation and modified it slightly.
streamingReadFile :: MonadResource m => FilePath -> m (FileStream m)
streamingReadFile filePath = do
  (releaseKey, handle) <- allocate (openFile filePath ReadMode) hClose
  pure $ FileStream { filePath, releaseKey, fileStream = go handle }
 where
  go h = do
    eof <- liftIO $ IO.hIsEOF h
    unless eof $ do
      str  <- liftIO $ Text.hGetLine h
      eof2 <- liftIO $ IO.hIsEOF h
      if eof2
        then Streaming.yield str
        else do
          Streaming.each [str, "\n"]
          go h

closeFileStream :: MonadResource m => FileStream m -> m ()
closeFileStream (FileStream _ releaseKey _) = release releaseKey

parseMigrationFiles
  :: forall m
   . ( MonadUnliftIO m
     , MonadIO m
     , MonadLogger m
     , MonadResource m
     , MonadThrow m
     , EnvVars m
     )
  => [FilePath]
  -> Either [FilePath] [AddedSqlMigration m]
  -> m [BlockOfMigrations m]
parseMigrationFiles migsCompleted sqlMigrations = do
  pendingParsedMigrations :: [ ( Either String (FileStream m)
      , AddedSqlMigration m
      )
    ]                                                         <-
    either (listPendingFromDisk >=> readFromDisk)
           (pure . readFromMemory . listPendingFromMemory)
           sqlMigrations

  -- Group migrations in blocks of consecutive transactions by in-txn/no-txn and custom
  -- connection string.
  pure
    $   NE.groupWith
          (\(_, AddedSqlMigration m _) ->
            (migrationInTxn m, migrationCustomConnInfo m)
          )
          pendingParsedMigrations
    <&> \migs -> BlockOfMigrations { allMigs     = snd <$> migs
                                   , reReadBlock = reRead migs
                                   }

 where
  reRead oldMigsAndPaths = do
      -- Close handles of all migrations in the block, re-open and read+parse them
    filePaths <- forM oldMigsAndPaths $ \case
      (Left _memStream, _) ->
        error "Re-reading in-memory streams is not yet implemented"
      (Right fileStream, _) ->
        closeFileStream fileStream >> pure (filePath fileStream)
    newMigs <- readFromDisk filePaths
    pure BlockOfMigrations { allMigs     = snd <$> newMigs
                           , reReadBlock = reRead newMigs
                           }
  readFromMemory
    :: [AddedSqlMigration m]
    -> [(Either String (FileStream m), AddedSqlMigration m)]
  readFromMemory ams =
    map
        (\asqlmig@(AddedSqlMigration mig _) ->
          (Left $ migrationName mig, asqlmig)
        )
      $ sortOn (\(AddedSqlMigration _ ts) -> ts) ams
  listPendingFromMemory =
    filter
      (\(AddedSqlMigration mig _) -> migrationName mig `notElem` migsCompleted)
  listPendingFromDisk sqlDirs =
    fmap (sortOn takeFileName . concat) $ forM sqlDirs $ \dir -> do
      filesInDir <- listDirectory dir
      return $ map (dir </>) $ filter
        (\fn -> ".sql" `List.isSuffixOf` fn && fn `notElem` migsCompleted)
        filesInDir

  readFromDisk
    :: forall t
     . Traversable t
    => t FilePath
    -> m (t (Either String (FileStream m), AddedSqlMigration m))
  readFromDisk pendingSqlMigrationFiles = do
    sqlMigrationsContents :: t (FileStream m) <-
      pendingSqlMigrationFiles `forM` streamingReadFile

    forM sqlMigrationsContents $ \fileStream -> do
      let fn = takeFileName $ filePath fileStream
      parsedMig <- parseAddedSqlMigration fn fileStream
      case parsedMig of
        Left err -> do
          throwIO
            $  userError
            $  "Fatal error parsing migration '"
            <> fn
            <> "': "
            <> err
        Right asqlmig@(AddedSqlMigration mig _) -> do
          case migrationSql mig of
            UnparsedSql _ ->
              logWarnN
                $ Text.pack fn
                <> " is not to be parsed and thus will be considered in is entirety as in-txn, without support for COPY."
            _ -> pure ()
          pure (Right fileStream, asqlmig)

data ApplyMigsResult a = ApplyMigsResult
  { migsAppliedAt     :: [AppliedMigration]
  , actionAfterResult :: a
  }

-- | Applies the supplied migrations, running blocks of (in-txn, same-connection-string) migrations each within their own
-- separate transactions.
-- Behaviour here unfortunately is _really_ complex right now. Important notes:
-- - Iff there's a single (in-txn, same-connection-string) block of migrations *with* the default connection string,
--   then "actionAfter" runs in the same transaction (and thus in the same connection as well) as that block.
--   Otherwise - and including if there are no migrations - it runs after all migrations, in an explicit transaction of
--   its own and in the default connection, which is not necessarily the same connection as the last migration's.
baseApplyMigsBlock
  :: forall m a txn
   . (MonadUnliftIO m, MonadIO m, MonadLogger m, NotInTxn m, txn ~ InTxnT (ResourceT m), CanStartTxn (ResourceT m) txn) -- TODO: This last constraint should be redundant. Review.
  => DB.ConnectInfo
  -> DiffTime
  -> RetryPolicy
  -> ([BlockOfMigrations txn]
     -> DB.Connection
     -> txn a
     )
  -> TxnIsolationLvl
  -> BootstrapCheck
  -> [BlockOfMigrations m]
  -> m (ApplyMigsResult a)
baseApplyMigsBlock defaultConnInfo connectTimeout retryPol actionAfter isolLvl bootstrapCheck blocksOfMigs
  =
        -- This function is complex because:
        -- 1. There are 3 major cases (no migrations, one in-txn default-conn-string block of migrations, all others).
        -- 2. We need to open connections only when strictly necessary due to bootstrapping.
        -- 3. We want to insert into codd_schema.sql_migrations as early as possible even for custom-connection migrations.
        -- 4. When possible, we want to insert into codd_schema.sql_migrations in the same transaction the migrations are running.

        -- So we separate the first two cases (the second hopefully being the most common one) into simpler code paths
        -- and make sure to test the third code path very well.
    let
      -- Such ugliness just to get types right..
      hoistedBlocks1 :: [BlockOfMigrations (ResourceT m)] = map (hoistBlockOfMigrationsInTxn @m @ResourceT) blocksOfMigs
      hoistedBlocks :: [BlockOfMigrations (InTxnT (ResourceT m))] = map (hoistBlockOfMigrationsInTxn @(ResourceT m) @InTxnT) hoistedBlocks1
    in
    case blocksOfMigs of
    [] -> withConnection defaultConnInfo connectTimeout $ \defaultConn ->
      runResourceT $ 
        withTxnIfNecessary
          isolLvl
          defaultConn
            (ApplyMigsResult [] <$> actionAfter [] defaultConn)
    [block]
      | blockInTxn block
        && fromMaybe defaultConnInfo (blockCustomConnInfo block)
        == defaultConnInfo
      ->
            -- Our very "special" and most common case case:
            -- all migs being in-txn in the default connection-string.
            -- We use `runResourceT` only to make type signatures more homogeneous, since
            -- the more complex case uses `runResourceT` as well.
         withConnection defaultConnInfo connectTimeout $ \defaultConn ->
            runResourceT $ runBlock (actionAfter hoistedBlocks) defaultConn block (registerRanMigration @txn @txn defaultConn)
    _ -> runResourceT $ do
           -- Note: We could probably compound this Monad with StateT instead of using an MVar, but IIRC that creates issues
           -- with MonadUnliftIO.
      connsPerInfo <- newMVar (mempty :: [(DB.ConnectInfo, DB.Connection)])
      let openConn :: DB.ConnectInfo -> ResourceT m (ReleaseKey, DB.Connection)
          openConn cinfo = flip allocate DB.close $ do
            mConn <- lookup cinfo <$> readMVar connsPerInfo
            case mConn of
              Just conn -> pure conn
              Nothing   -> modifyMVar connsPerInfo $ \m -> do
                                  -- Need to unliftIO to log in here?
                                  -- logInfoN $ "Connecting to (TODO: REDACT PASSWORD) " <> Text.pack (show cinfo)
                conn <- DB.connect cinfo
                pure ((cinfo, conn) : m, conn)

          queryConn :: DB.ConnectInfo -> ResourceT m (Maybe DB.Connection)
          queryConn cinfo = lookup cinfo <$> readMVar connsPerInfo

      (appliedMigs :: [(AppliedMigration, MigrationRegistered)], finalBootCheck :: BootstrapCheck) <-
        foldM
          (\(previouslyAppliedMigs, bootCheck) block -> do
            let cinfo = fromMaybe defaultConnInfo (blockCustomConnInfo block)
            (_, conn)                   <- openConn cinfo
            mDefaultConn                <- queryConn defaultConnInfo

            -- The default connection string is the one which should have permissions
            -- to create codd_schema.
            (runAfterMig, newBootCheck) <- case mDefaultConn of
              Nothing ->
                pure
                  ( \_ _ -> DB.fromOnly <$> unsafeQuery1 conn "SELECT now()" ()
                  , bootCheck
                  )
              Just defaultConn -> do
                newBootCheck <- if not (coddSchemaExists bootCheck)
                  then do
                    logInfoN "Creating codd_schema..."
                    createCoddSchema isolLvl defaultConn
                    pure bootCheck { defaultConnAccessible = True
                                   , coddSchemaExists      = True
                                   }
                  else pure bootCheck
                lift $ registerPendingMigrations defaultConn
                                                 previouslyAppliedMigs
                pure
                  ( registerRanMigration @txn @txn defaultConn
                  , newBootCheck
                  )

            ApplyMigsResult justAppliedMigs () <- runBlock (const (pure ())) conn block runAfterMig

            -- Keep in mind that migrations are applied but might not be registered if
            -- we still haven't run any default-connection-string migrations.
            pure $ (, newBootCheck) $ map
              (
              , if coddSchemaExists newBootCheck
                then MigrationRegistered
                else MigrationNotRegistered
              )
              (map fst previouslyAppliedMigs ++ justAppliedMigs)
          )
          ([], bootstrapCheck)
          blocksOfMigs

      -- It is possible to have only non-default-connection-string migrations.
      -- In that case, we assume the default-connection-string will be valid after those migrations
      -- and use that to register all applied migrations and then run "actionAfter".
      (_, defaultConn) <- openConn defaultConnInfo
      unless (coddSchemaExists finalBootCheck) $ do
        logInfoN "Creating codd_schema..."
        createCoddSchema isolLvl defaultConn
      beginCommitTxnBracket isolLvl defaultConn
        $ registerPendingMigrations defaultConn appliedMigs
      actAfterResult <- beginCommitTxnBracket isolLvl defaultConn
        $ actionAfter hoistedBlocks defaultConn

      pure $ ApplyMigsResult (map fst appliedMigs) actAfterResult

 where
  registerPendingMigrations
    :: MonadIO n
    => DB.Connection
    -> [(AppliedMigration, MigrationRegistered)]
    -> n ()
  registerPendingMigrations defaultConn appliedMigs =
    liftIO
      $ forM_ [ am | (am, MigrationNotRegistered) <- appliedMigs ]
      $ \AppliedMigration {..} -> DB.execute
          defaultConn
          "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, applied_at) \
                                \                            VALUES (?, ?, ?)"
          (appliedMigrationTimestamp, appliedMigrationName, appliedMigrationAt)

  runMigs
    :: (MonadUnliftIO n, MonadLogger n, CanStartTxn n txn)
    => DB.Connection
    -> RetryPolicy
    -> NonEmpty (AddedSqlMigration n)
    -> (FilePath -> DB.UTCTimestamp -> txn UTCTime)
    -> n [AppliedMigration]
  runMigs conn withRetryPolicy migs runAfterMig =
    fmap NE.toList $ forM migs $ applySingleMigration conn
                                                      withRetryPolicy
                                                      runAfterMig
                                                      isolLvl
  runBlock
    :: (DB.Connection -> txn b)
    -> DB.Connection
    -> BlockOfMigrations m
    -> (FilePath -> DB.UTCTimestamp -> txn UTCTime)
    -> ResourceT m (ApplyMigsResult b)
  runBlock act conn migBlock registerMig = do
    if blockInTxn migBlock
      then do
        res <-
          retryFold
              retryPol
              (\previousBlock RetryIteration { tryNumber } -> if tryNumber == 0
                then pure previousBlock
                else do
                  logDebugN "Re-reading migrations of this block from disk"
                  lift $ reReadBlock previousBlock
              )
              migBlock
            $ \blockFinal -> do
                logInfoN "BEGINning transaction"
                -- Apparently we need to use `withTxnIfNecessary` instead of `beginCommitTxnBracket`
                -- because otherwise the compiler doesn't know `txn ~ InTxnT (ResourceT m)` inside
                -- the nested block
                withTxnIfNecessary isolLvl conn $
                  ApplyMigsResult
                    <$> runMigs @txn
                          conn
                          singleTryPolicy
                          (allMigs (hoistBlockOfMigrationsInTxn $ hoistBlockOfMigrationsInTxn blockFinal)) -- Ouch once again..
                          registerMig -- We retry entire transactions, not individual statements
                    <*> act conn
        logInfoN "COMMITed transaction"
        pure res
      else
        ApplyMigsResult
        <$> runMigs conn retryPol (allMigs (hoistBlockOfMigrationsInTxn migBlock)) (\fp ts -> withTxnIfNecessary isolLvl conn $ registerMig fp ts)
        <*> withTxnIfNecessary isolLvl conn (act conn)

-- | This can be used as a last-action when applying migrations to
-- strict-check schemas, logging differences, success and throwing
-- an exception if they mismatch.
strictCheckLastAction
  :: (MonadUnliftIO m, MonadLogger m)
  => CoddSettings
  -> DbRep
  -> (  forall t
      . (InTxn t, MonadUnliftIO t, MonadLogger t, MonadUnliftIO t)
     => [BlockOfMigrations m]
     -> DB.Connection
     -> t ()
     )
strictCheckLastAction coddSettings expectedReps blocksOfMigs conn = do
  cksums <- readRepresentationsFromDbWithSettings coddSettings conn
  unless (all blockInTxn blocksOfMigs) $ do
    logWarnN
      "IMPORTANT: Due to the presence of no-txn or custom-connection migrations, all migrations have been applied. We'll run a schema check."
  logSchemasComparison cksums expectedReps
  when (cksums /= expectedReps) $ throwIO $ userError
    "Exiting. Database's schema differ from expected."

-- | This can be used as a last-action when applying migrations to
-- lax-check schemas, logging differences or success, but
-- _never_ throwing exceptions and returning the database schema.
laxCheckLastAction
  :: forall m
   . (MonadUnliftIO m, MonadLogger m)
  => CoddSettings
  -> DbRep
  -> (  forall t
      . (InTxn t, MonadUnliftIO t, MonadLogger t, MonadUnliftIO t)
     => [BlockOfMigrations m]
     -> DB.Connection
     -> t DbRep
     )
laxCheckLastAction coddSettings expectedReps _blocksOfMigs conn = do
  cksums <- readRepresentationsFromDbWithSettings coddSettings conn
  logSchemasComparison cksums expectedReps
  pure cksums

-- | A collection of consecutive migrations that has the same (in-txn, db-connection)
-- attributes.
data BlockOfMigrations m = BlockOfMigrations
  { allMigs     :: NonEmpty (AddedSqlMigration m)
  , reReadBlock :: m (BlockOfMigrations m)
  }
hoistBlockOfMigrationsInTxn
  :: forall m t. (Monad m, MonadTrans t, Monad (t m)) => BlockOfMigrations m -> BlockOfMigrations (t m)
hoistBlockOfMigrationsInTxn (BlockOfMigrations {..}) =
  let hoistedAllMigs     = hoistInTxn <$> allMigs
      hoistedReReadBlock = lift $ reReadBlock <&> hoistBlockOfMigrationsInTxn
  in  BlockOfMigrations { allMigs     = hoistedAllMigs
                        , reReadBlock = hoistedReReadBlock
                        }
blockInTxn :: BlockOfMigrations m -> Bool
blockInTxn (BlockOfMigrations (AddedSqlMigration { addedSqlMig } :| _) _) =
  migrationInTxn addedSqlMig

blockCustomConnInfo :: BlockOfMigrations m -> Maybe DB.ConnectInfo
blockCustomConnInfo (BlockOfMigrations (AddedSqlMigration { addedSqlMig } :| _) _)
  = migrationCustomConnInfo addedSqlMig

-- | Applies a single migration and returns the time when it finished being applied. Does not
-- itself register that the migration ran, only runs "afterMigRun" after applying the migration.
applySingleMigration
  :: forall m txn
   . (MonadUnliftIO m, MonadIO m, MonadLogger m, CanStartTxn m txn)
  => DB.Connection
  -> RetryPolicy
  -> (FilePath -> DB.UTCTimestamp -> txn UTCTime)
  -> TxnIsolationLvl
  -> AddedSqlMigration m
  -> m AppliedMigration
applySingleMigration conn statementRetryPol afterMigRun isolLvl (AddedSqlMigration sqlMig migTimestamp)
  = do
    let fn = migrationName sqlMig
    logInfoN $ "Applying " <> Text.pack fn

    let inTxn = if migrationInTxn sqlMig
          then InTransaction
          else NotInTransaction statementRetryPol

    multiQueryStatement_ inTxn conn $ migrationSql sqlMig
    timestamp <- withTxnIfNecessary isolLvl conn $ afterMigRun fn migTimestamp
    pure AppliedMigration { appliedMigrationName      = migrationName sqlMig
                          , appliedMigrationTimestamp = migTimestamp
                          , appliedMigrationAt        = timestamp
                          }

data MigrationRegistered = MigrationRegistered | MigrationNotRegistered

-- | Registers in the DB that a migration with supplied name and timestamp
--   has been applied and returns the DB's now() value (used for the "applied_at" column).
--   Fails if the codd_schema hasn't yet been created.
registerRanMigration
  :: forall m txn. (MonadUnliftIO m, MonadIO txn, CanStartTxn m txn)
  => DB.Connection
    -- ^ The default connection, not any other or this might fail.
  -> FilePath
  -> DB.UTCTimestamp
  -> m UTCTime
registerRanMigration conn fn migTimestamp = withTxnIfNecessary @txn @m (error "register missing isolLvl") conn $ DB.fromOnly <$> unsafeQuery1
  conn
  "INSERT INTO codd_schema.sql_migrations (migration_timestamp, name, applied_at) \
            \                            VALUES (?, ?, now()) \
            \                            RETURNING applied_at"
  (migTimestamp, fn)
