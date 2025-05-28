{-# LANGUAGE AllowAmbiguousTypes #-}

module Codd.Internal where

import Codd.Environment (CoddSettings (..))
import Codd.Internal.MultiQueryStatement
  ( SqlStatementException (..),
    StatementApplied (..),
    applyStatementStream,
    forceStreamConcurrently,
    singleStatement_,
    skipCountableRunnableStatements,
  )
import Codd.Internal.Retry
  ( RetryIteration (..),
    retryFold,
  )
import qualified Codd.InternalSchema.V1 as InternalSchema
import qualified Codd.InternalSchema.V2 as InternalSchema
import qualified Codd.InternalSchema.V3 as InternalSchema
import qualified Codd.InternalSchema.V4 as InternalSchema
import qualified Codd.InternalSchema.V5 as InternalSchema
import Codd.Logging
  ( CoddLogger,
    logError,
    logInfo,
    logInfoNoNewline,
    logWarn,
  )
import Codd.Parsing
  ( AddedSqlMigration (..),
    AppliedMigration (..),
    EnvVars (..),
    FileStream (..),
    MigrationApplicationStatus (..),
    ParsedSql (..),
    SqlMigration (..),
    hoistAddedSqlMigration,
    parseAddedSqlMigration,
    parseSqlPiecesStreaming,
    substituteEnvVarsInSqlPiecesStream,
  )
import Codd.Prelude (nubOrd)
import Codd.Query
  ( CanStartTxn,
    InTxn,
    InTxnT,
    NotInTxn,
    query,
    queryMay,
    queryServerMajorAndFullVersion,
    txnStatus,
    unsafeQuery1,
    withTransaction,
  )
import Codd.Representations
  ( DbRep,
    logSchemasComparison,
    readRepresentationsFromDbWithSettings,
    readRepsFromDisk,
  )
import Codd.Timing (prettyPrintDuration, timeAction)
import Codd.Types (TxnIsolationLvl (..))
import Control.Applicative (Alternative (..))
import Control.Monad
  ( foldM,
    forM,
    forM_,
    unless,
    void,
    when,
    (>=>),
  )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Resource (MonadThrow)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List (sortOn)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
  ( DiffTime,
    NominalDiffTime,
    UTCTime,
    diffTimeToPicoseconds,
  )
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Time as DB
import Database.PostgreSQL.Simple.ToRow as DB
import qualified Formatting as Fmt
import Streaming (Of (..))
import qualified Streaming.Prelude as Streaming
import System.Exit (exitFailure)
import System.FilePath
  ( takeFileName,
    (</>),
  )
import UnliftIO
  ( Concurrently (..),
    Exception,
    MonadUnliftIO,
    SomeException,
    hClose,
    modifyIORef',
    newIORef,
    readIORef,
    timeout,
    try,
    writeIORef,
  )
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (listDirectory)
import UnliftIO.Exception
  ( IOException,
    bracket,
    catchJust,
    handleJust,
    throwIO,
    tryJust,
  )
import UnliftIO.IO
  ( IOMode (ReadMode),
    openFile,
  )
import UnliftIO.Resource
  ( MonadResource,
    ReleaseKey,
    ResourceT,
    allocate,
    release,
    runResourceT,
  )
import Prelude hiding (readFile)

dbIdentifier :: Text -> DB.Query
dbIdentifier s = "\"" <> fromString (Text.unpack s) <> "\""

-- | Tries to connect until a connection succeeds or until a timeout, and returns a connection or throws an exception.
connectWithTimeout ::
  (MonadUnliftIO m) => DB.ConnectInfo -> DiffTime -> m DB.Connection
connectWithTimeout connStr timeLimit = do
  -- It feels bad using `timeout` for this given the asynchronous exception it can throw, but name resolution can block `DB.connect` for an arbitrary amount of time.
  -- The risk is we interrupt `DB.connect` after the operating system socket is open, which would mean we'd leave it hanging
  -- open for a while. This is probably not very probable, and not a terrible consequence as a single failing connection means
  -- we'll likely close codd itself pretty soon.
  -- The "decent" alternative (which I feel isn't worth it) here is to wrap postgresql-simple's `connectPostgreSQL` in a `onException` that closes the open handle on exception, or copy that code over ourselves and do that here. See https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/src/Database.PostgreSQL.Simple.Internal.html#connectPostgreSQL
  lastErrorRef <- newIORef (Nothing :: Maybe IOException)
  mconn <-
    timeout
      (fromInteger $ diffTimeToPicoseconds timeLimit `div` 1_000_000)
      (tryConnect lastErrorRef)
  lastError <- readIORef lastErrorRef
  case (mconn, lastError) of
    (Just conn, _) -> pure conn
    (Nothing, Just err) -> throwIO err
    (Nothing, Nothing) ->
      throwIO $ userError "Timeout connecting to database"
  where
    tryConnect lastErrorRef = do
      attempt <-
        tryJust
          ( \e ->
              if isServerNotAvailableError e
                then Just e
                else Nothing
          )
          $ liftIO (DB.connect connStr)
      case attempt of
        Right conn -> pure conn
        Left e -> do
          writeIORef lastErrorRef (Just e)
          threadDelay 100_000 -- Wait 100ms before retrying
          tryConnect lastErrorRef

-- | Tries to connect until a connection succeeds or until a timeout,
--   executes the supplied action and disposes of the opened Connection.
withConnection ::
  (MonadUnliftIO m) =>
  DB.ConnectInfo ->
  DiffTime ->
  (DB.Connection -> m a) ->
  m a
withConnection connStr timeLimit =
  bracket (connectWithTimeout connStr timeLimit) (liftIO . DB.close)

-- | Verifies if a libpq error means the server is not ready to accept connections yet,
-- either by not being listening at all or still being initializing.
isServerNotAvailableError :: IOException -> Bool
isServerNotAvailableError e =
  let err = Text.pack $ show e
   in "libpq"
        `Text.isInfixOf` err
        && ( "could not connect to server: Connection refused"
               `Text.isInfixOf` err
               || "Is the server running on that host and accepting"
                 `Text.isInfixOf` err
               || "server closed the connection"
                 `Text.isInfixOf` err
               || "the database system is starting up"
                 `Text.isInfixOf` err
           )

-- | Returns true for errors such as "permission denied for database xxx"
isPermissionDeniedError :: DB.SqlError -> Bool
isPermissionDeniedError e = DB.sqlState e == "42501"

data BootstrapCheck = BootstrapCheck
  { defaultConnAccessible :: Bool,
    coddSchemaVersion :: CoddSchemaVersion
  }

-- | Returns info on what kind of bootstrapping will be necessary,
-- waiting up to the time limit for postgres to be up before throwing
-- an exception.
checkNeedsBootstrapping ::
  (MonadUnliftIO m) => DB.ConnectInfo -> DiffTime -> m BootstrapCheck
checkNeedsBootstrapping connInfo connectTimeout =
  handleJust
    ( \e ->
        -- 1. No server available is a big "No", meaning we throw an exception.
        if isServerNotAvailableError e
          then Nothing
          -- 2. Maybe the default migration connection string doesn't work because:
          -- - The DB does not exist.
          -- - CONNECT rights not granted.
          -- - User doesn't exist.
          -- In any case, it's best to be conservative and consider any libpq errors
          -- here as errors that might just require bootstrapping.
          else
            if isLibPqError e
              then
                Just
                  BootstrapCheck
                    { defaultConnAccessible = False,
                      coddSchemaVersion = CoddSchemaDoesNotExist
                    }
              -- 3. Let other exceptions blow up
              else Nothing
    )
    pure
    $ withConnection
      connInfo
      connectTimeout
      (fmap (BootstrapCheck True) . detectCoddSchema)
  where
    isLibPqError :: IOException -> Bool
    isLibPqError e =
      let err = Text.pack $ show e in "libpq: failed" `Text.isInfixOf` err

data PendingMigrations m = PendingMigrations
  { pendingMigs :: [BlockOfMigrations m],
    -- | More information on what kind of bootstrapping is necessary.
    bootstrapCheck :: BootstrapCheck
  }

collectAndApplyMigrations ::
  ( MonadUnliftIO m,
    CoddLogger m,
    MonadThrow m,
    EnvVars m,
    NotInTxn m,
    txn ~ InTxnT (ResourceT m)
  ) =>
  ([BlockOfMigrations txn] -> DB.Connection -> txn a) ->
  CoddSettings ->
  -- | Instead of collecting migrations from disk according to codd settings, use these if they're defined.
  Maybe [AddedSqlMigration m] ->
  DiffTime ->
  m a
collectAndApplyMigrations lastAction settings@CoddSettings {migsConnString, sqlMigrations, txnIsolationLvl} mOverrideMigs connectTimeout =
  do
    let dbName = Text.pack $ DB.connectDatabase migsConnString
    let waitTimeInSecs :: Double = realToFrac connectTimeout
    logInfo $
      "Checking if database <MAGENTA>"
        <> dbName
        <> "</MAGENTA> is accessible with the configured connection string... (waiting up to <CYAN>"
        <> Text.pack (show @Int $ truncate waitTimeInSecs)
        <> "sec</CYAN>)"

    runResourceT $ do
      let migsToUse =
            maybe
              (Left sqlMigrations)
              (Right . map (hoistAddedSqlMigration lift))
              mOverrideMigs
      pendingMigs <-
        collectPendingMigrations
          migsConnString
          migsToUse
          txnIsolationLvl
          connectTimeout
      applyCollectedMigrations
        lastAction
        settings
        pendingMigs
        connectTimeout

-- | Applies the supplied migrations, running blocks of (in-txn, same-connection-string) migrations each within their own
-- separate transactions.
-- Behaviour here unfortunately is _really_ complex right now. Important notes:
-- - Iff there's a single (in-txn, same-connection-string) block of migrations *with* the default connection string,
--   then "actionAfter" runs in the same transaction (and thus in the same connection as well) as that block.
--   Otherwise it runs after all migrations, in an explicit transaction of
--   its own and in the default connection, which is not necessarily the same connection as the last migration's.
applyCollectedMigrations ::
  forall m a txn.
  ( MonadUnliftIO m,
    CoddLogger m,
    MonadResource m,
    NotInTxn m,
    txn ~ InTxnT m
  ) =>
  ([BlockOfMigrations txn] -> DB.Connection -> txn a) ->
  CoddSettings ->
  PendingMigrations m ->
  DiffTime ->
  m a
applyCollectedMigrations actionAfter CoddSettings {migsConnString = defaultConnInfo, retryPolicy, txnIsolationLvl} PendingMigrations {pendingMigs, bootstrapCheck} connectTimeout =
  do
    -- This function is complex because:
    -- 1. We need to open connections as late as possible due to bootstrapping.
    -- 2. We want to insert into codd.sql_migrations as early as possible even for custom-connection migrations.
    -- 3. While the item above says we want to register applied migrations as early as possible, we want to delay that sufficiently to allow for migrations that create codd's internal schema themselves, so as to allow dumps to be migrations, as long as there is no harm to atomicity.
    -- 4. We allow migrations to insert into codd.sql_migrations themselves (the reasonable use case here is having dumps as migrations), so we need to skip migrations if a migration registers they were applied.
    -- 5. When possible, we want to insert into codd.sql_migrations in the same transaction the migrations are running.
    let dbName = Text.pack $ DB.connectDatabase defaultConnInfo
        hoistedBlocks :: [BlockOfMigrations txn] =
          map (hoistBlockOfMigrations lift) pendingMigs

    -- Note: We could probably compound this Monad with StateT instead of using IORefs, but IIRC that creates issues
    -- with MonadUnliftIO.
    connsPerInfo <- newIORef (mempty :: [(DB.ConnectInfo, DB.Connection)])
    unregisteredButAppliedMigs <- newIORef (mempty :: [AppliedMigration])
    lastKnownCoddSchemaVersionRef <- newIORef $ coddSchemaVersion bootstrapCheck
    let coddSchemaUpToDate :: forall n. (MonadUnliftIO n) => n Bool
        coddSchemaUpToDate =
          (== maxBound) <$> readIORef lastKnownCoddSchemaVersionRef

        openConn :: DB.ConnectInfo -> m (ReleaseKey, DB.Connection)
        openConn cinfo = flip allocate DB.close $ do
          mConn <- lookup cinfo <$> readIORef connsPerInfo
          case mConn of
            Just conn -> pure conn
            Nothing -> do
              currentlyOpenConns <- readIORef connsPerInfo
              -- print
              --     $  "Connecting to (TODO: REDACT PASSWORD) "
              --     <> Text.pack (show cinfo)
              conn <- connectWithTimeout cinfo connectTimeout
              modifyIORef' connsPerInfo $ const $ (cinfo, conn) : currentlyOpenConns
              pure conn

        queryConn ::
          forall n.
          (MonadIO n) =>
          DB.ConnectInfo ->
          n (Maybe DB.Connection)
        queryConn cinfo = lookup cinfo <$> readIORef connsPerInfo

        -- \| Meant to check what the application status of an arbitrary migration is. Used because dumps-as-migrations can insert into codd.sql_migrations themselves,
        -- in which case we should detect that and skip migrations that were collected as pending at an earlier stage.
        -- Returns `Nothing` if the migration has not been applied at all.
        hasMigBeenApplied ::
          forall n.
          (MonadUnliftIO n) =>
          Maybe DB.Connection ->
          FilePath ->
          n (Maybe MigrationApplicationStatus)
        hasMigBeenApplied mDefaultDatabaseConn fp = do
          mDefaultConn <- queryConn defaultConnInfo
          lastKnownCoddSchemaVersion <- readIORef lastKnownCoddSchemaVersionRef
          apunregmigs <- readIORef unregisteredButAppliedMigs
          let appliedUnreg =
                List.find
                  (\apmig -> appliedMigrationName apmig == fp)
                  apunregmigs
          case appliedUnreg of
            Just m -> pure $ Just $ appliedMigrationStatus m
            Nothing ->
              -- We use the same connection as the one applying migrations if it's in the default database, otherwise we try to use the default conn if it's available
              case mDefaultDatabaseConn <|> mDefaultConn of
                Nothing -> pure Nothing
                Just connToUse -> do
                  -- If in-memory info says codd's internal schema does not exist or is not the latest, a migration may have created it or upgraded it and we're just not aware yet, so check that.
                  refinedCoddSchemaVersion <-
                    if lastKnownCoddSchemaVersion < maxBound
                      then do
                        actualVersion <-
                          detectCoddSchema
                            connToUse
                        modifyIORef'
                          lastKnownCoddSchemaVersionRef
                          (const actualVersion)
                        pure actualVersion
                      else
                        pure lastKnownCoddSchemaVersion
                  if refinedCoddSchemaVersion >= CoddSchemaV5
                    then
                      queryMay
                        connToUse
                        "SELECT num_applied_statements, no_txn_failed_at FROM codd.sql_migrations WHERE name=?"
                        (DB.Only fp)
                    else
                      if refinedCoddSchemaVersion >= CoddSchemaV3
                        then
                          queryMay
                            connToUse
                            "SELECT num_applied_statements, no_txn_failed_at FROM codd_schema.sql_migrations WHERE name=?"
                            (DB.Only fp)
                        else
                          if refinedCoddSchemaVersion >= CoddSchemaV1
                            then do
                              queryMay
                                connToUse
                                "SELECT 0, NULL::timestamptz FROM codd_schema.sql_migrations WHERE name=?"
                                (DB.Only fp)
                            else pure Nothing

        createCoddSchemaAndFlushPendingMigrationsDefaultConnection ::
          DB.Connection -> txn ()
        createCoddSchemaAndFlushPendingMigrationsDefaultConnection defaultConn =
          do
            csUpToDate <- coddSchemaUpToDate
            unless csUpToDate $ do
              logInfo
                "Creating or updating <MAGENTA>codd's internal schema</MAGENTA>..."
              createCoddSchema @txn
                maxBound
                txnIsolationLvl
                defaultConn
              modifyIORef'
                lastKnownCoddSchemaVersionRef
                (const maxBound)
            apmigs <- readIORef unregisteredButAppliedMigs
            withTransaction @txn txnIsolationLvl defaultConn $
              forM_ apmigs $
                \AppliedMigration {..} ->
                  registerRanMigration @txn
                    defaultConn
                    txnIsolationLvl
                    appliedMigrationName
                    appliedMigrationTimestamp
                    (SpecificTime appliedMigrationAt)
                    appliedMigrationDuration
                    appliedMigrationStatus
                    ( SpecificIds
                        appliedMigrationTxnId
                        appliedMigrationConnId
                    )
            modifyIORef' unregisteredButAppliedMigs $ const []

        createCoddSchemaAndFlushPendingMigrations :: m ()
        createCoddSchemaAndFlushPendingMigrations = do
          mDefaultConn <- queryConn defaultConnInfo
          csUpToDate <- coddSchemaUpToDate
          case mDefaultConn of
            Just defaultConn -> do
              unless csUpToDate $ do
                logInfo
                  "Creating or updating <MAGENTA>codd's internal schema</MAGENTA>..."
                createCoddSchema @txn
                  maxBound
                  txnIsolationLvl
                  defaultConn
                modifyIORef'
                  lastKnownCoddSchemaVersionRef
                  (const maxBound)
              apmigs <- readIORef unregisteredButAppliedMigs
              withTransaction @txn txnIsolationLvl defaultConn $
                forM_ apmigs $
                  \AppliedMigration {..} ->
                    registerRanMigration @txn
                      defaultConn
                      txnIsolationLvl
                      appliedMigrationName
                      appliedMigrationTimestamp
                      (SpecificTime appliedMigrationAt)
                      appliedMigrationDuration
                      appliedMigrationStatus
                      ( SpecificIds
                          appliedMigrationTxnId
                          appliedMigrationConnId
                      )
              modifyIORef' unregisteredButAppliedMigs $ const []
            Nothing -> pure ()

        -- \| The function used to register applied migrations for in-txn migrations.
        -- This will use same transaction as the one used to apply the migrations to insert into codd.sql_migrations as long as the block's connection is on the default database (even under a different user) and codd's internal schema has been created, and will otherwise only register them in-memory so they're applied at the earliest future opportunity.
        registerAppliedInTxnMig ::
          DB.Connection ->
          DB.ConnectInfo ->
          FilePath ->
          DB.UTCTimestamp ->
          DiffTime ->
          MigrationApplicationStatus ->
          txn ()
        registerAppliedInTxnMig blockConn blockConnInfo appliedMigrationName appliedMigrationTimestamp appliedMigrationDuration appliedMigrationStatus =
          do
            csReady <- coddSchemaUpToDate
            -- If we are on the default database, we can insert into codd.sql_migrations with any user
            if DB.connectDatabase blockConnInfo
              == DB.connectDatabase defaultConnInfo
              && csReady
              then
                void $
                  registerRanMigration @txn
                    blockConn
                    txnIsolationLvl
                    appliedMigrationName
                    appliedMigrationTimestamp
                    NowInPostgresTime
                    appliedMigrationDuration
                    appliedMigrationStatus
                    OfCurrentTransaction
              else
                -- If not in the default database, we have to wait until after COMMIT to register that migrations were applied
                do
                  (appliedMigrationAt, appliedMigrationTxnId, appliedMigrationConnId) <-
                    unsafeQuery1
                      blockConn
                      "SELECT clock_timestamp(), txid_current(), pg_backend_pid()"
                      ()
                  modifyIORef' unregisteredButAppliedMigs $ \apmigs ->
                    apmigs
                      ++ [ AppliedMigration
                             { appliedMigrationName,
                               appliedMigrationTimestamp,
                               appliedMigrationAt,
                               appliedMigrationDuration,
                               appliedMigrationStatus,
                               appliedMigrationTxnId,
                               appliedMigrationConnId
                             }
                         ]

        -- \| The function used to register fully or partially applied no-txn migrations.
        -- This will account for the possibility that the default connection string still isn't accessible and that codd's internal schema still hasn't been created by storing in memory that some migrations were applied but not registered, leaving flushing of those to the database to a later opportunity.
        registerAppliedNoTxnMig ::
          DB.Connection ->
          DB.ConnectInfo ->
          FilePath ->
          DB.UTCTimestamp ->
          DiffTime ->
          MigrationApplicationStatus ->
          m ()
        registerAppliedNoTxnMig blockConn blockConnInfo appliedMigrationName appliedMigrationTimestamp appliedMigrationDuration appliedMigrationStatus =
          do
            csReady <- coddSchemaUpToDate
            case (appliedMigrationStatus, csReady) of
              (NoTxnMigrationFailed _, False) -> do
                -- Super duper ultra extra special case: we try to create codd's internal schema as a partially-run no-txn migration may have applied statements that make the default connection string accessible. The same isn't possible with in-txn migrations.
                -- This will increase the delay between retry intervals beyond what the user has specified since we're adding a bit of a timeout to each retry of failed no-txn migrations. Since this is an extremely rare error case, it probably doesn't
                -- matter too much what we do. I have questions if we should even support this, to be honest. Hacky stuff below:
                void
                  $ try @m @SomeException
                  $ withConnection
                    defaultConnInfo
                    (min 0.3 connectTimeout)
                  $ \_conn -> do
                    void $ openConn defaultConnInfo
                    createCoddSchemaAndFlushPendingMigrations
              _ -> pure ()
            mDefaultConn <- queryConn defaultConnInfo
            case ( mDefaultConn,
                   DB.connectDatabase blockConnInfo
                     == DB.connectDatabase defaultConnInfo
                     && csReady
                 ) of
              (Nothing, False) -> do
                -- No default connection available and migrations running on non-default database
                (appliedMigrationAt, appliedMigrationTxnId, appliedMigrationConnId) <-
                  unsafeQuery1
                    blockConn
                    "SELECT clock_timestamp(), txid_current(), pg_backend_pid()"
                    ()
                modifyIORef' unregisteredButAppliedMigs $
                  \apmigs ->
                    apmigs
                      ++ [ AppliedMigration
                             { appliedMigrationName,
                               appliedMigrationTimestamp,
                               appliedMigrationAt,
                               appliedMigrationDuration,
                               appliedMigrationStatus,
                               appliedMigrationTxnId,
                               appliedMigrationConnId
                             }
                         ]
              (Just defaultConn, False) -> do
                -- Running migrations on non-default database, but default connection is available
                (appliedMigrationTxnId, appliedMigrationConnId) <-
                  unsafeQuery1
                    blockConn
                    "SELECT txid_current(), pg_backend_pid()"
                    ()
                void
                  $ withTransaction @txn
                    txnIsolationLvl
                    defaultConn
                  $ registerRanMigration @txn
                    defaultConn
                    txnIsolationLvl
                    appliedMigrationName
                    appliedMigrationTimestamp
                    NowInPostgresTime
                    appliedMigrationDuration
                    appliedMigrationStatus
                    ( SpecificIds
                        appliedMigrationTxnId
                        appliedMigrationConnId
                    )
              (_, True) -> do
                -- Migrations running on default database means we can register them here if codd's internal schema exists and is up to date
                void
                  $ withTransaction @txn
                    txnIsolationLvl
                    blockConn
                  $ registerRanMigration @txn
                    blockConn
                    txnIsolationLvl
                    appliedMigrationName
                    appliedMigrationTimestamp
                    NowInPostgresTime
                    appliedMigrationDuration
                    appliedMigrationStatus
                    OfCurrentTransaction

    singleInTxnBlockResult :: Maybe a <-
      foldM
        ( \_ block -> do
            let cinfo =
                  fromMaybe defaultConnInfo (blockCustomConnInfo block)
                isDefaultConn = cinfo == defaultConnInfo
                connUsesDefaultDb =
                  DB.connectDatabase cinfo
                    == DB.connectDatabase defaultConnInfo
            (_, conn) <- openConn cinfo

            case block of
              BlockInTxn inTxnBlock -> do
                unless
                  isDefaultConn
                  createCoddSchemaAndFlushPendingMigrations
                runInTxnBlock
                  ( do
                      -- Creating codd's internal schema inside the same transaction as migrations increases atomicity
                      -- and also allows for migrations themselves to create codd's internal schema, which is useful because then users
                      -- can have dumps as migrations
                      when isDefaultConn $
                        createCoddSchemaAndFlushPendingMigrationsDefaultConnection
                          conn
                      if isOneShotApplication
                        defaultConnInfo
                        pendingMigs
                        then
                          Just <$> actionAfter hoistedBlocks conn
                        else
                          pure Nothing
                  )
                  conn
                  inTxnBlock
                  (registerAppliedInTxnMig conn cinfo)
                  ( hasMigBeenApplied
                      ( if connUsesDefaultDb
                          then Just conn
                          else Nothing
                      )
                  )
              BlockNoTxn noTxnBlock -> do
                createCoddSchemaAndFlushPendingMigrations
                runNoTxnMig
                  conn
                  noTxnBlock
                  (registerAppliedNoTxnMig conn cinfo)
                  ( hasMigBeenApplied
                      ( if connUsesDefaultDb
                          then Just conn
                          else Nothing
                      )
                  )
        )
        Nothing
        pendingMigs

    actAfterResult <- case singleInTxnBlockResult of
      Just result -> pure result
      Nothing -> do
        -- It is possible to have only non-default-connection-string migrations, or to have in-txn migrations running on a non-default database last.
        -- In those cases, we assume the default-connection-string will be valid after those migrations
        -- and use that to register all applied migrations and then run "actionAfter".
        (_, defaultConn) <- openConn defaultConnInfo
        createCoddSchemaAndFlushPendingMigrations
        withTransaction txnIsolationLvl defaultConn $
          actionAfter hoistedBlocks defaultConn

    logInfo $
      "<GREEN>Successfully</GREEN> applied all migrations to </GREEN><MAGENTA>"
        <> dbName
        <> "</MAGENTA>"
    return actAfterResult
  where
    runInTxnBlock ::
      txn b ->
      DB.Connection ->
      ConsecutiveInTxnMigrations m ->
      ( FilePath ->
        DB.UTCTimestamp ->
        DiffTime ->
        MigrationApplicationStatus ->
        txn ()
      ) ->
      -- \^ Using the `txn` monad is right: registering applied migrations happens in the same connection that applies migrations if that is a default-database connection, and otherwise will be scheduled to be inserted into codd.sql_migrations in the first future opportunity, meaning when this function is called it's merely an in-memory operation, which can also run in `txn`.
      (FilePath -> txn (Maybe MigrationApplicationStatus)) ->
      -- \^ Function to check if a migration has been applied
      m b
    runInTxnBlock act conn migBlock registerMig hasMigBeenApplied =
      -- Naturally, we retry entire in-txn block transactions on error, not individual statements or individual migrations
      retryFold
        retryPolicy
        ( \previousBlock RetryIteration {tryNumber} ->
            if tryNumber == 0
              then pure previousBlock
              else reReadBlock previousBlock
        )
        migBlock
        ( \case
            Left lastEx -> do
              logError
                "Failed after all configured retries. Giving up."
              throwIO lastEx
            Right ret -> pure ret
        )
        $ \blockFinal -> do
          logInfo "<MAGENTA>BEGIN</MAGENTA>ning transaction"
          withTransaction txnIsolationLvl conn $ do
            let hoistedMigs :: NonEmpty (AddedSqlMigration txn)
                hoistedMigs =
                  hoistAddedSqlMigration lift
                    <$> inTxnMigs blockFinal
            errorOrOk <- forMExcept hoistedMigs $ \mig -> do
              let migname = migrationName $ addedSqlMig mig
              appStatus <- hasMigBeenApplied migname
              case appStatus of
                Just (NoTxnMigrationFailed _) ->
                  error $
                    migname
                      ++ " is an in-txn migration, yet when I look in codd.sql_migrations it's registered as a partially applied no-txn migration. If you aren't messing with codd's internals, this is a bug. Otherwise you're trying to do something unsupported."
                Just _ -> do
                  logInfo
                    ( "<YELLOW>Skipping</YELLOW> "
                        <> Text.pack migname
                    )
                  pure (Right ())
                Nothing ->
                  applySingleMigration
                    conn
                    registerMig
                    NoSkipStatements
                    mig
            case errorOrOk of
              Left e -> do
                liftIO $ DB.rollback conn
                logInfo
                  "<MAGENTA>ROLLBACK</MAGENTA>ed transaction"
                pure $ Left e
              Right () -> do
                res <- act
                -- Also catch exceptions on COMMIT so they're treated as a retriable error
                commitE <-
                  try $
                    liftIO $
                      DB.execute_
                        conn
                        "COMMIT"
                case commitE of
                  Left e -> do
                    let sqlEx =
                          SqlStatementException "COMMIT" e
                    logError (Text.pack $ show sqlEx)
                    logError "COMMIT failed"
                    pure $ Left sqlEx
                  Right _ -> do
                    logInfo
                      "<MAGENTA>COMMIT</MAGENTA>ed transaction"
                    pure $ Right res

    runNoTxnMig ::
      DB.Connection ->
      SingleNoTxnMigration m ->
      ( FilePath ->
        DB.UTCTimestamp ->
        DiffTime ->
        MigrationApplicationStatus ->
        m ()
      ) ->
      -- \^ This is `m` instead of `txn` and is correct. The reason is that there is no transaction opened by codd for no-txn migrations, and so the function that registers applied migrations needs to start its own transaction.
      (FilePath -> m (Maybe MigrationApplicationStatus)) ->
      -- \^ Function to check if a migration has been applied
      m (Maybe x)
    runNoTxnMig conn mig registerMig hasMigBeenApplied = do
      let migname = migrationName $ addedSqlMig $ singleNoTxnMig mig
      retryFold
        retryPolicy
        ( \(previousMig, _) RetryIteration {lastError} ->
            case lastError of
              Nothing -> do
                -- We recheck application status because dumps-as-migrations can insert into codd.sql_migrations themselves.
                recheckedAppStatus <- hasMigBeenApplied migname
                case recheckedAppStatus of
                  Just (MigrationAppliedSuccessfully _) -> do
                    -- Here a migration inserted into codd.sql_migrations itself, claiming the migration has been applied.
                    -- So we signal it must be skipped with the `Left ()` value
                    pure (previousMig, Left ())
                  _ -> do
                    let numStmtsApplied =
                          case recheckedAppStatus of
                            Nothing ->
                              numStatementsAlreadyApplied
                                mig
                            Just (NoTxnMigrationFailed n) ->
                              n
                    when (numStmtsApplied > 0) $
                      logWarn $
                        "Resuming application of partially applied <YELLOW>no-txn</YELLOW> migration <MAGENTA>"
                          <> Text.pack migname
                          <> "</MAGENTA>. Skipping the first "
                          <> Fmt.sformat Fmt.int numStmtsApplied
                          <> " SQL statements, which have already been applied, and starting application from the "
                          <> Fmt.sformat
                            Fmt.ords
                            (numStmtsApplied + 1)
                          <> " statement"
                    pure (previousMig, Right numStmtsApplied)
              Just NoTxnMigrationApplicationFailure {noTxnMigAppliedStatements} ->
                do
                  logWarn $
                    "Skipping the first "
                      <> Fmt.sformat
                        Fmt.int
                        noTxnMigAppliedStatements
                      <> " SQL statements, which have already been applied, and starting application from the "
                      <> Fmt.sformat
                        Fmt.ords
                        (noTxnMigAppliedStatements + 1)
                      <> " statement"
                  freshBlock <- reReadMig previousMig
                  pure
                    ( freshBlock,
                      Right noTxnMigAppliedStatements
                    )
        )
        (mig, Right 0)
        ( \case
            Left lastErr -> do
              logError
                "Failed after all configured retries. Giving up."
              -- The warning below would be very nice to have, but we don't want it appearing for new users of codd when they're trying `codd add` for the first time. We should know if this migration application happens during `codd add` and omit the warning below.
              -- logWarn "IMPORTANT:\n\
              --        \ If this is a database you care about and can't simply recreate (e.g. a Production database), then read the following _very carefully_:\n\
              --        \ A no-txn migration failed to be applied completely but may have had some of its statements applied. Your database may have been left in an intermediary state.\n\
              --        \ If you think this is a temporary error and that resuming from the exact statement inside the no-txn migration that failed might work, you can just run `codd up` and codd _will resume_ migration application precisely from that last failed statement.\n\
              --        \ If you're going to do that, however, do _not_ edit the migration as changing the position of the failed statement inside it will make codd silently continue from the wrong place.\n\
              --        \ But if this is not going away merely by resuming application from the last failed statement, one other option you have is to look at the last error above to see how many statements codd applied and which statement failed. This should help you pinpoint the precise failing statement inside the migration, even if it might not be entirely obvious how codd counts statements internally. \n\
              --        \ Once you know which statement that is, you can edit the migration and remove that failing statement and all others that come after it. You can then rewrite that part of the migration in a way you think will fix the problem. Just make sure you don't change anything before it. After that you can run `codd up` and codd will resume application after skipping the statements that had been applied, meaning it will resume from the first statement in the rewritten part of the migration."
              throwIO lastErr
            Right ret -> pure ret
        )
        $ \(migFinal, skipInstr) -> do
          case skipInstr of
            Left () -> do
              logInfo
                ( "<YELLOW>Skipping</YELLOW> " <> Text.pack migname
                )
              pure (Right Nothing)
            Right numStmtsToSkip ->
              fmap (const Nothing)
                <$> applySingleMigration
                  conn
                  registerMig
                  (SkipStatementsNoTxn numStmtsToSkip)
                  (singleNoTxnMig migFinal)

data CoddSchemaVersion
  = CoddSchemaDoesNotExist
  | CoddSchemaV1
  | -- | V2 includes duration of each migration's application
    CoddSchemaV2
  | -- | V3 includes the number of SQL statements applied per migration, allowing codd to resume application of even failed no-txn migrations correctly
    CoddSchemaV3
  | -- | V4 includes the ID of the transaction and of the connection applying each migration
    CoddSchemaV4
  | -- | V5 includes background migration functions and tables, and renames codd_schema to codd
    CoddSchemaV5
  deriving stock (Bounded, Enum, Eq, Ord, Show)

detectCoddSchema :: (MonadIO m) => DB.Connection -> m CoddSchemaVersion
detectCoddSchema conn = do
  qualifiedCols :: [(Text, Text)] <-
    query
      conn
      "select nspname, attname from pg_attribute join pg_class on attrelid=pg_class.oid join pg_namespace on relnamespace=pg_namespace.oid where relname='sql_migrations' AND nspname IN ('codd_schema', 'codd') AND attnum >= 1 AND NOT attisdropped order by attnum"
      ()
  let cols = map snd qualifiedCols
      schemaName = nubOrd $ map fst qualifiedCols
  case schemaName of
    [] -> pure CoddSchemaDoesNotExist
    ["codd"] -> pure CoddSchemaV5
    ["codd_schema"] ->
      case cols of
        ["id", "migration_timestamp", "applied_at", "name"] ->
          pure CoddSchemaV1
        ["id", "migration_timestamp", "applied_at", "name", "application_duration"] ->
          pure CoddSchemaV2
        ["id", "migration_timestamp", "applied_at", "name", "application_duration", "num_applied_statements", "no_txn_failed_at"] ->
          pure CoddSchemaV3
        ["id", "migration_timestamp", "applied_at", "name", "application_duration", "num_applied_statements", "no_txn_failed_at", "txnid", "connid"] ->
          pure CoddSchemaV4
        _ ->
          error $
            "Internal codd error. Unless you've manually modified the codd_schema.sql_migrations table, this is a bug in codd. Please report it and include the following as column names in your report: "
              ++ show cols
    _ ->
      error
        "Internal codd error, may have found more than one codd schema. Do you have a 'codd' or 'codd_schema' in your application? That might interfere with codd. Otherwise please report a bug in codd"

createCoddSchema ::
  forall txn m.
  (MonadUnliftIO m, CanStartTxn m txn, MonadUnliftIO txn) =>
  -- | Desired schema version. This should always be `maxBound` in the app; it's meant to assume other values only in tests
  CoddSchemaVersion ->
  TxnIsolationLvl ->
  DB.Connection ->
  m ()
createCoddSchema targetVersion txnIsolationLvl conn =
  withTransaction @txn txnIsolationLvl conn $ do
    currentSchemaVersion <- detectCoddSchema conn
    catchJust
      (\e -> if isPermissionDeniedError e then Just () else Nothing)
      (go currentSchemaVersion)
      ( \() ->
          throwIO $
            userError
              "Not enough permissions to create or update codd's internal schema. Please check that your default connection string can create tables, sequences and GRANT them permissions."
      )
  where
    go currentSchemaVersion
      | targetVersion < currentSchemaVersion =
          error
            "Cannot migrate newer codd's internal schema version to an older version. Please report this as a bug in codd."
      | targetVersion == currentSchemaVersion =
          pure ()
      | otherwise =
          do
            case currentSchemaVersion of
              CoddSchemaDoesNotExist -> InternalSchema.createInternalSchemaV1 conn
              CoddSchemaV1 -> InternalSchema.migrateInternalSchemaV1ToV2 conn
              CoddSchemaV2 -> InternalSchema.migrateInternalSchemaV2ToV3 conn
              CoddSchemaV3 -> InternalSchema.migrateInternalSchemaV3ToV4 conn
              CoddSchemaV4 -> InternalSchema.migrateInternalSchemaV4ToV5 conn
              CoddSchemaV5 -> pure ()

            -- `succ` is a partial function, but it should never throw in this context
            go (succ currentSchemaVersion)

-- | Collects pending migrations and separates them according to being bootstrap
--   or not.
collectPendingMigrations ::
  forall m.
  ( MonadUnliftIO m,
    CoddLogger m,
    MonadResource m,
    MonadThrow m,
    NotInTxn m,
    EnvVars m
  ) =>
  DB.ConnectInfo ->
  Either [FilePath] [AddedSqlMigration m] ->
  TxnIsolationLvl ->
  DiffTime ->
  m (PendingMigrations m)
collectPendingMigrations defaultConnString sqlMigrations txnIsolationLvl connectTimeout =
  do
    bootCheck <- checkNeedsBootstrapping defaultConnString connectTimeout
    pendingMigs <- collect bootCheck

    unless (defaultConnAccessible bootCheck) $ do
      logInfo
        "Default connection string is not accessible. Codd will run in bootstrap mode, expecting the first migrations will contain custom connection strings and will create/bootstrap the database."

      -- The specific error below isn't necessary at this stage, but it's much more informative
      -- than the errors we'd generate further ahead.
      let bootstrapMigBlocks =
            takeWhile hasNonDefaultConnectionString pendingMigs
      when (null bootstrapMigBlocks) $ do
        logError
          "The earliest existing migration has no custom connection string or there are no migrations at all. Exiting."
        liftIO exitFailure

    pure $ PendingMigrations pendingMigs bootCheck
  where
    hasNonDefaultConnectionString block =
      let mConnInfo = migrationCustomConnInfo $ addedSqlMig $ case block of
            BlockInTxn (ConsecutiveInTxnMigrations (m1 :| _) _) -> m1
            BlockNoTxn (SingleNoTxnMigration m _ _) -> m
       in case mConnInfo of
            Nothing -> False
            Just connInfo ->
              DB.connectDatabase defaultConnString
                /= DB.connectDatabase connInfo
    collect bootCheck = do
      logInfoNoNewline "Looking for pending migrations..."
      migsAlreadyApplied :: Map FilePath MigrationApplicationStatus <-
        Map.fromList <$> case coddSchemaVersion bootCheck of
          CoddSchemaDoesNotExist -> pure []
          v -> do
            rows <-
              withConnection
                defaultConnString
                connectTimeout
                ( \conn ->
                    withTransaction @(InTxnT m) txnIsolationLvl conn $
                      do
                        case v of
                          CoddSchemaV5 ->
                            query
                              conn
                              "SELECT name, no_txn_failed_at IS NULL, COALESCE(num_applied_statements, 0) FROM codd.sql_migrations"
                              ()
                          CoddSchemaV4 ->
                            query
                              conn
                              "SELECT name, no_txn_failed_at IS NULL, COALESCE(num_applied_statements, 0) FROM codd_schema.sql_migrations"
                              ()
                          CoddSchemaV3 ->
                            query
                              conn
                              "SELECT name, no_txn_failed_at IS NULL, COALESCE(num_applied_statements, 0) FROM codd_schema.sql_migrations"
                              ()
                          CoddSchemaV2 ->
                            query
                              conn
                              "SELECT name, TRUE, 0 FROM codd_schema.sql_migrations"
                              ()
                          CoddSchemaV1 ->
                            query
                              conn
                              "SELECT name, TRUE, 0 FROM codd_schema.sql_migrations"
                              ()
                )
            pure $
              map
                ( \(name, succeeded, numStmts) ->
                    if succeeded
                      then (name, MigrationAppliedSuccessfully numStmts)
                      else (name, NoTxnMigrationFailed numStmts)
                )
                rows

      blocksOfPendingMigs <-
        parseMigrationFiles
          migsAlreadyApplied
          sqlMigrations
      logInfo $
        " [<CYAN>"
          <> Fmt.sformat
            Fmt.int
            (sum $ map (NE.length . allMigs) blocksOfPendingMigs)
          <> " found</CYAN>]"
      forM_ (concatMap (NE.toList . allMigs) blocksOfPendingMigs) $ \mig -> do
        case migrationSql $ addedSqlMig mig of
          UnparsedSql _ ->
            logWarn $
              Text.pack (migrationName $ addedSqlMig mig)
                <> " is not to be parsed and thus will be treated in its entirety as an in-txn migration with a single statement, without support for COPY."
          _ -> pure ()
      pure blocksOfPendingMigs

-- | Opens a UTF-8 file allowing it to be read in streaming fashion. This function delays opening the file
-- until the returned Stream's first chunk is forced, and closes the file immediately after the returned Stream
-- is fully consumed.
delayedOpenStreamFile :: (MonadResource m) => FilePath -> m (FileStream m)
delayedOpenStreamFile filePath = do
  -- We can't use Streaming.fromHandle because it uses hGetLine, which removes '\n' from lines it
  -- reads, but does not append '\n' to the Strings it returns, making it impossible to know
  -- whether the last line had a '\n' after it.
  -- See https://hackage.haskell.org/package/streaming-0.2.3.1/docs/src/Streaming.Prelude.html#fromHandle
  -- So we copied streaming's implementation and modified it slightly.
  releaseKey <- newIORef Nothing
  pure $
    FileStream
      { filePath,
        releaseKey,
        fileStream = lazyStream releaseKey
      }
  where
    -- \| Lazy stream because it waits until the file is demanded to open it in the first place
    lazyStream releaseKeyIORef = do
      (releaseKey, handle) <-
        lift $
          allocate (openFile filePath ReadMode) hClose
      writeIORef releaseKeyIORef (Just releaseKey)
      go releaseKeyIORef handle
    go rkioref h = do
      str <- liftIO $ Text.hGetChunk h
      case str of
        "" -> do
          hClose h
          lift $ writeIORef rkioref Nothing
        _ -> do
          Streaming.yield str
          go rkioref h

closeFileStream :: (MonadResource m) => FileStream m -> m ()
closeFileStream (FileStream _ releaseKey _) = do
  mrkey <- readIORef releaseKey
  forM_ mrkey release

-- | Returns all migrations on the supplied folders except for the explicitly supplied ones.
listMigrationsFromDisk ::
  (MonadIO m) =>
  -- | The folders where to look for migrations.
  [FilePath] ->
  -- | Migration filenames (without their directories) to exclude from returned list.
  [FilePath] ->
  m [FilePath]
listMigrationsFromDisk sqlDirs excludeList = do
  fmap (sortOn takeFileName . concat) $ forM sqlDirs $ \dir -> do
    filesInDir <- listDirectory dir
    return $
      map (dir </>) $
        filter
          (\fn -> ".sql" `List.isSuffixOf` fn && fn `notElem` excludeList)
          filesInDir

parseMigrationFiles ::
  forall m.
  ( MonadUnliftIO m,
    CoddLogger m,
    MonadResource m,
    MonadThrow m,
    EnvVars m
  ) =>
  -- | Migrations fully or partially applied (the latter can only happen with no-txn migrations) and how many statements have been applied per migration
  Map FilePath MigrationApplicationStatus ->
  Either [FilePath] [AddedSqlMigration m] ->
  m [BlockOfMigrations m]
parseMigrationFiles migsApplied sqlMigrations = do
  pendingParsedMigrations ::
    [ ( Either String (FileStream m),
        AddedSqlMigration m,
        Maybe Int
      )
    ] <-
    -- \^ How many statements to skip

    either
      (listPendingFromDisk >=> readFromDisk)
      (pure . readFromMemory . listPendingFromMemory)
      sqlMigrations

  -- Group consecutive in-txn migrations with the same connection string together for atomic application
  pure $
    NE.groupBy
      ( \(_, AddedSqlMigration m1 _, _) (_, AddedSqlMigration m2 _, _) ->
          migrationInTxn m1
            && migrationInTxn m2
            && migrationCustomConnInfo m1
              == migrationCustomConnInfo m2
      )
      pendingParsedMigrations
      <&> \migs ->
        let (_, firstMig, mNumStmtsApplied) = NE.head migs
         in if migrationInTxn (addedSqlMig firstMig)
              then
                BlockInTxn
                  ConsecutiveInTxnMigrations
                    { inTxnMigs = snd3 <$> migs,
                      reReadBlock = reRead migs
                    }
              else
                BlockNoTxn
                  SingleNoTxnMigration
                    { singleNoTxnMig = firstMig,
                      reReadMig =
                        reReadNoTxn migs $
                          fromMaybe 0 mNumStmtsApplied,
                      numStatementsAlreadyApplied =
                        fromMaybe
                          0
                          mNumStmtsApplied
                    }
  where
    snd3 (_, v, _) = v
    reRead oldMigsAndPaths = do
      -- Close handles of all migrations in the block, re-open and read+parse them
      filePaths <- forM oldMigsAndPaths $ \case
        (Left _memStream, _, _) ->
          error "Re-reading in-memory streams is not yet implemented"
        (Right fileStream, _, _) ->
          closeFileStream fileStream >> pure (filePath fileStream)
      newMigs <- readFromDisk filePaths
      pure
        ConsecutiveInTxnMigrations
          { inTxnMigs = snd3 <$> newMigs,
            reReadBlock = reRead newMigs
          }
    -- \| TODO: This is a near duplicate of `reRead`. Improve this.
    reReadNoTxn oldMigsAndPaths numStatementsAlreadyApplied = do
      -- Close handles of all migrations in the block, re-open and read+parse them
      filePaths <- forM oldMigsAndPaths $ \case
        (Left _memStream, _, _) ->
          error "Re-reading in-memory streams is not yet implemented"
        (Right fileStream, _, _) ->
          closeFileStream fileStream >> pure (filePath fileStream)
      newMigs <- readFromDisk filePaths
      pure
        SingleNoTxnMigration
          { singleNoTxnMig = snd3 (NE.head newMigs),
            reReadMig = reReadNoTxn newMigs numStatementsAlreadyApplied,
            numStatementsAlreadyApplied
          }
    readFromMemory ::
      [AddedSqlMigration m] ->
      [(Either String (FileStream m), AddedSqlMigration m, Maybe Int)]
    readFromMemory ams =
      map
        ( \asqlmig@(AddedSqlMigration mig _) ->
            ( Left $ migrationName mig,
              asqlmig,
              case Map.lookup (migrationName mig) migsApplied of
                Just (NoTxnMigrationFailed n) -> Just n
                _ -> Nothing
            )
        )
        $ sortOn (\(AddedSqlMigration _ ts) -> ts) ams
    migNamesFullyApplied =
      [ name
        | (name, status) <- Map.toList migsApplied,
          case status of
            MigrationAppliedSuccessfully _ -> True
            NoTxnMigrationFailed _ -> False
      ]
    listPendingFromMemory =
      filter
        ( \(AddedSqlMigration mig _) ->
            migrationName mig `notElem` migNamesFullyApplied
        )
    listPendingFromDisk sqlDirs =
      listMigrationsFromDisk sqlDirs migNamesFullyApplied

    readFromDisk ::
      forall t.
      (Traversable t) =>
      t FilePath ->
      m
        ( t
            ( Either String (FileStream m),
              AddedSqlMigration m,
              Maybe Int
            )
        )
    readFromDisk pendingSqlMigrationFiles =
      forM pendingSqlMigrationFiles $ \pendingMigrationPath -> do
        fs :: FileStream m <- delayedOpenStreamFile pendingMigrationPath
        let fn = takeFileName $ filePath fs
        parsedMig <- parseAddedSqlMigration fn fs
        let numAppliedStatementsNoTxn = case Map.lookup fn migsApplied of
              Just (NoTxnMigrationFailed n) -> Just n
              _ -> Nothing

        fmap (\(a, b) -> (a, b, numAppliedStatementsNoTxn)) $
          case parsedMig of
            Left err -> do
              throwIO $
                userError $
                  "Fatal error parsing migration '"
                    <> fn
                    <> "': "
                    <> err
            Right asqlmig@(AddedSqlMigration mig _) -> do
              case migrationSql mig of
                UnparsedSql _ -> do
                  -- We can close the file with peace of mind in this case, as it has been read into memory in its entirety. In fact,
                  -- it's already closed because the stream was consumed completely, but let's be explicit.
                  closeFileStream fs
                  pure (Right fs, asqlmig)
                _ -> do
                  -- Close the file so we don't crash due to the shell's open files limit. The handle will be opened again
                  -- when the stream is forced next time.
                  -- This isn't terribly pretty and assumes the file won't change in between now and the moment it'll be opened again for SQL to be effectivelly applied,
                  -- but that assumption is probably fine and unavoidable if we want to report errors in any pending migrations before we start applying their SQL.
                  closeFileStream fs
                  fileStreamAgain :: FileStream m <-
                    delayedOpenStreamFile pendingMigrationPath
                  let sqlPiecesStreamAgain =
                        substituteEnvVarsInSqlPiecesStream
                          ( migrationEnvVars $
                              addedSqlMig asqlmig
                          )
                          $ parseSqlPiecesStreaming
                          $ fileStream fileStreamAgain
                      asqlmigAgain =
                        asqlmig
                          { addedSqlMig =
                              (addedSqlMig asqlmig)
                                { migrationSql =
                                    WellParsedSql
                                      sqlPiecesStreamAgain
                                }
                          }
                  pure (Right fileStreamAgain, asqlmigAgain)

-- | This can be used as a last-action when applying migrations to
-- strict-check schemas, logging differences, success and throwing
-- an exception if they mismatch.
strictCheckLastAction ::
  (MonadUnliftIO m, CoddLogger m, InTxn m) =>
  CoddSettings ->
  ([BlockOfMigrations m] -> DB.Connection -> m DbRep)
strictCheckLastAction coddSettings@CoddSettings {onDiskReps} blocksOfMigs conn = do
  -- Concurrency barely improves performance here, but why not do it?
  (pgMajorVer, _) <- queryServerMajorAndFullVersion conn
  (expectedReps, dbReps) <- runConcurrently $ (,) <$> Concurrently (either (readRepsFromDisk pgMajorVer) pure onDiskReps) <*> Concurrently (readRepresentationsFromDbWithSettings coddSettings conn)
  unless (isOneShotApplication (migsConnString coddSettings) blocksOfMigs) $
    do
      logWarn
        "Because it wasn't possible to apply all pending migrations in a single transaction, all migrations have been applied. We'll run a schema check."
  logSchemasComparison dbReps expectedReps
  when (dbReps /= expectedReps) $
    throwIO $
      userError
        "Exiting. Database's schema differ from expected."
  pure expectedReps

-- | This can be used as a last-action when applying migrations to
-- lax-check schemas, logging differences or success, but
-- _never_ throwing exceptions and returning the database and expected schemas
-- in this order.
laxCheckLastAction ::
  (MonadUnliftIO m, CoddLogger m, InTxn m) =>
  CoddSettings ->
  ([BlockOfMigrations m] -> DB.Connection -> m (DbRep, DbRep))
laxCheckLastAction coddSettings@CoddSettings {onDiskReps} _blocksOfMigs conn = do
  -- Concurrency barely improves performance here, but why not do it?
  (pgMajorVer, _) <- queryServerMajorAndFullVersion conn
  (expectedReps, dbReps) <- runConcurrently $ (,) <$> Concurrently (either (readRepsFromDisk pgMajorVer) pure onDiskReps) <*> Concurrently (readRepresentationsFromDbWithSettings coddSettings conn)
  logSchemasComparison dbReps expectedReps
  pure (dbReps, expectedReps)

-- | A collection of consecutive migrations. Consecutive in-txn migrations with the same connection string are grouped together,
-- no-txn migrations appear alone.
data BlockOfMigrations m = BlockInTxn (ConsecutiveInTxnMigrations m) | BlockNoTxn (SingleNoTxnMigration m)

data SingleNoTxnMigration m = SingleNoTxnMigration
  { singleNoTxnMig :: AddedSqlMigration m,
    reReadMig :: m (SingleNoTxnMigration m),
    numStatementsAlreadyApplied :: Int
  }

data ConsecutiveInTxnMigrations m = ConsecutiveInTxnMigrations
  { inTxnMigs :: NonEmpty (AddedSqlMigration m),
    reReadBlock :: m (ConsecutiveInTxnMigrations m)
  }

allMigs :: BlockOfMigrations m -> NonEmpty (AddedSqlMigration m)
allMigs (BlockInTxn b) = inTxnMigs b
allMigs (BlockNoTxn b) = singleNoTxnMig b :| []

hoistBlockOfMigrations ::
  forall m n.
  (Monad m, Monad n) =>
  (forall x. m x -> n x) ->
  BlockOfMigrations m ->
  BlockOfMigrations n
hoistBlockOfMigrations hoist = \case
  BlockInTxn b -> BlockInTxn $ hoistInTxnBlock b
  BlockNoTxn b -> BlockNoTxn $ hoistNoTxnBlock b
  where
    hoistInTxnBlock ConsecutiveInTxnMigrations {..} =
      let hoistedAllMigs = hoistAddedSqlMigration hoist <$> inTxnMigs
          hoistedReReadBlock = hoist $ reReadBlock <&> hoistInTxnBlock
       in ConsecutiveInTxnMigrations
            { inTxnMigs = hoistedAllMigs,
              reReadBlock = hoistedReReadBlock
            }
    hoistNoTxnBlock SingleNoTxnMigration {..} =
      let hoistedAllMigs = hoistAddedSqlMigration hoist singleNoTxnMig
          hoistedReReadBlock = hoist $ reReadMig <&> hoistNoTxnBlock
       in SingleNoTxnMigration
            { singleNoTxnMig = hoistedAllMigs,
              reReadMig = hoistedReReadBlock,
              numStatementsAlreadyApplied
            }

-- | Returns True only if all pending migrations are in-txn and of the same connection string, meaning they'll all be applied
-- in a single transaction.
isOneShotApplication :: DB.ConnectInfo -> [BlockOfMigrations m] -> Bool
isOneShotApplication defaultConnInfo pending = case pending of
  [] -> True
  [block@(BlockInTxn _)] ->
    fromMaybe defaultConnInfo (blockCustomConnInfo block) == defaultConnInfo
  _ -> False

blockCustomConnInfo :: BlockOfMigrations m -> Maybe DB.ConnectInfo
blockCustomConnInfo (BlockInTxn (ConsecutiveInTxnMigrations (AddedSqlMigration {addedSqlMig} :| _) _)) =
  migrationCustomConnInfo addedSqlMig
blockCustomConnInfo (BlockNoTxn (SingleNoTxnMigration (AddedSqlMigration {addedSqlMig}) _ _)) =
  migrationCustomConnInfo addedSqlMig

data NoTxnMigrationApplicationFailure = NoTxnMigrationApplicationFailure
  { sqlStatementEx :: SqlStatementException,
    noTxnMigAppliedStatements :: Int
  }
  deriving stock (Show)

instance Exception NoTxnMigrationApplicationFailure

-- | Applies the supplied function to each element in the list in order, stopping on the first one that returns a `Left`.
forMExcept ::
  (Monad m) => NonEmpty a -> (a -> m (Either e ())) -> m (Either e ())
forMExcept nl f = go (NE.toList nl)
  where
    go [] = pure $ Right ()
    go (x : xs) =
      f x >>= \case
        Left e -> pure $ Left e
        Right () -> go xs

class SkipStatements a where
  type SkipError a :: Type
  numStatementsToSkip :: a -> Int
  mkSqlError :: SqlStatementException -> Int -> SkipError a

newtype SkipStatementsNoTxn = SkipStatementsNoTxn Int

data NoSkipStatements = NoSkipStatements

instance SkipStatements SkipStatementsNoTxn where
  type SkipError SkipStatementsNoTxn = NoTxnMigrationApplicationFailure
  numStatementsToSkip (SkipStatementsNoTxn n) = n
  mkSqlError = NoTxnMigrationApplicationFailure

instance SkipStatements NoSkipStatements where
  type SkipError NoSkipStatements = SqlStatementException
  numStatementsToSkip NoSkipStatements = 0
  mkSqlError ex _ = ex

-- | Applies a single migration and returns an error if it failed to be applied or `()` otherwise. Calls the supplied
-- `registerMigRan` after the migration is applied or fails. Does not throw exceptions coming from SQL errors.
applySingleMigration ::
  forall m s.
  (MonadUnliftIO m, CoddLogger m, SkipStatements s) =>
  DB.Connection ->
  ( FilePath ->
    DB.UTCTimestamp ->
    DiffTime ->
    MigrationApplicationStatus ->
    m ()
  ) ->
  -- | Number of countable-runnable statements to skip completely. Useful when retrying no-txn migrations from exactly the statements they last failed in.
  s ->
  AddedSqlMigration m ->
  m (Either (SkipError s) ())
applySingleMigration conn registerMigRan skip (AddedSqlMigration sqlMig migTimestamp) =
  do
    let fn = migrationName sqlMig
        numCountableRunnableStmtsToSkip = numStatementsToSkip skip
    logInfoNoNewline $ "Applying " <> Text.pack fn

    ((appliedMigrationNumStatements, errorOrDone, mLastBegin), appliedMigrationDuration) <-
      timeAction $ do
        case ( migrationSql sqlMig,
               numCountableRunnableStmtsToSkip > 0
             ) of
          (UnparsedSql _, True) ->
            error
              "Internal error in codd. A migration with no-parse cannot have failed in a statement that is not its 0th statement. Please report this as a bug."
          (UnparsedSql unparsedSql, False) -> do
            applyResult <- singleStatement_ conn unparsedSql
            case applyResult of
              NotACountableStatement ->
                error
                  "Internal error in codd. no-parse migration cannot be NotACountableStatement. Please report this as a bug"
              StatementErred err ->
                pure (0, Just err, Nothing)
              StatementApplied _ ->
                pure (1, Nothing, Nothing)
          (WellParsedSql sqlStream, _) -> do
            initialTxnStatus <- txnStatus conn
            ((numStmts, mLastBegin, _) :> errorOrDone) <-
              Streaming.fold
                ( \(!l, !lastBegin, !lastTxnStatus) !txnStatusNow ->
                    (l + 1,,txnStatusNow) $
                      case (lastTxnStatus, txnStatusNow) of
                        (PQ.TransInTrans, PQ.TransInTrans) ->
                          lastBegin
                        (PQ.TransIdle, PQ.TransIdle) ->
                          Nothing
                        (PQ.TransIdle, PQ.TransInTrans) ->
                          Just (l + 1)
                        (PQ.TransInTrans, PQ.TransIdle) ->
                          Nothing
                        states@(PQ.TransActive, _) ->
                          error $
                            "Internal error in codd. It seems libpq returned a transaction status while another statement was running, which should be impossible. Please report this as a bug: "
                              ++ show states
                        states@(_, PQ.TransActive) ->
                          error $
                            "Internal error in codd. It seems libpq returned a transaction status while another statement was running, which should be impossible. Please report this as a bug: "
                              ++ show states
                        states@(PQ.TransInError, _) ->
                          error $
                            "Internal error in codd. Erring statements should be in stream's return, not as an element of it. Please report this as a bug: "
                              ++ show states
                        states@(_, PQ.TransInError) ->
                          error $
                            "Internal error in codd. Erring statements should be in stream's return, not as an element of it. Please report this as a bug: "
                              ++ show states
                        states@(PQ.TransUnknown, _) ->
                          error $
                            "Connection to database may have gone bad. Did someone else kill the connection while codd was applying migrations, perhaps? Codd cannot retry under these circumstances, sadly. Please file a bug report if retrying under such circumstances is important to you: "
                              ++ show states
                        states@(_, PQ.TransUnknown) ->
                          error $
                            "Connection to database may have gone bad. Did someone else kill the connection while codd was applying migrations, perhaps? Codd cannot retry under these circumstances, sadly. Please file a bug report if retrying under such circumstances is important to you: "
                              ++ show states
                )
                ( numCountableRunnableStmtsToSkip,
                  Nothing,
                  initialTxnStatus
                )
                id
                $ applyStatementStream conn
                $ forceStreamConcurrently 3
                $ skipCountableRunnableStatements
                  numCountableRunnableStmtsToSkip
                  sqlStream
            pure (numStmts, errorOrDone, mLastBegin)

    case errorOrDone of
      Just sqlStatementEx -> do
        logInfo " [<RED>failed</RED>]"
        logError $ Text.pack $ show sqlStatementEx
        fmap Left $
          if migrationInTxn sqlMig
            then pure $ mkSqlError @s sqlStatementEx 0
            else do
              case mLastBegin of
                Nothing -> do
                  logError $
                    "After applying "
                      <> Fmt.sformat
                        Fmt.int
                        appliedMigrationNumStatements
                      <> " statements from <YELLOW>no-txn</YELLOW> migration "
                      <> Text.pack fn
                      <> ", the "
                      <> Fmt.sformat
                        Fmt.ords
                        (appliedMigrationNumStatements + 1)
                      <> " failed to be applied. Codd will resume the next retry or <MAGENTA>codd up</MAGENTA> from it"
                  registerMigRan
                    fn
                    migTimestamp
                    appliedMigrationDuration
                    ( NoTxnMigrationFailed
                        appliedMigrationNumStatements
                    )
                  pure $
                    mkSqlError @s
                      sqlStatementEx
                      appliedMigrationNumStatements
                Just lastBeginNum -> do
                  logError $
                    "After applying "
                      <> Fmt.sformat
                        Fmt.int
                        appliedMigrationNumStatements
                      <> " statements from <YELLOW>no-txn</YELLOW> migration "
                      <> Text.pack fn
                      <> ", the "
                      <> Fmt.sformat
                        Fmt.ords
                        (appliedMigrationNumStatements + 1)
                      <> " failed to be applied. Since this failed statement is inside an explicitly started transaction in the migration, codd will resume the next retry or <MAGENTA>codd up</MAGENTA> from the last <GREEN>BEGIN</GREEN>-like statement, which is the "
                      <> Fmt.sformat Fmt.ords lastBeginNum
                      <> " statement in this migration"

                  -- ROLLBACK if necessary. Notice that if a COMMIT statement fails, a ROLLBACK is redundant. For any other failing statements that is not the case.
                  finalTxnStatus <- txnStatus conn
                  when (finalTxnStatus == PQ.TransInError) $ do
                    liftIO $ DB.rollback conn
                    logInfo
                      "<MAGENTA>ROLLBACK</MAGENTA>ed last explicitly started transaction"
                  registerMigRan
                    fn
                    migTimestamp
                    appliedMigrationDuration
                    (NoTxnMigrationFailed (lastBeginNum - 1))
                  pure $
                    mkSqlError @s
                      sqlStatementEx
                      (lastBeginNum - 1)
      Nothing -> do
        registerMigRan
          fn
          migTimestamp
          appliedMigrationDuration
          (MigrationAppliedSuccessfully appliedMigrationNumStatements)
        logInfo $
          " (<CYAN>"
            <> prettyPrintDuration appliedMigrationDuration
            <> "</CYAN>)"
        pure $ Right ()

-- | This type exists because bootstrapping migrations can't be registered until codd's internal schema is created. But we want the time when they were applied to truly reflect when they were
-- applied, so we wouldn't be able to use NowInPostgresTime by the time codd's internal schema is created.
data MigrationLastStatementAppliedAt = NowInPostgresTime | SpecificTime UTCTime

-- | This type exists because we want to register the txnId and connId that was effectively the one that applied statements in each migration (no-txn migration have a different txnId for each statement, but that's orthogonal),
-- and some migrations are applied in separate connections or before codd's internal schema is ready, so they query these IDs from the database at that time in those cases. When codd's internal schema is ready and the migration runs in the default
-- database, callers will call us with `OfCurrentTransaction`, issuing one less statement per migration (not really a performance gain, but less log spamming).
data MigrationTxnAndConnIds = OfCurrentTransaction | SpecificIds Int64 Int

-- | Registers in the DB that a migration with supplied name and timestamp
--   has been either successfully applied or partially failed (the latter only makes sense for no-txn migrations).
--   Will throw an error if codd's internal schema hasn't yet been created.
registerRanMigration ::
  forall txn m.
  (MonadUnliftIO m, MonadIO txn, CanStartTxn m txn) =>
  -- | The default connection, not any other or this might fail.
  DB.Connection ->
  TxnIsolationLvl ->
  FilePath ->
  DB.UTCTimestamp ->
  -- | The time the last statement of the migration was applied or when it failed.
  MigrationLastStatementAppliedAt ->
  DiffTime ->
  MigrationApplicationStatus ->
  MigrationTxnAndConnIds ->
  m UTCTime
registerRanMigration conn isolLvl fn migTimestamp appliedAt appliedMigrationDuration apStatus txnConnIds =
  let -- Ugly splicing and dicing follows..
      (args1, numAppliedStatements, timestampValue) =
        case (appliedAt, apStatus) of
          (NowInPostgresTime, NoTxnMigrationFailed numStmts) ->
            ("?, ?, clock_timestamp()", numStmts, Nothing)
          (NowInPostgresTime, MigrationAppliedSuccessfully numStmts) ->
            ("?, clock_timestamp(), ?", numStmts, Nothing)
          (SpecificTime t, NoTxnMigrationFailed numStmts) ->
            ("?, NULL, ?", numStmts, Just t)
          (SpecificTime t, MigrationAppliedSuccessfully numStmts) ->
            ("?, ?, NULL", numStmts, Just t)
      (cols2, args2, txnConnIdsRow) = case txnConnIds of
        OfCurrentTransaction -> (")", "", DB.toRow ())
        SpecificIds txnId connId ->
          (", txnid, connid)", ", ?, ?", DB.toRow (txnId, connId))
   in withTransaction @txn isolLvl conn $
        DB.fromOnly
          <$> unsafeQuery1
            conn
            ( "INSERT INTO codd.sql_migrations as m (migration_timestamp, name, application_duration, num_applied_statements, applied_at, no_txn_failed_at"
                <> cols2
                <> " \
                   \                            SELECT ?, ?, ?, "
                <> args1
                <> args2
                <> " \
                   \                            ON CONFLICT (name) DO UPDATE \
                   \                               SET application_duration=EXCLUDED.application_duration + m.application_duration \
                   \                                 , num_applied_statements=EXCLUDED.num_applied_statements \
                   \                                 , applied_at=EXCLUDED.applied_at \
                   \                                 , no_txn_failed_at=EXCLUDED.no_txn_failed_at \
                   \                                 , txnid=EXCLUDED.txnid \
                   \                                 , connid=EXCLUDED.connid \
                   \                            RETURNING COALESCE(applied_at, no_txn_failed_at)"
            )
            ( ( migTimestamp,
                fn,
                -- postgresql-simple does not have a `ToField DiffTime` instance :(
                realToFrac @Double @NominalDiffTime $
                  fromIntegral (diffTimeToPicoseconds appliedMigrationDuration)
                    / 1_000_000_000_000,
                numAppliedStatements,
                timestampValue
              )
                DB.:. txnConnIdsRow
            )
