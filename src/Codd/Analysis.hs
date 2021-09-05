{-|
This Module is all about analyzing SQL Migrations, by e.g. running them and checking if they're destructive, amongst other things, possibly.
-}

module Codd.Analysis
    ( MigrationCheck(..)
    , MigrationCheckSimpleWorkflow(..)
    , NonDestructiveSectionCheck(..)
    , DestructiveSectionCheck(..)
    , checkMigration
    , canRunEverythingInASingleTransaction
    , someDestructiveChangeHasBeenApplied
    , migrationErrors
    , checkMigrationSimpleWorkflow
    ) where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( DbHashes(..)
                                                , DbObject(..)
                                                , IsDbObject(..)
                                                , childrenObjs
                                                , matchOrd
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Internal
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , ParsedSql
                                                    ( ParseFailSqlText
                                                    , WellParsedSql
                                                    )
                                                , SqlMigration(..)
                                                , SqlPiece(BeginTransaction)
                                                , isTransactionEndingPiece
                                                )
import           Codd.Query                     ( query
                                                , unsafeQuery1
                                                )
import           Codd.Types                     ( DeploymentWorkflow(..)
                                                , singleTryPolicy
                                                )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Logger           ( MonadLogger )
import           Data.Foldable                  ( foldl' )
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import           GHC.Int                        ( Int64 )
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
                                                , bracket_
                                                )

newtype MigrationCheckSimpleWorkflow = MigrationCheckSimpleWorkflow { transactionManagementProblem :: Maybe Text }
    deriving stock Show

data MigrationCheck = MigrationCheck NonDestructiveSectionCheck
                                     DestructiveSectionCheck
    deriving stock Show

data NonDestructiveSectionCheck = NonDestructiveSectionCheck
    { nonDestSectionIsDestructive   :: Bool
    , nonDestSectionEndsTransaction :: Bool
    }
    deriving stock Show

newtype DestructiveSectionCheck = DestructiveSectionCheck
    { destSectionEndsTransaction :: Bool
    }
    deriving stock Show

migrationErrors :: SqlMigration -> MigrationCheck -> [Text]
migrationErrors sqlMig (MigrationCheck NonDestructiveSectionCheck {..} DestructiveSectionCheck {..})
    | nonDestSectionIsDestructive && not (nonDestructiveForce sqlMig)
    = [ "Non-destructive section is destructive but not properly annotated. Add the option 'force' if you really want this."
      ]
    | nonDestSectionEndsTransaction && nonDestructiveInTxn sqlMig
    = [ "Non-destructive section ends Transactions when run, and that is not allowed."
      ]
    | destSectionEndsTransaction && destructiveInTxn sqlMig
    = [ "Destructive section ends Transactions when run, and that is not allowed."
      ]
    | otherwise
    = []

-- | Returns True iff all pending migrations and the non-destructive section of the one passed as an argument can run in a single transaction.
canRunEverythingInASingleTransaction
    :: (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> SqlMigration
    -> m Bool
canRunEverythingInASingleTransaction settings mig = do
    createEmptyDbIfNecessary settings
    pendingMigBlocks <- collectPendingMigrations settings
    -- TODO: In Blue-Green-Safe mode, how do we decide this?
    return
        $  all blockInTxn pendingMigBlocks
        && nonDestructiveInTxn mig
        && isNothing (destructiveSql mig)

-- | Checks for problems in a migration for the Simple Workflow mode, including:
--   1. in-txn migration ROLLBACKs or COMMITs.
--   2. no-txn migrations BEGIN transaction but never ROLLBACKs or COMMITs it.
checkMigrationSimpleWorkflow
    :: SqlMigration -> Either Text MigrationCheckSimpleWorkflow
checkMigrationSimpleWorkflow mig =
    MigrationCheckSimpleWorkflow <$> migEndsTransaction

  where
    migEndsTransaction =
        case (nonDestructiveSql mig, nonDestructiveInTxn mig) of
            (Nothing, _) -> Left "Empty migration"
            (Just (ParseFailSqlText _), _) ->
                Left "Error parsing migration so checking is not possible"
            (Just (WellParsedSql _ pieces), True) ->
                Right $ if any isTransactionEndingPiece pieces
                    then Just "in-txn migration cannot issue COMMIT or ROLLBACK"
                    else Nothing
            (Just (WellParsedSql _ (p1 :| ps)), False) ->
                let
                    isOpenAfter wasOpen p = case p of
                        BeginTransaction _ -> True
                        _ -> not (isTransactionEndingPiece p) && wasOpen

                    leavesTransactionOpen =
                        foldl' isOpenAfter (isOpenAfter False p1) ps
                in
                    Right $ if leavesTransactionOpen
                        then
                            Just
                                "no-txn migration BEGINs but does not COMMIT or ROLLBACK transaction"
                        else Nothing


-- | Checks if there are any problems, including:
--   1. in-txn migration ROLLBACKs or COMMITs inside any of its Sql sections.
--   2. non-destructive migration is destructive without 'force' option (not a perfect algorithm, but helpful for most common cases).
-- This function can only be used for Migrations that haven't been added yet.
checkMigration
    :: forall m
     . (MonadUnliftIO m, MonadIO m, MonadLogger m)
    => CoddSettings
    -> SqlMigration
    -> m MigrationCheck
checkMigration dbInfoApp@CoddSettings { superUserConnString, dbName, retryPolicy, txnIsolationLvl } mig
    = do
        createEmptyDbIfNecessary dbInfoApp

        -- Note: we want to run every single pending destructive migration when checking new migrations to ensure
        -- conflicts that aren't caught by on-disk hashes are detected by developers

        -- Also: Everything must run in a throw-away Database, which can't use BEGIN ... ROLLBACK because
        -- there might be deferrable constraints and triggers which run on COMMIT and which must also be tested.
        let throwAwayDbName = dbIdentifier "codd-throwaway-db"
            appDbName       = dbIdentifier dbName
            throwAwayDbInfo =
                dbInfoForExistingMigs { dbName = "codd-throwaway-db" }

        numOtherConnected :: Int64 <-
            fmap DB.fromOnly $ withConnection superUserConnString $ \conn ->
                unsafeQuery1
                    conn
                    "select count(*) from pg_stat_activity where datname=? and pid <> pg_backend_pid()"
                    (DB.Only dbName)
        when (numOtherConnected > 0) $ do
            liftIO
                $ putStrLn
                $ "Warning: To analyze a migration, a throw-away Database will be created to avoid modifying the App's Database."
            liftIO
                $  putStrLn
                $  "There are other open connections to database "
                ++ show dbName
                ++ ". Would you like to terminate them and proceed? [y/n]"
            proceed <- liftIO getLine
            case proceed of
                "y" -> withConnection superUserConnString $ \conn ->
                    void $ query @(DB.Only Bool)
                        conn
                        "select pg_terminate_backend(pid) from pg_stat_activity where datname=? and pid <> pg_backend_pid()"
                        (DB.Only dbName)
                _ -> error "Exiting without analyzing migration."

        bracket_
            (withConnection superUserConnString $ \conn -> liftIO $ do
                void
                    $  DB.execute_ conn
                    $  "DROP DATABASE IF EXISTS "
                    <> throwAwayDbName
                DB.execute_ conn
                    $  "CREATE DATABASE "
                    <> throwAwayDbName
                    <> " TEMPLATE "
                    <> appDbName
            )
            (withConnection superUserConnString $ \conn ->
                liftIO
                    $  DB.execute_ conn
                    $  "DROP DATABASE IF EXISTS "
                    <> throwAwayDbName
            )
            (applyMigrationsInternal applyMigs throwAwayDbInfo)

  where
    thisMigrationAdded    = AddedSqlMigration mig DB.PosInfinity

    dbInfoForExistingMigs = dbInfoApp
        { deploymentWorkflow = case deploymentWorkflow dbInfoApp of
                                   SimpleDeployment -> SimpleDeployment
                                   BlueGreenSafeDeploymentUpToAndIncluding _ ->
                                       BlueGreenSafeDeploymentUpToAndIncluding
                                           DB.PosInfinity
        }

    applyMigs :: DB.Connection -> [NonEmpty MigrationToRun] -> m MigrationCheck
    applyMigs conn allMigs =
        baseApplyMigsBlock retryPolicy runLast txnIsolationLvl conn allMigs

    getTxId :: DB.Connection -> m Int64
    getTxId conn = DB.fromOnly <$> unsafeQuery1 conn "SELECT txid_current()" ()

    runLast _migBlocks conn = do
        hbef                <- readHashesFromDatabaseWithSettings dbInfoApp conn
        nonDestSectionCheck <- if nonDestructiveInTxn mig
            then beginCommitTxnBracket txnIsolationLvl conn $ do
                txId1 <- getTxId conn
                -- Note: if this is going to be expensive, add a --no-check to the app for Users to add migrations. It will be useful
                -- in case of bugs too.
                applySingleMigration conn
                                     txnIsolationLvl
                                     singleTryPolicy
                                     ApplyNonDestructiveOnly
                                     thisMigrationAdded
                txId2 <- getTxId conn
                haft  <- readHashesFromDatabaseWithSettings dbInfoApp conn
                return NonDestructiveSectionCheck
                    { nonDestSectionIsDestructive   =
                        someDestructiveChangeHasBeenApplied hbef haft
                    , nonDestSectionEndsTransaction = txId1 /= txId2
                    }
            else do
                applySingleMigration conn
                                     txnIsolationLvl
                                     retryPolicy
                                     ApplyNonDestructiveOnly
                                     thisMigrationAdded
                haft <- readHashesFromDatabaseWithSettings dbInfoApp conn
                return NonDestructiveSectionCheck
                    { nonDestSectionIsDestructive   =
                        someDestructiveChangeHasBeenApplied hbef haft
                    , nonDestSectionEndsTransaction = False -- It doesn't even run in a transaction, so it should be fine
                    }

        -- We want to test the destructive section (if any) when analysing to avoid errors being detected only when it's
        -- marked to run.
        destSectionCheck <- case destructiveSql mig of
            Nothing -> pure DestructiveSectionCheck
                { destSectionEndsTransaction = False
                }
            Just _ ->
                (if not (destructiveInTxn mig)
                        then id
                        else beginCommitTxnBracket txnIsolationLvl conn
                    )
                    $ do
                          txId3 <- getTxId conn
                          applySingleMigration conn
                                               txnIsolationLvl
                                               singleTryPolicy
                                               ApplyDestructiveOnly
                                               thisMigrationAdded
                          txId4 <- getTxId conn
                          return DestructiveSectionCheck
                              { destSectionEndsTransaction = txId3 /= txId4
                              }
        return $ MigrationCheck nonDestSectionCheck destSectionCheck

-- | TODO: It'd be interesting to show which changes we detected as destructive
someDestructiveChangeHasBeenApplied :: DbHashes -> DbHashes -> Bool
someDestructiveChangeHasBeenApplied (DbHashes dbsh1 (Map.elems -> sbf) (Map.elems -> rbf)) (DbHashes dbsh2 (Map.elems -> saf) (Map.elems -> raf))
    = anyDrop (map DbObject sbf) (map DbObject saf)
        || anyDrop (map DbObject rbf) (map DbObject raf)
        || dbsh1
        /= dbsh2

anyDrop :: [DbObject] -> [DbObject] -> Bool
anyDrop objs1 objs2 =
    let matched = matchOrd objName objs1 objs2
    in 
        -- Check: something dropped at this level and something dropped recursively for objects
        -- that have been kept
        any
            (\case
                (Just _, Nothing) -> True
                (Just obf, Just oaf) ->
                    anyDrop (childrenObjs obf) (childrenObjs oaf)
                _ -> False
            )
            matched
