module Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration, someDestructiveChangeHasBeenApplied, migrationErrors) where

-- | This Module is all about analyzing SQL Migrations, by e.g. running them and checking if they're destructive, amongst other things, possibly.

import Codd.Hashing (DbHashes(..), IsDbObject(..), DbObject(..), readHashesFromDatabaseWithSettings, childrenObjs)
import Codd.Internal
import Codd.Query (unsafeQuery1, query)
import Codd.Types (SqlMigration(..), AddedSqlMigration(..), DeploymentWorkflow(..), CoddSettings(..))
import Control.Monad (void, when)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Time as DB
import GHC.Int (Int64)
import UnliftIO (MonadUnliftIO, MonadIO(..), bracket_)

data MigrationCheck = MigrationCheck NonDestructiveSectionCheck DestructiveSectionCheck deriving stock Show

data NonDestructiveSectionCheck = NonDestructiveSectionCheck {
    nonDestSectionIsDestructive :: Bool
    , nonDestSectionEndsTransaction :: Bool
} deriving stock Show

data DestructiveSectionCheck = DestructiveSectionCheck {
    destSectionEndsTransaction :: Bool
} deriving stock Show

migrationErrors :: SqlMigration -> MigrationCheck -> [String]
migrationErrors sqlMig (MigrationCheck (NonDestructiveSectionCheck {..}) (DestructiveSectionCheck {..})) = 
    if (nonDestSectionIsDestructive && not (nonDestructiveForce sqlMig)) then [ "Non-destructive section is destructive but not properly annotated. Add the option 'force' if you really want this." ] else []
    ++ if (nonDestSectionEndsTransaction && nonDestructiveInTxn sqlMig) then [ "Non-destructive section ends Transactions when run, and that is not allowed." ] else []
    ++ if (destSectionEndsTransaction && destructiveInTxn sqlMig) then [ "Destructive section ends Transactions when run, and that is not allowed." ] else []


-- | Checks if there are any problems, including:
--   1. in-txn migration ROLLBACKs or COMMITs inside any of its Sql sections.
--   2. non-destructive migration is destructive without 'force' option (not a perfect algorithm, but helpful for most common cases).
-- This function can only be used for Migrations that haven't been added yet.
checkMigration :: forall m. (MonadUnliftIO m, MonadIO m) => CoddSettings -> SqlMigration -> m MigrationCheck
checkMigration dbInfoApp@(CoddSettings { superUserConnString, dbName }) mig = do
    -- Note: we want to run every single pending destructive migration when checking new migrations to ensure
    -- conflicts that aren't caught by on-disk hashes are detected by developers

    -- Also: Everything must run in a throw-away Database, with can't use BEGIN ... ROLLBACK because
    -- there might be deferrable constraints and triggers which run on COMMIT and which must also be tested.
    let throwAwayDbName = dbIdentifier "codd-throwaway-db"
        appDbName = dbIdentifier dbName
        throwAwayDbInfo = dbInfoForExistingMigs { dbName = "codd-throwaway-db" }

    numOtherConnected :: Int64 <- fmap DB.fromOnly $ withConnection superUserConnString $ \conn -> unsafeQuery1 conn "select count(*) from pg_stat_activity where datname=? and pid <> pg_backend_pid()" (DB.Only dbName)
    when (numOtherConnected > 0) $ do
        liftIO $ putStrLn $ "Warning: To analyze a migration, a throw-away Database will be created to avoid modifying the App's Database."
        liftIO $ putStrLn $ "There are other open connections to database " ++ show dbName ++ ". Would you like to kill them and proceed? [y/n]"
        proceed <- liftIO getLine
        case proceed of
            "y" -> withConnection superUserConnString $ \conn -> void $ query @(DB.Only Bool) conn "select pg_terminate_backend(pid) from pg_stat_activity where datname=? and pid <> pg_backend_pid()" (DB.Only dbName)
            _ -> error "Exiting without analyzing migration."
    
    bracket_
        (withConnection superUserConnString $ \conn -> liftIO $ do
            void $ DB.execute_ conn $ "DROP DATABASE IF EXISTS " <> throwAwayDbName
            DB.execute_ conn $ "CREATE DATABASE " <> throwAwayDbName <> " TEMPLATE " <> appDbName)
        (withConnection superUserConnString $ \conn -> liftIO $ DB.execute_ conn $ "DROP DATABASE IF EXISTS " <> throwAwayDbName)
        (applyMigrationsInternal beginCommitTxnBracket applyMigs throwAwayDbInfo)
    
    where
        thisMigrationAdded = AddedSqlMigration mig DB.PosInfinity

        dbInfoForExistingMigs =
            dbInfoApp {
                deploymentWorkflow =
                    case deploymentWorkflow dbInfoApp of
                        SimpleDeployment -> SimpleDeployment
                        BlueGreenSafeDeploymentUpToAndIncluding _ -> BlueGreenSafeDeploymentUpToAndIncluding DB.PosInfinity
                }

        applyMigs :: DB.Connection -> TxnBracket m -> [NonEmpty MigrationToRun] -> m MigrationCheck
        applyMigs conn txnBracket allMigs = baseApplyMigsBlock runLast conn txnBracket allMigs

        getTxId :: DB.Connection -> m Int64
        getTxId conn = fmap DB.fromOnly $ unsafeQuery1 conn "SELECT txid_current()" ()
        
        runLast conn = do
            hbef <- readHashesFromDatabaseWithSettings dbInfoApp conn
            nonDestSectionCheck <-
                if nonDestructiveInTxn mig then
                    beginCommitTxnBracket conn $ do
                        txId1 <- getTxId conn
                        -- Note: if this is going to be expensive, add a --no-check to the app for Users to add migrations. It will be useful
                        -- in case of bugs too.
                        applySingleMigration conn ApplyNonDestructiveOnly thisMigrationAdded
                        txId2 <- getTxId conn
                        haft <- readHashesFromDatabaseWithSettings dbInfoApp conn
                        return NonDestructiveSectionCheck {
                            nonDestSectionIsDestructive = someDestructiveChangeHasBeenApplied hbef haft
                            , nonDestSectionEndsTransaction = txId1 /= txId2
                        }
                else do
                    applySingleMigration conn ApplyNonDestructiveOnly thisMigrationAdded
                    haft <- readHashesFromDatabaseWithSettings dbInfoApp conn
                    return NonDestructiveSectionCheck {
                        nonDestSectionIsDestructive = someDestructiveChangeHasBeenApplied hbef haft
                        , nonDestSectionEndsTransaction = False -- It doesn't even run in a transaction, so it should be fine
                    }

            -- We want to test the destructive section (if any) when analysing to avoid errors being detected only when it's
            -- marked to run.
            destSectionCheck <-
                case destructiveSql mig of
                    Nothing -> pure DestructiveSectionCheck { destSectionEndsTransaction = False }
                    Just _  ->
                        (if not (destructiveInTxn mig) then id else beginCommitTxnBracket conn) $ do
                            txId3 <- getTxId conn
                            applySingleMigration conn ApplyDestructiveOnly thisMigrationAdded
                            txId4 <- getTxId conn
                            return DestructiveSectionCheck {
                                destSectionEndsTransaction = txId3 /= txId4
                            }
            return $ MigrationCheck nonDestSectionCheck destSectionCheck

-- | TODO: It'd be interesting to show which changes we detected as destructive
someDestructiveChangeHasBeenApplied :: DbHashes -> DbHashes -> Bool
someDestructiveChangeHasBeenApplied (DbHashes (Map.elems -> sbf) (Map.elems -> rbf)) (DbHashes (Map.elems -> saf) (Map.elems -> raf)) =
    anyDrop (map DbObject sbf) (map DbObject saf)
    || anyDrop (map DbObject rbf) (map DbObject raf)

anyDrop :: [DbObject] -> [DbObject] -> Bool
anyDrop objs1 objs2 =
    let
        matched = matchOrd objName objs1 objs2
    in
        -- Check: something dropped at this level and something dropped recursively for objects
        -- that have been kept
        any (\case
                (Just _, Nothing) -> True
                (Just obf, Just oaf) -> anyDrop (childrenObjs obf) (childrenObjs oaf)
                _ -> False) matched

matchOrd :: Ord b => (a -> b) -> [a] -> [a] -> [(Maybe a, Maybe a)]
matchOrd f (sortOn f -> lsub) (sortOn f -> lsup) = go lsub lsup
    where
        go [] [] = []
        go (x:xs) [] = (Just x, Nothing) : go xs []
        go [] (y:ys) = (Nothing, Just y) : go [] ys
        go (x:xs) (y:ys) =
            let
                xf = f x
                yf = f y
            in
                if xf < yf then (Just x, Nothing) : go xs (y:ys)
                else if xf == yf then (Just x, Just y) : go xs ys
                else (Nothing, Just y) : go (x:xs) ys