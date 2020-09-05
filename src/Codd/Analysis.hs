module Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration, someDestructiveChangeHasBeenApplied) where

-- | This Module is all about analyzing SQL Migrations, by e.g. running them and checking if they're destructive, amongst other things, possibly.

import Codd.Hashing (DbHashes(..), IsDbObject(..), DbObject(..), getDbHashes, childrenObjs)
import Codd.Internal
import Codd.Query (unsafeQuery1)
import Codd.Types (SqlMigration(..), ApplyMigrations(..), DbVcsInfo)
import Data.List (sortOn)
import qualified Database.PostgreSQL.Simple as DB
import GHC.Int (Int64)
import UnliftIO (MonadUnliftIO, MonadIO(..))

data MigrationCheck = MigrationCheck NonDestructiveSectionCheck DestructiveSectionCheck

data NonDestructiveSectionCheck = NonDestructiveSectionCheck {
    nonDestSectionIsDestructive :: Bool
    , nonDestSectionEndsTransaction :: Bool
}

data DestructiveSectionCheck = DestructiveSectionCheck {
    destSectionEndsTransaction :: Bool
}

-- | Checks if there are any problems, including:
--   1. in-txn migration ROLLBACKs or COMMITs inside any of its Sql sections.
--   2. non-destructive migration is destructive without 'force' option (not a perfect algorithm, but helpful for most common cases).
-- This function can only be used for Migrations that haven't been added yet.
checkMigration :: forall m. (MonadUnliftIO m, MonadIO m) => DbVcsInfo -> SqlMigration -> m MigrationCheck
checkMigration dbInfo mig =
    -- TODO: If there are no-txn non-destructive migrations, create a separate throw-away DB to do this
    -- TODO: What if this migration is itself no-txn ?
    applyMigrationsInternal beginRollbackTxnBracket applyMigs dbInfo OnlyNonDestructive
    where
        applyMigs :: DB.Connection -> ApplyMigrations -> [SqlMigration] -> m MigrationCheck
        applyMigs conn applyType allMigs = baseApplyMigsBlock runLast conn applyType allMigs

        getTxId :: DB.Connection -> m Int64
        getTxId conn = fmap DB.fromOnly $ unsafeQuery1 conn "SELECT txid_current()" ()
        
        runLast conn = do
            hbef <- getDbHashes conn
            txId1 <- getTxId conn
            -- TODO: because we are inside a transaction here, if the migration attempts to
            -- do something that can't be done in a transaction we should expect a specific exception..
            -- TODO: If a migration ends a transaction and runs some modification.. (or if it's no-txn),
            -- how do we recover the previous DB state? Maybe we should always create a throw-away DB for this operation!
            -- Note: if this is going to be expensive, add a --no-check to the app for Users to add migrations. It will be useful
            -- in case of bugs too.
            applySingleMigration conn IndNonDestructive mig
            txId2 <- getTxId conn
            haft <- getDbHashes conn

            -- If the non-destructive section ended the transaction, we should start a new one here!
            (txId3, txId4) <- (if txId1 == txId2 then id else beginRollbackTxnBracket conn) $ do
                txId3 <- getTxId conn
                applySingleMigration conn IndDestructive mig
                txId4 <- getTxId conn
                return (txId3, txId4)
            let
                nonDestSectionCheck = NonDestructiveSectionCheck {
                    nonDestSectionIsDestructive = someDestructiveChangeHasBeenApplied hbef haft
                    , nonDestSectionEndsTransaction = txId1 /= txId2
                }
                destSectionCheck = DestructiveSectionCheck {
                    destSectionEndsTransaction = txId3 /= txId4
                }
            return $ MigrationCheck nonDestSectionCheck destSectionCheck

-- | TODO: It'd be interesting to show which changes we detected as destructive
someDestructiveChangeHasBeenApplied :: DbHashes -> DbHashes -> Bool
someDestructiveChangeHasBeenApplied (DbHashes sbf) (DbHashes saf) = anyDrop (map DbObject sbf) (map DbObject saf)

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