{-|
This Module is all about analyzing SQL Migrations, by e.g. running them and checking if they're destructive, amongst other things, possibly.
-}

module Codd.Analysis
    ( MigrationCheck(..)
    , MigrationCheckSimpleWorkflow(..)
    , NonDestructiveSectionCheck(..)
    , DestructiveSectionCheck(..)
    , canRunEverythingInASingleTransaction
    , checkMigrationSimpleWorkflow
    ) where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal
import           Codd.Parsing                   ( ParsedSql
                                                    ( ParseFailSqlText
                                                    , WellParsedSql
                                                    )
                                                , SqlMigration(..)
                                                , SqlPiece(BeginTransaction)
                                                , isTransactionEndingPiece
                                                )
import           Control.Monad.Logger           ( MonadLogger )
import           Data.Foldable                  ( foldl' )
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
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
