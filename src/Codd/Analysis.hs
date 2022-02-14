{-|
This Module is all about analyzing SQL Migrations, by e.g. running them and checking if they're destructive, amongst other things, possibly.
-}

module Codd.Analysis
    ( MigrationCheck(..)
    , checkMigration
    ) where

import           Codd.Parsing                   ( ParsedSql
                                                    ( ParseFailSqlText
                                                    , WellParsedSql
                                                    )
                                                , SqlMigration(..)
                                                , SqlPiece(BeginTransaction)
                                                , isTransactionEndingPiece
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import           Data.Text                      ( Text )

newtype MigrationCheck = MigrationCheck { transactionManagementProblem :: Maybe Text }
    deriving stock Show


-- | Checks for problems in a migration, including:
--   1. in-txn migration ROLLBACKs or COMMITs.
--   2. no-txn migrations BEGIN transaction but never ROLLBACKs or COMMITs it.
checkMigration :: SqlMigration -> Either Text MigrationCheck
checkMigration mig = MigrationCheck <$> migEndsTransaction

  where
    migEndsTransaction = case (migrationSql mig, migrationInTxn mig) of
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
