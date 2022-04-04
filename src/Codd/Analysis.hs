module Codd.Analysis
    ( MigrationCheck(..)
    , checkMigration
    ) where

import           Codd.Parsing                   ( ParsedSql(..)
                                                , SqlMigration(..)
                                                , SqlPiece(..)
                                                , isCommentPiece
                                                , isTransactionEndingPiece
                                                , isWhiteSpacePiece
                                                )
import           Data.Text                      ( Text )
import qualified Streaming.Prelude             as Streaming

newtype MigrationCheck = MigrationCheck { transactionManagementProblem :: Maybe Text }
    deriving stock Show


-- | Checks for problems in a migration, including:
--   1. in-txn migration ROLLBACKs or COMMITs.
--   2. no-txn migrations BEGIN transaction but never ROLLBACKs or COMMITs it.
checkMigration :: Monad m => SqlMigration m -> m (Either Text MigrationCheck)
checkMigration mig = do
    migEndsTransaction <- case (migrationSql mig, migrationInTxn mig) of
        (UnparsedSql   _     , _   ) -> pure $ Right Nothing
        (WellParsedSql pieces, True) -> do
            problem <- Streaming.any_ isTransactionEndingPiece pieces
            pure $ Right $ if problem
                then Just "in-txn migration cannot issue COMMIT or ROLLBACK"
                else Nothing
        (WellParsedSql pieces, False) -> do
            noSqlToRun <- Streaming.all_
                (\p -> isWhiteSpacePiece p || isCommentPiece p)
                pieces
            if noSqlToRun
                then pure $ Left "The migration seems to be empty"
                else do
                    let isOpenAfter wasOpen p = case p of
                            BeginTransaction _ -> True
                            _ -> not (isTransactionEndingPiece p) && wasOpen

                    leavesTransactionOpen <- Streaming.fold_ isOpenAfter
                                                             False
                                                             id
                                                             pieces

                    pure $ Right $ if leavesTransactionOpen
                        then
                            Just
                                "no-txn migration BEGINs but does not COMMIT or ROLLBACK transaction"
                        else Nothing
    pure $ MigrationCheck <$> migEndsTransaction
