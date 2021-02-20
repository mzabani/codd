module Codd.Internal.MultiQueryStatement (InTransaction(..), multiQueryStatement_, runSingleStatementInternal_) where

import Prelude hiding (takeWhile)

import Codd.Parsing (SqlPiece(..), ParsedSql(..))
import Control.Monad (void, forM_)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Copy as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import qualified Database.PostgreSQL.Simple.Internal as PGInternal
import qualified Database.PostgreSQL.LibPQ as PQ
import UnliftIO (Exception, MonadIO, liftIO, handle, throwIO)

-- Multi-query statements are automatically enveloped in a single transaction by the server. This happens because according to
-- https://www.postgresql.org/docs/12/libpq-exec.html, "Multiple queries sent in a single PQexec call are processed in a single transaction,
-- unless there are explicit BEGIN/COMMIT commands included in the query string to divide it into multiple transactions."
-- This creates problem for statements that can't run inside a single transaction (changing enums, creating dbs etc.)
-- Because that seems to be at libpq level, we need to parse SQL (ugh..) and detect plPGSQL bodies and standard SQL to
-- split commands up.. I expect problems from this, really.
-- Note 1: Maybe alex works to translate psqlscan.l to Haskell? Seems like a rather complicated translation when I look at the source,
-- and I don't know anything about lex/flex/alex.. also, we dong't need to be as good as psql in parsing SQL; we just need to find statement boundaries
-- (psql counts parentheses, for instance) to split them up by that.
-- Note 2: The CLI utility psql does the same and splits up commands before sending them to the server. See https://github.com/postgres/postgres/blob/master/src/fe_utils/psqlscan.l

data SqlStatementException = SqlStatementException { sqlStatement :: Text, psimpleError :: DB.SqlError } deriving stock (Show)
instance Exception SqlStatementException

-- | Runs SQL that could be either a row-returning or count-returning statement. Just lie postgresql-simple, throws exceptions in case of SQL errors.
singleStatement_ :: MonadIO m => DB.Connection -> Text -> m ()
singleStatement_ conn sql = do
    res <- liftIO $ PGInternal.exec conn $ encodeUtf8 sql
    status <- liftIO $ PQ.resultStatus res
    case status of
        PQ.CommandOk -> pure ()
        PQ.TuplesOk -> pure ()
        _ -> liftIO $ handle (throwIO . SqlStatementException sql) $
            -- Throw to catch and re-throw.. a bit nasty, but should be ok 
                PGInternal.throwResultError "singleStatement_" res status

data InTransaction = InTransaction | NotInTransaction deriving stock Eq

-- | A bit like singleStatement_, but following these criteria:
--   1. If already in a transaction, then this will only execute statements one-by-one only if there's at least one COPY FROM STDIN.. statement.
--   2. If not in a transaction, then this will execute statements one-by-one.
multiQueryStatement_ :: MonadIO m => InTransaction -> DB.Connection -> ParsedSql -> m ()
multiQueryStatement_ inTxn conn sql =
    case sql of
        ParseFailSqlText t -> singleStatement_ conn t
        WellParsedSql t stms ->
            if not (hasCopyFromStdin stms) && inTxn == InTransaction then
                singleStatement_ conn t
            else
                forM_ stms $ \stm -> runSingleStatementInternal_ conn stm

    where
        hasCopyFromStdin xs = any id [True | (CopyFromStdinPiece _ _ _) <- NE.toList xs]

runSingleStatementInternal_ :: MonadIO m => DB.Connection -> SqlPiece -> m ()
runSingleStatementInternal_ _ (CommentPiece _) = pure ()
runSingleStatementInternal_ _ (WhiteSpacePiece _) = pure ()
runSingleStatementInternal_ conn (OtherSqlPiece s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CopyFromStdinPiece copyStm copyData _) = liftIO $ do
    DB.copy_ conn $ DB.Query (encodeUtf8 copyStm)
    DB.putCopyData conn $ encodeUtf8 $ copyData
    DB.putCopyData conn $ encodeUtf8 "\n"
    void $ DB.putCopyEnd conn

