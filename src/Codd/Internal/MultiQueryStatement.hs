module Codd.Internal.MultiQueryStatement (SqlPiece(..), InTransaction(..), blockToText, multiQueryStatement_, runSingleStatementInternal_, parseMultiStatement) where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>))
import Control.Monad (void, forM_, guard)
import Data.Attoparsec.Text (Parser, anyChar, asciiCI, atEnd, char, endOfInput, many', many1, manyTill, parseOnly, peekChar, string, takeWhile, takeWhile1)
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.Char as Char
import qualified Data.Text as Text
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

-- | A bit like singleStatement_, but parses the query to run each command separately, following these criteria:
--   1. If already in a transaction, then this will only execute statements one-by-one only if there's at least one COPY FROM STDIN.. statement and no parsing errors.
--   2. If not in a transaction, then this will execute statements one-by-one unless there are parsing errors.
--   * In all cases when there is a parsing error, all of the supplied SQL will be executed at-once (not one-by-one) and a message will be printed to the output.
multiQueryStatement_ :: MonadIO m => InTransaction -> DB.Connection -> Text -> m ()
multiQueryStatement_ inTxn conn q =
    -- Fallback into regular implicitly transaction-enveloped statement in case of parsing error
    case parseMultiStatement q of
        Left _ -> parseErrorFallbackExec
        Right stms -> do
            if Text.concat (map blockToText stms) /= q then
                parseErrorFallbackExec
            else if not (hasCopyFromStdin stms) && inTxn == InTransaction then
                singleStatement_ conn q
            else
                forM_ stms $ \sql -> runSingleStatementInternal_ conn sql

    where
        hasCopyFromStdin xs = any id [True | (CopyFromStdinPiece _ _ _) <- xs]
        parseErrorFallbackExec = do
            liftIO $ putStrLn $ "NOTE: An internal inconsistency was detected in the multi query parser. We'll try to run the whole query as a single statement. Please report this as a bug."
            singleStatement_ conn q

runSingleStatementInternal_ :: MonadIO m => DB.Connection -> SqlPiece -> m ()
runSingleStatementInternal_ _ (CommentPiece _) = pure ()
runSingleStatementInternal_ _ (WhiteSpacePiece _) = pure ()
runSingleStatementInternal_ conn (OtherSqlPiece s) = singleStatement_ conn s
runSingleStatementInternal_ conn (CopyFromStdinPiece copyStm copyData _) = liftIO $ do
    DB.copy_ conn $ DB.Query (encodeUtf8 copyStm)
    DB.putCopyData conn $ encodeUtf8 $ copyData
    DB.putCopyData conn $ encodeUtf8 "\n"
    void $ DB.putCopyEnd conn

parseMultiStatement :: Text -> Either String [SqlPiece]
parseMultiStatement = parseOnly (sqlPiecesParser <* endOfInput) 

data SqlPiece = CommentPiece Text | WhiteSpacePiece Text | CopyFromStdinPiece Text Text Text | OtherSqlPiece Text
    deriving stock (Show, Eq)

blockToText :: SqlPiece -> Text
blockToText (CommentPiece s) = s
blockToText (WhiteSpacePiece s) = s
blockToText (OtherSqlPiece s) = s
blockToText (CopyFromStdinPiece s1 s2 s3) = s1 <> s2 <> s3

sqlPiecesParser :: Parser [SqlPiece]
sqlPiecesParser = manyTill sqlPieceParser endOfInput
    where
        sqlPieceParser = CommentPiece <$> commentParser <|> (WhiteSpacePiece <$> takeWhile1 Char.isSpace) <|> copyFromStdinParser <|> (OtherSqlPiece <$> anySqlPieceParser)
        anySqlPieceParser = spaceSeparatedTokensToParser [AllUntilEndOfStatement]

data SqlToken = CITextToken Text | SqlIdentifier | CommaSeparatedIdentifiers | Optional [SqlToken] | AllUntilEndOfStatement
spaceSeparatedTokensToParser :: [SqlToken] -> Parser Text
spaceSeparatedTokensToParser allTokens =
    case allTokens of
        [] -> pure ""
        (token1 : []) -> parseToken token1
        (token1 : tokens) -> do
            s1 <- parseToken token1
            spaces <- case (s1, token1) of
                ("", Optional _) -> pure ""
                _ -> commentOrSpaceParser True <|> pure ""

            others <- spaceSeparatedTokensToParser tokens
            pure $ s1 <> spaces <> others
    
    where
        parseToken =
            \case
                Optional t -> spaceSeparatedTokensToParser t <|> pure ""
                CITextToken t -> asciiCI t
                SqlIdentifier -> blockParserOfType (==DoubleQuotedIdentifier) <|> takeWhile1 (\c -> not (Char.isSpace c) && c /= ',' && c /= ')') -- TODO: What are the valid chars for identifiers?? Figure it out!!
                CommaSeparatedIdentifiers -> do
                    firstIdent <- spaceSeparatedTokensToParser [SqlIdentifier]
                    anySpace <- commentOrSpaceParser False
                    otherIdents <- fmap Text.concat $ many' (spaceSeparatedTokensToParser [CITextToken ",", SqlIdentifier])
                    pure $ firstIdent <> anySpace <> otherIdents
                AllUntilEndOfStatement -> do
                    t1 <- takeWhile (\c -> not (isPossibleStartingChar c) && c /= ';')
                    mc <- peekChar
                    case mc of
                        Nothing -> pure t1
                        Just ';' -> do
                            void $ char ';'
                            pure $ t1 <> ";"
                        Just _ -> do
                            t2 <- (Text.concat . map snd) <$> many1 blockParser <|> Parsec.take 1
                            -- After reading blocks or just a char, we still need to find a semi-colon to get a statement from start to finish!
                            t3 <- parseToken AllUntilEndOfStatement
                            pure $ t1 <> t2 <> t3

-- parseCopy :: Text -> Either String SqlPiece
-- parseCopy t = parseOnly copyFromStdinParser t

-- Urgh.. parsing statements precisely would benefit a lot from importing the lex parser
copyFromStdinParser :: Parser SqlPiece
copyFromStdinParser = do
    stmt <- spaceSeparatedTokensToParser [CITextToken "COPY", SqlIdentifier, Optional [CITextToken "(", Optional [CommaSeparatedIdentifiers], CITextToken ")"], CITextToken "FROM", CITextToken "STDIN", AllUntilEndOfStatement]
    seol <- eol
    (copyData, parsedSuffix) <- parseUntilSuffix '\n' copyLastLine <|> parseUntilSuffix '\r' copyLastLine -- Sorry Windows users, but you come second..
    pure $ CopyFromStdinPiece (stmt <> seol) copyData parsedSuffix

    where
        copyLastLine = do
            eol1 <- eol
            s <- string "\\."
            eol2 <- eol <|> ("" <$ endOfInput)
            pure $ eol1 <> s <> eol2

-- | Parse until finding the suffix string, returning both contents and suffix separately, in this order.
--   Can return an empty string for the contents.
parseUntilSuffix :: Char -> Parser Text -> Parser (Text, Text)
parseUntilSuffix suffixFirstChar wholeSuffix = do
    s <- takeWhile (/= suffixFirstChar)
    (parsedSuffix, succeeded) <- (,True) <$> wholeSuffix <|> pure ("", False)
    if succeeded then pure (s, parsedSuffix) else do
        nc <- char suffixFirstChar
        (remain, recParsedSuffix) <- parseUntilSuffix suffixFirstChar wholeSuffix
        pure (Text.snoc s nc <> remain, recParsedSuffix)

-- | Parses 0 or more consecutive white-space or comments
commentOrSpaceParser :: Bool -> Parser Text
commentOrSpaceParser atLeastOne = if atLeastOne then Text.concat <$> many1 (commentParser <|> takeWhile1 Char.isSpace) else Text.concat <$> many' (commentParser <|> takeWhile1 Char.isSpace)

commentParser :: Parser Text        
commentParser = do
    (commentType, commentInit) <- (DoubleDashComment,) <$> string "--" <|> (CStyleComment,) <$> string "/*"
    bRemaining <- blockInnerContentsParser commentType
    pure $ commentInit <> bRemaining

data BlockType = DoubleDashComment | CStyleComment | DollarQuotedBlock Text | DoubleQuotedIdentifier | SingleQuotedString deriving stock (Show, Eq)

isPossibleStartingChar :: Char -> Bool
isPossibleStartingChar c = c == '-' || c == '/' || c == '"' || c == '$' || c == '\''

isPossibleEndingChar :: BlockType -> Char -> Bool
isPossibleEndingChar DoubleDashComment c = c == '\n'
isPossibleEndingChar CStyleComment c = c == '*'
isPossibleEndingChar (DollarQuotedBlock _) c = c == '$'
isPossibleEndingChar DoubleQuotedIdentifier c = c == '"'
isPossibleEndingChar SingleQuotedString c = c == '\''

blockBeginParser :: Parser (BlockType, Text)
blockBeginParser =
    (DoubleDashComment,) <$> string "--"
    <|> (CStyleComment,) <$> string "/*"
    <|> dollarBlockParser
    <|> (DoubleQuotedIdentifier,) <$> string "\""
    <|> (SingleQuotedString,) <$> string "'"
    where
        dollarBlockParser = do
            void $ char '$'
            b <- takeWhile (/= '$')
            void $ char '$'
            let tb = "$" <> b <> "$"
            pure (DollarQuotedBlock tb, tb)

blockEndingParser :: BlockType -> Parser Text
blockEndingParser =
    \case
        DoubleDashComment -> eol <|> (pure "" <* endOfInput)
        CStyleComment -> string "*/"
        DollarQuotedBlock q -> string q -- TODO: CASE INSENSITIVE!
        DoubleQuotedIdentifier -> string "\""
        SingleQuotedString -> string "'"

eol :: Parser Text
eol = string "\n" <|> string "\t\n"

blockParser :: Parser (BlockType, Text)
blockParser = do
    (bt, bBegin) <- blockBeginParser
    bRemaining <- blockInnerContentsParser bt
    pure $ (bt, bBegin <> bRemaining)

blockParserOfType :: (BlockType -> Bool) -> Parser Text
blockParserOfType p = do
    (bt, c) <- blockParser
    guard $ p bt
    pure c

blockInnerContentsParser :: BlockType -> Parser Text
blockInnerContentsParser bt = do
    t <- case bt of
            SingleQuotedString -> parseWithEscapeChar (== '\'') -- '' escaping seems not to be explicitly implemented (this parser understands it as two consecutive strings)?
            DoubleQuotedIdentifier -> parseWithEscapeChar (== '"') -- "" escaping seems to be the same as above..
            _ -> takeWhile (not . isPossibleEndingChar bt)
    done <- atEnd
    if done then pure t
    else do
        mEndingQuote <- Just <$> blockEndingParser bt <|> pure Nothing
        case mEndingQuote of
            Nothing -> do
                -- Could mean we found e.g. '*' inside a C-Style comment block, but not followed by '/'
                c <- anyChar
                remain <- blockInnerContentsParser bt
                pure $ Text.snoc t c <> remain
            Just endingQuote -> pure $ t <> endingQuote

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Does not consume the ending char.
parseWithEscapeChar :: (Char -> Bool) -> Parser Text
parseWithEscapeChar untilc = do
    cs <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
    nextChar <- peekChar
    case nextChar of
        Just '\\' -> do
            c <- Parsec.take 2
            rest <- parseWithEscapeChar untilc
            pure $ cs <> c <> rest
        _ -> pure cs