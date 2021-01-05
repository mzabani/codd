module Codd.Internal.MultiQueryStatement (SqlBlock(..), SqlStatement(..), blockToText, finalCommentsTextOnly, noTxnStatement_, noTxnSingleStatement_, inTxnStatement_, parseMultiStatement, statementTextOnly) where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>))
import Control.Monad (void, forM_, guard)
import Data.Attoparsec.Text (Parser, anyChar, atEnd, char, endOfInput, many', many1, parseOnly, peekChar, string, takeWhile, takeWhile1)
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import UnliftIO (MonadIO, liftIO)

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

noTxnStatement_ :: MonadIO m => DB.Connection -> Text -> m ()
noTxnStatement_ conn q =
    -- Fallback into regular command in case of parsing error
    case parseMultiStatement q of
        Left _ -> parseErrorFallbackExec
        Right stms ->
            if Text.concat (map blockToText stms) /= q then
                parseErrorFallbackExec
            else
                forM_ stms $ \sql -> noTxnSingleStatement_ conn sql

    where
        parseErrorFallbackExec = do
            liftIO $ putStrLn $ "NOTE: An internal inconsistency was detected in the multi statement parser. We now have to run this migration inside a transaction, so it may fail. Please report this as a bug."
            inTxnStatement_ conn $ DB.Query (encodeUtf8 q)

-- | This is sad, but when running each command separately, there'll be row-returning statements such as SELECT as well
-- as Int64 returning ones such as INSERT, ALTER TABLE etc..
noTxnSingleStatement_ :: MonadIO m => DB.Connection -> SqlBlock -> m ()
noTxnSingleStatement_ conn (SqlBlock (SelectStatement s) comm) = liftIO $ void $ DB.query_ @(DB.Only Int) conn $ DB.Query $ encodeUtf8 $ s <> comm <> "\n;SELECT 1;"
noTxnSingleStatement_ conn (SqlBlock (OtherStatement s) comm) = liftIO $ void $ DB.execute_ conn $ DB.Query $ encodeUtf8 $ s <> comm

inTxnStatement_ :: MonadIO m => DB.Connection -> DB.Query -> m ()
inTxnStatement_ conn s =
    -- Same problem as noTxnSingleStatement_, but if we're in a in-txn migration, we can append a "SELECT 1" and we're good. No need to parse things.
    liftIO $ void $ DB.query_ @(DB.Only Int) conn $ s <> "\n;SELECT 1;"

parseMultiStatement :: Text -> Either String [SqlBlock]
parseMultiStatement = parseOnly (multiStatementParserDetailed <* endOfInput) 

multiStatementParserDetailed :: Parser [SqlBlock]
multiStatementParserDetailed = many1 singleStatementParser

data SqlStatement = SelectStatement Text | OtherStatement Text
    deriving stock (Show, Eq)

-- | SqlBlock is a statement succeeded by some comment and white-space (or empty Text if none).
data SqlBlock = SqlBlock SqlStatement Text
    deriving stock (Show, Eq)

blockToText :: SqlBlock -> Text
blockToText (SqlBlock (SelectStatement s) comm) = s <> comm
blockToText (SqlBlock (OtherStatement s) comm) = s <> comm

statementTextOnly :: SqlBlock -> Text
statementTextOnly (SqlBlock (SelectStatement s) _) = s
statementTextOnly (SqlBlock (OtherStatement s) _) = s

finalCommentsTextOnly :: SqlBlock -> Text
finalCommentsTextOnly (SqlBlock _ c) = c

-- | Parses statements into the actual SQL statement first and comments+whitespace second.
singleStatementParser :: Parser SqlBlock
singleStatementParser = do
    -- 0 or more comments and white-space
    commentsBefore <- Text.concat <$> many' commentOrSpaceParser
    
    -- 0 or more blocks of any type, until a semi-colon or eof
    stm <- blocksUntilEofOrSemiColon
    guard $ stm /= ""

    -- 0 or more comments and white-space
    commentsAfter <- Text.concat <$> many' commentOrSpaceParser
    pure $ sectionsToBlock commentsBefore stm commentsAfter

    where
        blocksUntilEofOrSemiColon = do
            t1 <- takeWhile (\c -> not (isPossibleStartingChar c) && c /= ';')
            mc <- peekChar
            case mc of
                Nothing -> pure t1
                Just ';' -> do
                    void $ char ';'
                    pure $ Text.snoc t1 ';'
                Just _ -> do
                    t2 <- Text.concat . map snd <$> many1 blockParser <|> Parsec.take 1
                    -- After reading blocks or just the char, we still need to find a semi-colon to get a statement from start to finish!
                    t3 <- blocksUntilEofOrSemiColon
                    pure $ t1 <> t2 <> t3
                    
        sectionsToBlock :: Text -> Text -> Text -> SqlBlock
        sectionsToBlock befStm stm aftStm =
            case Text.toCaseFold (Text.takeWhile (not . Char.isSpace) stm) of
                "select" -> SqlBlock (SelectStatement (befStm <> stm)) aftStm
                _ -> SqlBlock (OtherStatement (befStm <> stm)) aftStm

commentOrSpaceParser :: Parser Text
commentOrSpaceParser = commentParser <|> takeWhile1 Char.isSpace
    where
        commentParser = do
            s1 <- takeWhile Char.isSpace
            (commentType, commentInit) <- (DoubleDashComment,) <$> string "--" <|> (CStyleComment,) <$> string "/*"
            bRemaining <- blockInnerContentsParser commentType
            s2 <- takeWhile Char.isSpace
            pure $ s1 <> commentInit <> bRemaining <> s2

data BlockType = DoubleDashComment | CStyleComment | DollarQuotedBlock Text | DoubleQuotedIdentifier | SingleQuotedString deriving stock Show

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

-- blockParserOfType :: (BlockType -> Bool) -> Parser Text
-- blockParserOfType pred = do
--     (bt, c) <- blockParser
--     guard $ pred bt
--     pure c

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