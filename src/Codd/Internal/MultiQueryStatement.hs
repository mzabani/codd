module Codd.Internal.MultiQueryStatement (mqStatement_, parseMultiStatement) where

import Prelude hiding (takeWhile)

import Codd.Query (execvoid_)
import Control.Applicative ((<|>))
import Control.Monad (void, forM_)
import Data.Attoparsec.Text (Parser, anyChar, atEnd, char, endOfInput, many1, parseOnly, peekChar, string, takeWhile)
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
-- and I don't know anything about lex/flex/alex.. also, we dong't need to be as good as psql in parsing SQL; we just need to find rough-enough boundaries
-- (psql counts parentheses, for instance) to split up commands.
-- Note 2: The CLI utility psql does the same and splits up commands before sending them to the server. See https://github.com/postgres/postgres/blob/master/src/fe_utils/psqlscan.l

mqStatement_ :: MonadIO m => DB.Connection -> Text -> m ()
mqStatement_ conn q =
    -- Fallback into regular command in case of parsing error
    case parseOnly (multiStatementParser <* endOfInput) q of
        Left _ -> singleStatementExec
        Right stms ->
            if Text.concat stms /= q then do
                liftIO $ putStrLn $ "NOTE: An internal inconsistency was detected in the multi statement parser. You should receive an error when adding this migration if this would mean an error when running it, so it shouldn't be a problem. Still, please report this as a bug."
                singleStatementExec
            else
                forM_ stms $ \sql -> execvoid_ conn (DB.Query $ encodeUtf8 sql)

    where
        singleStatementExec = execvoid_ conn $ DB.Query (encodeUtf8 q)

parseMultiStatement :: Text -> Either String [Text]
parseMultiStatement = parseOnly (multiStatementParser <* endOfInput)

multiStatementParser :: Parser [Text]
multiStatementParser = many1 singleStatementParser

singleStatementParser :: Parser Text
singleStatementParser = do
    t1 <- takeWhile (\c -> not (isPossibleStartingChar c) && c /= ';')
    mc <- peekChar
    case mc of
        Nothing -> if t1 == "" then fail "Empty input" else pure t1 -- This could be a single statement without a semi-colon
        Just ';' -> do
            void $ char ';'
            -- To avoid trailing comments being parsed as a separate command, look for a comment now as well
            trailingComment <- commentParser <|> pure ""
            pure $ Text.snoc t1 ';' <> trailingComment
        Just _ -> do
            (_, t2) <- blockParser
            -- After reading an entire block, we still need to find a semi-colon to get a statement from start to finish!
            -- One exception: eof
            done <- atEnd
            if done then pure $ t1 <> t2
            else do
                more <- singleStatementParser
                pure $ t1 <> t2 <> more

commentParser :: Parser Text
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

blockInnerContentsParser :: BlockType -> Parser Text
blockInnerContentsParser bt = do
    t <- case bt of
            SingleQuotedString -> parseWithEscapeChar (== '\'') -- TODO: '' escaping!!
            DoubleQuotedIdentifier -> parseWithEscapeChar (== '"') -- TODO: "" escaping
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