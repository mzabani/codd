module Codd.Internal.MultiQueryStatement (SqlBlock(..), SqlStatement(..), InTransaction(..), blockToText, finalCommentsTextOnly, multiQueryStatement_, runSingleStatementInternal_, parseMultiStatement, statementTextOnly) where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>))
import Control.Monad (void, forM_, guard)
import Data.Attoparsec.Text (Parser, anyChar, asciiCI, atEnd, char, endOfInput, isEndOfLine, many', many1, parseOnly, peekChar, string, takeWhile, takeWhile1)
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.Char as Char
import qualified Data.Either as Either
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
        hasCopyFromStdin xs = any id [True | SqlBlock (CopyFromStdinStatement _ _) _ <- xs]
        parseErrorFallbackExec = do
            liftIO $ putStrLn $ "NOTE: An internal inconsistency was detected in the multi query parser. We'll try to run the whole query as a single statement. Please report this as a bug."
            singleStatement_ conn q

-- | This is sad, but when running each command separately, there'll be row-returning statements such as SELECT as well
-- as Int64 returning ones such as INSERT, ALTER TABLE etc..
runSingleStatementInternal_ :: MonadIO m => DB.Connection -> SqlBlock -> m ()
runSingleStatementInternal_ conn (SqlBlock (SelectStatement s) comm) = singleStatement_ conn $ s <> comm
runSingleStatementInternal_ conn (SqlBlock (OtherStatement s) comm) = singleStatement_ conn $ s <> comm
runSingleStatementInternal_ conn (SqlBlock (CopyFromStdinStatement copyStm copyData) _) = liftIO $ do
    -- Notable exception here: the comments following COPY statements are not sent to the server
    DB.copy_ conn $ DB.Query (encodeUtf8 copyStm)
    -- The last line in copyData ismultiQueryStatement_ just "\n\\.\n", not proper data, so we don't send it!
    -- Also, the very first line is the empty line that is formed because of COPY's ";" and the newline after it,
    -- so it is not sent either.
    let copyContentsLines = Text.lines copyData
    forM_ (take (length copyContentsLines - 2) (drop 1 copyContentsLines)) $ \line ->
        DB.putCopyData conn $ encodeUtf8 $ line <> "\n"
    void $ DB.putCopyEnd conn

parseMultiStatement :: Text -> Either String [SqlBlock]
parseMultiStatement = parseOnly (multiStatementParserDetailed <* endOfInput) 

multiStatementParserDetailed :: Parser [SqlBlock]
multiStatementParserDetailed = many1 singleStatementParser

data SqlStatement = SelectStatement Text | CopyFromStdinStatement Text Text | OtherStatement Text
    deriving stock (Show, Eq)

mapStatement :: (Text -> Text) -> SqlStatement -> SqlStatement
mapStatement f =
    \case
        SelectStatement s -> SelectStatement (f s)
        CopyFromStdinStatement s b -> CopyFromStdinStatement (f s) b
        OtherStatement s -> OtherStatement (f s)

-- | SqlBlock is a statement succeeded by some comment and white-space (or empty Text if none).
data SqlBlock = SqlBlock SqlStatement Text
    deriving stock (Show, Eq)

blockToText :: SqlBlock -> Text
blockToText (SqlBlock (SelectStatement s) comm) = s <> comm
blockToText (SqlBlock (OtherStatement s) comm) = s <> comm
blockToText (SqlBlock (CopyFromStdinStatement s1 s2) comm) = s1 <> s2 <> comm

statementTextOnly :: SqlBlock -> Text
statementTextOnly (SqlBlock (SelectStatement s) _) = s
statementTextOnly (SqlBlock (OtherStatement s) _) = s
statementTextOnly (SqlBlock (CopyFromStdinStatement s1 s2) _) = s1 <> s2

finalCommentsTextOnly :: SqlBlock -> Text
finalCommentsTextOnly (SqlBlock _ c) = c

-- | Parses statements into the actual SQL statement first and comments+whitespace second.
singleStatementParser :: Parser SqlBlock
singleStatementParser = do
    -- 0 or more comments and white-space
    commentsBefore <- commentOrSpaceParser False
    
    -- 0 or more blocks of any type, until a semi-colon or eof
    stm <- blocksUntilEofOrSemiColon
    guard $ stm /= Right ""

    -- 0 or more comments and white-space
    commentsAfter <- commentOrSpaceParser False
    pure $ sectionsToBlock commentsBefore stm commentsAfter

    where
        blocksUntilEofOrSemiColon = do
            t1 <- takeWhile (\c -> not (isPossibleStartingChar c) && c /= ';')
            mc <- peekChar
            case mc of
                Nothing -> pure $ Right t1
                Just ';' -> do
                    void $ char ';'

                    -- Special case: "COPY FROM STDIN ... ;\nSTDIN-CONTENTS\n\\.\n"
                    if Either.isRight (parseOnly isCopyFromStdin t1) then do
                        stdinData <- blockParserOfType (==CopyFromStdinData)
                        pure $ Left $ CopyFromStdinStatement (t1 <> ";") stdinData
                    else pure $ Right $ t1 <> ";"
                Just _ -> do
                    -- TODO: We should really separate the blockparser of CopyFromSTdinData completely to avoid
                    -- having to backtrack on it..
                    t2 <- Text.concat <$> many1 (blockParserOfType (/=CopyFromStdinData)) <|> Parsec.take 1
                    -- After reading blocks or just the char, we still need to find a semi-colon to get a statement from start to finish!
                    et3 <- blocksUntilEofOrSemiColon
                    case et3 of
                        Left _ -> fail "Internal problem with mqs parser"
                        Right t3 -> pure $ Right $ t1 <> t2 <> t3
                    
        lowerCaseFirstWord :: Text -> Text
        lowerCaseFirstWord stm = Text.toCaseFold (Text.takeWhile (not . Char.isSpace) stm)

        sectionsToBlock :: Text -> Either SqlStatement Text -> Text -> SqlBlock
        sectionsToBlock befStm estm aftStm =
            case estm of
                Left stm -> SqlBlock (mapStatement (befStm <>) stm) aftStm
                Right stm ->
                    case lowerCaseFirstWord stm of
                        "select" -> SqlBlock (SelectStatement (befStm <> stm)) aftStm
                        _ -> SqlBlock (OtherStatement (befStm <> stm)) aftStm

-- Urgh.. parsing statements precisely would benefit a lot from importing the lex parser
isCopyFromStdin :: Parser ()
isCopyFromStdin = do
    void $ asciiCI "COPY"
    void $ commentOrSpaceParser True
    void $ tableNameParser
    void $ commentOrSpaceParser True
    void $ (listOfColsParser *> commentOrSpaceParser True) <|> pure ""
    void $ asciiCI "FROM"
    void $ commentOrSpaceParser True
    void $ asciiCI "STDIN"

    where
        tableNameParser = blockParserOfType (==DoubleQuotedIdentifier) <|> takeWhile1 (not . Char.isSpace)
        colNameParser = blockParserOfType (==DoubleQuotedIdentifier) <|> takeWhile1 (\c -> not (Char.isSpace c) && c /= ',')
        commaAndColNameParser = do
            void $ commentOrSpaceParser False
            void $ char ','
            void $ commentOrSpaceParser False
            void $ colNameParser
            
        listOfColsParser = do
            void $ char '('
            void $ commentOrSpaceParser False
            void $ colNameParser
            void $ commentOrSpaceParser False
            void $ many' commaAndColNameParser
            void $ commentOrSpaceParser False
            void $ char ')'

-- | Parses 0 or more consecutive white-space or comments
commentOrSpaceParser :: Bool -> Parser Text
commentOrSpaceParser atLeastOne = if atLeastOne then Text.concat <$> many1 (commentParser <|> takeWhile1 Char.isSpace) else Text.concat <$> many' (commentParser <|> takeWhile1 Char.isSpace)
    where
        commentParser = do
            s1 <- takeWhile Char.isSpace
            (commentType, commentInit) <- (DoubleDashComment,) <$> string "--" <|> (CStyleComment,) <$> string "/*"
            bRemaining <- blockInnerContentsParser commentType
            s2 <- takeWhile Char.isSpace
            pure $ s1 <> commentInit <> bRemaining <> s2

data BlockType = DoubleDashComment | CStyleComment | DollarQuotedBlock Text | DoubleQuotedIdentifier | SingleQuotedString | CopyFromStdinData deriving stock (Show, Eq)

isPossibleStartingChar :: Char -> Bool
isPossibleStartingChar c = c == '-' || c == '/' || c == '"' || c == '$' || c == '\'' || c == '\n' -- (the \n is for stdin input after the semi-colon of COPY)

isPossibleEndingChar :: BlockType -> Char -> Bool
isPossibleEndingChar DoubleDashComment c = c == '\n'
isPossibleEndingChar CStyleComment c = c == '*'
isPossibleEndingChar (DollarQuotedBlock _) c = c == '$'
isPossibleEndingChar DoubleQuotedIdentifier c = c == '"'
isPossibleEndingChar SingleQuotedString c = c == '\''
isPossibleEndingChar CopyFromStdinData c = isEndOfLine c -- The last line of input is "\n\\.\n"

blockBeginParser :: Parser (BlockType, Text)
blockBeginParser =
    (DoubleDashComment,) <$> string "--"
    <|> (CStyleComment,) <$> string "/*"
    <|> dollarBlockParser
    <|> (DoubleQuotedIdentifier,) <$> string "\""
    <|> (SingleQuotedString,) <$> string "'"
    <|> (CopyFromStdinData,) <$> eol
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
        CopyFromStdinData -> do
            eol1 <- eol
            rest <- string "\\."
            eol2 <- eol
            pure $ eol1 <> rest <> eol2

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