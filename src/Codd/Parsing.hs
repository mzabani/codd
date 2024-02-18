{-|

This module contains parsers that are helpful to separate queries from each other by finding query boundaries: semi-colons, but not when inside a string or a parenthesised expression, for example.

Parsing is made necessary because multi-query statements are automatically enveloped in a single transaction by the server. This happens because according to
<https://www.postgresql.org/docs/12/libpq-exec.html>, /"Multiple queries sent in a single PQexec call are processed in a single transaction, unless there are explicit BEGIN\/COMMIT commands included in the query string to divide it into multiple transactions"/.

This creates problem for statements that can't run inside a single transaction (changing enums, creating dbs etc.).
Because that happens at libpq's level, we need to parse SQL (ouch..) and detect query boundaries so we send them one at a time.

Parsing is far from trivial, but has a few advantages once implemented:  

  * It makes reading migrations from disk in streaming fashion possible, meaning we can run arbitrarily large migrations with bounded memory.  

  * It makes SQL errors when applying migrations much more granular.  

  * It is necessary to support COPY, since that is not a statement we can just send to the server; it uses a different protocol.

Ideally we would use the parser from upstream, at <https://github.com/postgres/postgres/blob/master/src/fe_utils/psqlscan.l>, but that will require
some FFI since it contains C code. But long term we really should.
-}
module Codd.Parsing
    ( AddedSqlMigration(..)
    , AppliedMigration(..)
    , CoddCommentParseResult(..)
    , EnvVars(..)
    , FileStream(..)
    , SqlPiece(..)
    , ParsedSql(..)
    , PureStream(..)
    , SqlMigration(..)
    , connStringParser
    , hoistAddedSqlMigration
    , isCommentPiece
    , isTransactionEndingPiece
    , isWhiteSpacePiece
    , piecesToText
    , sqlPieceText
    , parsedSqlText
    , parseSqlMigration
    , parseWithEscapeCharProper
    , parseAddedSqlMigration
    , parseAndClassifyMigration
    , parseMigrationTimestamp
    , parseSqlPiecesStreaming
    , substituteEnvVarsInSqlPiecesStream
    , toMigrationTimestamp

  -- Exported for tests
    , ParserState(..)
    , copyFromStdinAfterStatementParser
    , parseSqlPiecesStreaming'
    ) where

import           Control.Applicative            ( (<|>)
                                                , optional
                                                )
import           Control.Monad                  ( guard
                                                , void
                                                , when
                                                )
import           Control.Monad.Identity         ( Identity(..) )
import           Control.Monad.Trans            ( MonadTrans
                                                , lift
                                                )
import           Control.Monad.Trans.Except     ( runExceptT
                                                , throwE
                                                )
import           Control.Monad.Trans.Resource   ( MonadThrow(throwM) )
import           Data.Attoparsec.Text           ( Parser
                                                , asciiCI
                                                , char
                                                , endOfInput
                                                , endOfLine
                                                , many'
                                                , many1
                                                , parseOnly
                                                , peekChar
                                                , sepBy1
                                                , skipWhile
                                                , string
                                                , takeWhile
                                                , takeWhile1
                                                )
import qualified Data.Attoparsec.Text          as Parsec
import           Data.Bifunctor                 ( first )
import qualified Data.Char                     as Char
import           Data.Kind                      ( Type )
import           Data.List                      ( nub
                                                , sortOn
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( fromGregorianValid
                                                , secondsToDiffTime
                                                )
import           Data.Time.Clock                ( UTCTime(..) )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import           Network.URI                    ( URI(..)
                                                , URIAuth(..)
                                                , parseURI
                                                , unEscapeString
                                                )
import           Prelude                 hiding ( takeWhile )
import           Streaming                      ( MFunctor(hoist)
                                                , Of(..)
                                                )
import qualified Streaming.Prelude             as Streaming
import           Streaming.Prelude              ( Stream )
import           UnliftIO                       ( IORef
                                                , MonadIO
                                                , liftIO
                                                )
import           UnliftIO.Environment           ( lookupEnv )
import           UnliftIO.Exception             ( Exception )
import           UnliftIO.Resource              ( ReleaseKey )


-- | Contains either SQL parsed in pieces or the full original SQL contents
-- for cases where parsing was foregone.
data ParsedSql m =
  UnparsedSql
  !Text
  -- ^ The full file contents
  | WellParsedSql (Stream (Of SqlPiece) m ())

data SqlMigration m = SqlMigration
    { migrationName           :: FilePath
    , migrationSql            :: ParsedSql m
    , migrationInTxn          :: Bool
    , migrationCustomConnInfo :: Maybe ConnectInfo
    , migrationEnvVars        :: Map Text Text
    }

data AddedSqlMigration m = AddedSqlMigration
    { addedSqlMig       :: SqlMigration m
    , addedSqlTimestamp :: DB.UTCTimestamp
    }

data AppliedMigration = AppliedMigration
    { appliedMigrationName      :: FilePath
    , appliedMigrationTimestamp :: DB.UTCTimestamp
    -- ^ The migration's timestamp as extracted from its file name.
    , appliedMigrationAt        :: UTCTime
    -- ^ When the migration was effectively applied.
    }

data FileStream m = FileStream
    { filePath   :: FilePath
    , releaseKey :: IORef (Maybe ReleaseKey)
    , fileStream :: Stream (Of Text) m ()
    }

-- | Pure streams can be consumed as many times
-- as necessary because consuming doesn't advance
-- any stream state, unlike `FileStream`.
newtype PureStream m = PureStream {
    unPureStream :: Stream (Of Text) m ()
}
  deriving newtype (Semigroup)

class MigrationStream (m :: Type -> Type) (s :: Type) where
    readFullContents :: s -> m Text
    migStream :: s -> Stream (Of Text) m ()

instance Monad m => MigrationStream m (PureStream m) where
    readFullContents PureStream { unPureStream } =
        Streaming.fold_ (<>) "" id unPureStream
    migStream PureStream { unPureStream } = unPureStream

instance MonadIO m => MigrationStream m (FileStream m) where
    -- | Reads entire file from disk again as so to
    -- be immune to the state of the Stream.
    readFullContents FileStream { filePath } = liftIO $ Text.readFile filePath
    migStream FileStream { fileStream } = fileStream

-- TODO: This should probably not be in Parsing.hs
hoistAddedSqlMigration
    :: Monad m
    => (forall x . m x -> n x)
    -> AddedSqlMigration m
    -> AddedSqlMigration n
hoistAddedSqlMigration f (AddedSqlMigration sqlMig tst) = AddedSqlMigration
    (hoistSqlMig sqlMig)
    tst
  where
    hoistSqlMig mig = mig { migrationSql = hoistParsedSql $ migrationSql mig }
    hoistParsedSql (UnparsedSql   t     ) = UnparsedSql t
    hoistParsedSql (WellParsedSql stream) = WellParsedSql $ hoist f stream

data SectionOption = OptInTxn Bool | OptNoParse Bool
  deriving stock (Eq, Show)

data SqlPiece = CommentPiece !Text | WhiteSpacePiece !Text | CopyFromStdinStatement !Text | CopyFromStdinRows !Text | CopyFromStdinEnd !Text | BeginTransaction !Text | RollbackTransaction !Text | CommitTransaction !Text | OtherSqlPiece !Text
  deriving stock (Show, Eq)

data ParsingException = ParsingException
    { sqlFragment :: Text
    , errorMsg    :: String
    }
    deriving stock Show

instance Exception ParsingException

-- | This class indicates the ability to query environment variables.
-- It might seem overly polymorphic to have such a thing, but
-- `parseAndClassifyMigration` would be pure if not only for querying
-- env vars and being able to throw exceptions!
-- TODO: It should probably be in Codd.Environment, but that forms
-- a cycle as of today.
class EnvVars m where
    -- | Returns a map with a key for every asked-for environment
    -- variable, with empty values if they are undefined.
    getEnvVars :: [Text] -> m (Map Text Text)

instance EnvVars IO where
    getEnvVars vars =
        Map.fromList
            <$> traverse
                    (\var -> lookupEnv (Text.unpack var)
                        >>= \mVal -> pure (var, maybe "" Text.pack mVal)
                    )
                    vars

instance (MonadTrans t, Monad m, EnvVars m) => EnvVars (t m) where
    getEnvVars = lift . getEnvVars

{-# INLINE parseSqlPiecesStreaming #-} -- See Note [Inlining and specialization]
-- | This should be a rough equivalent to `many sqlPieceParser` for Streams.
parseSqlPiecesStreaming
    :: forall m
     . MonadThrow m
    => Stream (Of Text) m ()
    -> Stream (Of SqlPiece) m ()
parseSqlPiecesStreaming = parseSqlPiecesStreaming' sqlPieceParser

{-# INLINE parseSqlPiecesStreaming' #-} -- See Note [Inlining and specialization]
-- | This should be a rough equivalent to `many parser` for Streams.
parseSqlPiecesStreaming'
    :: forall m
     . MonadThrow m
    => (ParserState -> Parser ([SqlPiece], ParserState))
    -> Stream (Of Text) m ()
    -> Stream (Of SqlPiece) m ()
parseSqlPiecesStreaming' parser contents = Streaming.concat parseResultsStream
  -- NOTE: attoparsec's `parseWith` looks at first glance like it might help, but
  -- the parser supplied to it is fixed, and we need a state-sensitive parser for each
  -- chunk, because chunks might be incomplete SQL pieces.
  where
    mkErrorMsg errorMsg =
        "An internal parsing error occurred. This is most likely a bug in codd. Please file a bug report.\
    \ In the meantime, you can add this migration by writing \"-- codd: no-parse\" as its very first line.\
    \ The downside is this migration will be in-txn and COPY will not be supported. It will also be put in\
    \ memory all at once instead of being read and applied in streaming fashion.\
    \ The more detailed parsing error is: "
            ++ errorMsg

    -- | Takes a parser result and does one of:
    -- 1. Throws an error if it failed. Remember, we assume our SQL parsers can't fail.
    -- 2. If one `SqlPiece`s was successfuly parsed, great. Because there could still be unconsumed text,
    --    recursively parse that unconsumed text, and prepend the parsed `SqlPiece` to whatever the recursively
    --    applied parser returns.
    -- 3. If the parser didn't fail nor succeed, it wasn't yet fed sufficient input. This then returns
    --    an empty list of `SqlPiece`s and a parser continuation.
    -- In all cases, this returns the modified parser state too (one of `OutsideCopy` or `InsideCopy`).
    handleParseResult
        :: ParserState
        -> Text
        -> Maybe (Parsec.Result ([SqlPiece], ParserState))
      -- ^ `Nothing` means this text piece is the very first to be parsed.
      -- `Just` means this is the result of parsing the supplied text piece (second argument
      -- to this function).
        -> m
               ( [SqlPiece]
               , Maybe
                     (Text -> Parsec.Result ([SqlPiece], ParserState))
               , ParserState
               )
    handleParseResult !parserState !textPiece !mParseResult =
        case mParseResult of
            Nothing -> handleParseResult
                parserState
                textPiece
                (Just $ Parsec.parse (parser parserState) textPiece)
            Just (Parsec.Fail _ _ errorMsg) ->
                throwM $ ParsingException textPiece $ mkErrorMsg errorMsg
            Just (Parsec.Done unconsumedInput (sqlPieces, newParserState)) ->
                if textPiece == ""
                    -- Special case: after consuming the empty string "" - which is interpreted as 
                    -- EOF -, it is possible unconsumed input remained from a previous partial parse.
                    -- Notice that unconsumed input at the end is probably garbage, but we still want to
                    -- be defensive and include it (in case there's a problem in the parsers).
                    -- So we can't forget to return that last unconsumed fragment in that case.
                    then pure
                        ( [ sqlPiece
                          | sqlPiece <- sqlPieces
                          , sqlPiece /= OtherSqlPiece ""
                          ]
                        ++ [ OtherSqlPiece unconsumedInput
                           | unconsumedInput /= ""
                           ]
                        , Nothing
                        , newParserState
                        )
                    else first3 (sqlPieces ++) <$> handleParseResult
                        newParserState
                        unconsumedInput
                        (Just $ Parsec.parse (parser newParserState)
                                             unconsumedInput
                        )
            Just (Parsec.Partial nextContinue) ->
                pure ([], Just nextContinue, parserState)

    parseResultsStream :: Stream (Of [SqlPiece]) m ()
    parseResultsStream =
        Streaming.scanM
                (\(_, !mContinue, !parserState) !textPiece -> handleParseResult
                    parserState
                    textPiece
                    (mContinue <*> Just textPiece)
                )
                (pure ([], Nothing, OutsideCopy))
                (pure . fst3)
      -- SQL files' last statement don't need to end with a semi-colon, and because of
      -- that they could end up as a partial match in those cases.
      -- When applying parsing continuations the empty string is used to signal/match
      -- endOfInput. So we append a final empty string to the original stream to
      -- parse the last SQL statement properly regardless of semi-colon.
      -- Also, because empty strings are so special, we filter them out from the
      -- input stream just in case.
            $  Streaming.filter (/= "") contents
            <> Streaming.yield ""

    fst3 (a, _, _) = a
    first3 f (a, b, c) = (f a, b, c)

parsedSqlText :: Monad m => ParsedSql m -> m Text
parsedSqlText (UnparsedSql t) = pure t
parsedSqlText (WellParsedSql s) =
    Streaming.fold_ (<>) "" id $ Streaming.map sqlPieceText s

sqlPieceText :: SqlPiece -> Text
sqlPieceText (CommentPiece           s) = s
sqlPieceText (WhiteSpacePiece        s) = s
sqlPieceText (BeginTransaction       s) = s
sqlPieceText (RollbackTransaction    s) = s
sqlPieceText (CommitTransaction      s) = s
sqlPieceText (OtherSqlPiece          s) = s
sqlPieceText (CopyFromStdinStatement s) = s
sqlPieceText (CopyFromStdinRows      s) = s
sqlPieceText (CopyFromStdinEnd       s) = s

mapSqlPiece :: (Text -> Text) -> SqlPiece -> SqlPiece
mapSqlPiece f = \case
    CommentPiece           s -> CommentPiece (f s)
    WhiteSpacePiece        s -> WhiteSpacePiece (f s)
    BeginTransaction       s -> BeginTransaction (f s)
    RollbackTransaction    s -> RollbackTransaction (f s)
    CommitTransaction      s -> CommitTransaction (f s)
    OtherSqlPiece          s -> OtherSqlPiece (f s)
    CopyFromStdinStatement s -> CopyFromStdinStatement (f s)
    CopyFromStdinRows      s -> CopyFromStdinRows (f s)
    CopyFromStdinEnd       s -> CopyFromStdinEnd (f s)

data ParserState = OutsideCopy | InsideCopy
  deriving stock (Eq, Show)

sqlPieceParser :: ParserState -> Parser ([SqlPiece], ParserState)
sqlPieceParser parserState = case parserState of
    OutsideCopy -> first (: []) <$> outsideCopyParser
    InsideCopy  -> copyFromStdinAfterStatementParser 65536
  where
    outsideCopyParser =
        (, OutsideCopy)
            .   CommentPiece
            <$> commentParser
            <|> (, OutsideCopy)
            .   WhiteSpacePiece
            <$> takeWhile1
                    (\c -> Char.isSpace c || c == '\n' || c == '\r' || c == '\t'
                    )
            <|> (, InsideCopy)
            <$> copyFromStdinStatementParser
            <|> (, OutsideCopy)
            .   BeginTransaction
            <$> beginTransactionParser
            <|> (, OutsideCopy)
            .   RollbackTransaction
            <$> rollbackTransactionParser
            <|> (, OutsideCopy)
            .   CommitTransaction
            <$> commitTransactionParser
            <|> (, OutsideCopy)
            .   OtherSqlPiece
            <$> anySqlPieceParser
    beginTransactionParser =
        spaceSeparatedTokensToParser
                [CITextToken "BEGIN", AllUntilEndOfStatement]
            <|> spaceSeparatedTokensToParser
                    [ CITextToken "START"
                    , CITextToken "TRANSACTION"
                    , AllUntilEndOfStatement
                    ]
    rollbackTransactionParser = spaceSeparatedTokensToParser
        [CITextToken "ROLLBACK", AllUntilEndOfStatement]
    commitTransactionParser = spaceSeparatedTokensToParser
        [CITextToken "COMMIT", AllUntilEndOfStatement]
    anySqlPieceParser = spaceSeparatedTokensToParser [AllUntilEndOfStatement]

data SqlToken = CITextToken !Text | SqlIdentifier | CommaSeparatedIdentifiers | Optional ![SqlToken] | CustomParserToken (Parser Text) | AllUntilEndOfStatement

spaceSeparatedTokensToParser :: [SqlToken] -> Parser Text
spaceSeparatedTokensToParser allTokens = case allTokens of
    []                -> pure ""
    [token1         ] -> parseToken token1
    (token1 : tokens) -> do
        s1     <- parseToken token1
        spaces <- case (s1, token1) of
            ("", Optional _) -> pure ""
            _                -> commentOrSpaceParser True <|> pure ""

        others <- spaceSeparatedTokensToParser tokens
        pure $ s1 <> spaces <> others
  where
    parseToken = \case
        Optional          t       -> spaceSeparatedTokensToParser t <|> pure ""
        CITextToken       t       -> asciiCI t
        CustomParserToken p       -> p
        SqlIdentifier             -> objIdentifier
        CommaSeparatedIdentifiers -> listOfAtLeast1 [SqlIdentifier] ","
        AllUntilEndOfStatement    -> do
            t1 <- takeWhile
                (\c -> not (isPossibleBlockStartingChar c) && c /= ';')
            mc <- peekChar
            case mc of
                Nothing  -> pure t1
                Just ';' -> do
                    void $ char ';'
                    pure $ t1 <> ";"
                Just _ -> do
                    t2 <- Text.concat <$> many1 blockParser <|> Parsec.take 1
                    -- After reading blocks or just a char, we still need to find a semi-colon to get a statement from start to finish!
                    t3 <- parseToken AllUntilEndOfStatement
                    pure $ t1 <> t2 <> t3

listOfAtLeast1 :: [SqlToken] -> Text -> Parser Text
listOfAtLeast1 elementTokens separator = do
    firstEl  <- spaceSeparatedTokensToParser elementTokens
    -- We use pure "" only to allow for a space before the first separator..
    otherEls <- Text.concat <$> many'
        ( spaceSeparatedTokensToParser
        $ CustomParserToken (pure "")
        : CITextToken separator
        : elementTokens
        )
    pure $ firstEl <> otherEls

-- Urgh.. parsing statements precisely would benefit a lot from importing the lex parser
copyFromStdinStatementParser :: Parser SqlPiece
copyFromStdinStatementParser = do
    stmt <- spaceSeparatedTokensToParser
        [ CITextToken "COPY"
        , SqlIdentifier
        , Optional
            [ CITextToken "("
            , Optional [CommaSeparatedIdentifiers]
            , CITextToken ")"
            ]
        , CITextToken "FROM"
        , CITextToken "STDIN"
        , AllUntilEndOfStatement
        ]
    seol <- eol
    pure $ CopyFromStdinStatement $ stmt <> seol

-- | Parser to be used after "COPY FROM STDIN..." has been parsed with `copyFromStdinStatementParser`.
copyFromStdinAfterStatementParser :: Int -> Parser ([SqlPiece], ParserState)
copyFromStdinAfterStatementParser approxMaxChunkSize = do
    when (approxMaxChunkSize <= 0)
        $ error "approxMaxChunkSize must be strictly positive"
    -- This stateful parser is tricky to get right but it's proven to be much faster than simpler
    -- alternatives I've tried (e.g. taking lines and concatenating them was incredibly slow for some reason)
    (contents, (_, _, terminatorLen)) <- Parsec.runScanner
        (0 :: Int, 0 :: Int, 0 :: Int)
        (\(lenTotalParsed, lenCurrentLine, lenTerminatorSoFar) c ->
            if lenTotalParsed >= approxMaxChunkSize && lenCurrentLine == 0
                then Nothing -- Only stop at the beginning of a new line
                else if lenCurrentLine /= lenTerminatorSoFar && c == '\n'
                    then Just (1 + lenTotalParsed, 0, 0)
                    else if lenCurrentLine /= lenTerminatorSoFar
                        then Just (1 + lenTotalParsed, 1 + lenCurrentLine, 0)
                        else case (lenTerminatorSoFar, c) of
                            (0, '\\') ->
                                Just (1 + lenTotalParsed, 1 + lenCurrentLine, 1)
                            (1, '.') ->
                                Just (1 + lenTotalParsed, 1 + lenCurrentLine, 2)
                            (2, '\n') ->
                                Just (1 + lenTotalParsed, 1 + lenCurrentLine, 3) -- Last char in terminator, but `Just` because it needs to be in the parsed contents
                            (3, _   ) -> Nothing -- Terminator with len=3 means it's been parsed, so end here.
                            (_, '\n') -> Just (1 + lenTotalParsed, 0, 0)
                            _ ->
                                Just (1 + lenTotalParsed, 1 + lenCurrentLine, 0)
        )
    isEnd :: Bool <- Parsec.atEnd
    let fullTerminator = "\\.\n"
        eofTerminator  = "\\."
        terminatorFound | terminatorLen == 3          = fullTerminator
                        | isEnd && terminatorLen == 2 = eofTerminator
                        | otherwise                   = ""
        rows = Text.dropEnd terminatorLen contents
    case (rows, terminatorFound) of
        ("", "") -> pure ([], InsideCopy) -- This should be impossible
        ("", _ ) -> pure ([CopyFromStdinEnd terminatorFound], OutsideCopy)
        (_ , "") -> pure ([CopyFromStdinRows rows], InsideCopy)
        (_ , _ ) -> pure
            ( [CopyFromStdinRows rows, CopyFromStdinEnd terminatorFound]
            , OutsideCopy
            )

-- | Parses 0 or more consecutive white-space or comments
commentOrSpaceParser :: Bool -> Parser Text
commentOrSpaceParser atLeastOne = if atLeastOne
    then Text.concat <$> many1 (commentParser <|> takeWhile1 Char.isSpace)
    else Text.concat <$> many' (commentParser <|> takeWhile1 Char.isSpace)

commentParser :: Parser Text
commentParser = doubleDashComment <|> cStyleComment

eol :: Parser Text
eol = string "\n" <|> string "\r\n"

-- | Blocks are the name we give to some expressions that have a beginning and an end, inside of which
-- semicolons are not to be considered statement boundaries. These include strings, comments,
-- parenthesised expressions and dollar-quoted strings.
-- For now, this assumes standard_conforming_strings is always on.
blockParser :: Parser Text
blockParser =
    -- Unicode escaped strings aren't explicitly implemented, but work with the current parser.
    -- Since single quotes can't be an escape character for them (see https://www.postgresql.org/docs/current/sql-syntax-lexical.html),
    -- backslashes will be treated like a regular character - which works for us -, and if other characters are chosen as the escape character,
    -- our parsers will treat those also as regular characters, which should be fine.
    -- This seems fragile, but our tests will error out if changes make this unsupported.
    parseStdConformingString
        <|> parenthesisedExpression
        <|> cStyleComment
        <|> dollarStringParser
        <|> doubleDashComment
        <|> doubleQuotedIdentifier
        <|> cStyleEscapedString

-- | A character that may be the first of a block. This needs to match parsers in `blockParser`, and is only useful
-- to optimize our parsers by avoiding backtracking through usage of `takeWhile` and similar.
isPossibleBlockStartingChar :: Char -> Bool
isPossibleBlockStartingChar c =
    c
        == '('
        || c
        == '-'
        || c
        == '/'
        || c
        == '"'
        || c
        == '$'
        || c
        == '\''
        || c
        == 'E'


dollarStringParser :: Parser Text
dollarStringParser = do
    void $ char '$'
    b <- takeWhile (/= '$')
    void $ char '$'
    let dollarSep = "$" <> b <> "$"
    rest <- go dollarSep
    pure $ dollarSep <> rest

  where
    go dollarSep = do
        t      <- takeWhile (/= '$')
        ending <- optional $ string dollarSep <|> "" <$ endOfInput
        case ending of
            Nothing -> do
                void $ char '$'
                rest <- go dollarSep
                pure $ t <> "$" <> rest
            Just e -> pure $ t <> e

doubleDashComment :: Parser Text
doubleDashComment = do
    begin <- string "--"
    rest  <- Parsec.takeWhile (\c -> c /= '\n' && c /= '\r')
    end   <- eol <|> "" <$ endOfInput
    pure $ begin <> rest <> end

cStyleComment :: Parser Text
cStyleComment = do
    openComment <- string "/*"
    rest        <- Parsec.scan
        (1 :: Int, False, False)
        (\(openCommentCount, hasPartialOpening, hasPartialClosing) c ->
            nothingWhenDone $ case (hasPartialOpening, hasPartialClosing, c) of
          -- Handle slashes in all possible contexts
                (False, False, '/') -> Just (openCommentCount, True, False)
                (False, True , '/') -> Just (openCommentCount - 1, False, False)
                (True , False, '/') -> Just (openCommentCount, True, False)
                -- Handle asterisks in all possible contexts
                (False, False, '*') -> Just (openCommentCount, False, True)
                (True , False, '*') -> Just (openCommentCount + 1, False, False)
                (False, True , '*') -> Just (openCommentCount, False, True)
                -- Handle other characters
                (True, True, _) ->
                    error
                        "Report this as a bug in codd: C Style comment parser invalid state"
                _ -> Just (openCommentCount, False, False)
        )
    end <- string "/" <|> "" <$ endOfInput -- We are generous with eof and allow even invalid SQL in many places
    pure $ openComment <> rest <> end
  where
    nothingWhenDone (Just (0, _, _)) = Nothing
    nothingWhenDone x                = x

parenthesisedExpression :: Parser Text
parenthesisedExpression = do
    openParen <- string "(" <|> fail "No open paren"
    rest      <- insideParenParser
    pure $ openParen <> rest
  where
    insideParenParser :: Parser Text
    insideParenParser = do
        more <- takeWhile
            (\c -> not (isPossibleBlockStartingChar c) && c /= ')')
        nextChar <- peekChar
        case nextChar of
            Nothing  -> pure more -- Be gentle with EOF
            Just ')' -> do
                closeParen <- string ")"
                pure $ more <> closeParen
            Just _ -> do
                blockOrOtherwise <- blockParser <|> Parsec.take 1
                rest             <- insideParenParser -- We're still inside an openParen after parsing a block or a character
                pure $ more <> blockOrOtherwise <> rest

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Does not consume the ending char and RETURNS any
-- backslash escape chars in the result.
-- Use `parseWithEscapeCharProper` to exclude the escape chars from the result.
-- This function is useful if you want the original parsed contents.
parseWithEscapeCharPreserve :: (Char -> Bool) -> Parser Text
parseWithEscapeCharPreserve untilc = do
    cs       <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
    nextChar <- peekChar
    case nextChar of
        Just '\\' -> do
            c    <- Parsec.take 2
            rest <- parseWithEscapeCharPreserve untilc
            pure $ cs <> c <> rest
        _ -> pure cs

-- | Identifiers can be in fully qualified form `"database"."schema"."objectname"`,
-- with and without double quoting, e.g.: `"schema".tablename`, or just a simple `tablename`.
objIdentifier :: Parser Text
objIdentifier =
    let singleIdentifier =
            doubleQuotedIdentifier
                <|> takeWhile1
                        (\c ->
                            not (Char.isSpace c)
                                && c
                                /= ','
                                && c
                                /= '.'
                                && c
                                /= ')'
                        ) -- TODO: What are the valid chars for identifiers?? Figure it out!!
    in  listOfAtLeast1 [CustomParserToken singleIdentifier] "."

doubleQuotedIdentifier :: Parser Text
doubleQuotedIdentifier = do
    openingQuote <- string "\""
    rest         <- parseWithEscapeCharPreserve (== '"')
    ending       <- string "\""
    pure $ openingQuote <> rest <> ending

-- | Parses a single quoted NON standard conforming string, i.e. strings that use backslash as an escape character, and are marked
-- by beginning with an `E`. Consecutive simple quotes are also treated as a single quote, just like in std conforming strings.
-- See https://www.postgresql.org/docs/current/sql-syntax-lexical.html
cStyleEscapedString :: Parser Text
cStyleEscapedString = do
    openingChars <- string "E'"
    rest         <- Parsec.scan
        (False, False)
        (\(lastCharWasBackslash, lastCharWasSingleQuote) c ->
            case ((lastCharWasBackslash, lastCharWasSingleQuote), c) of
                ((False, False), '\'') -> Just (False, True)
                ((False, True ), '\'') -> Just (False, False) -- Two consecutive single quotes are not end of string
                ((False, True ), _   ) -> Nothing -- A single quote not preceded by \ and followed by not-a-single-quote is end of string
                ((False, False), '\\') -> Just (True, False)
                ((False, False), _   ) -> Just (False, False) -- "regular" character
                ((True , False), _   ) -> Just (False, False) -- Any character after a backslash must be included in the string
                ((True, True), _) ->
                    error
                        "Please submit this as a bug report to codd, saying both backslash and single quote were last char in cStyleEscapedString"
        )
    pure $ openingChars <> rest

-- | Parses a single quoted standard conforming string, i.e. strings that use '' as a representation of a single quote, and
-- takes any other character literally.
parseStdConformingString :: Parser Text
parseStdConformingString = do
    openingQuote <- string "'"
    rest         <- Parsec.scan
        False
        (\lastCharWasQuote c -> case (lastCharWasQuote, c) of
            (False, '\'') -> Just True
            (True , '\'') -> Just False -- Two consecutive single quotes represent a single quote, not end of string
            (True , _   ) -> Nothing -- One single quote followed by any other character means that single quote was end of string 
            (False, _   ) -> Just False
        )
    pure $ openingQuote <> rest

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Stops at and does not consume the first predicate-passing
-- char, and does not include escape chars in the returned value,
-- as one would expect.
parseWithEscapeCharProper :: (Char -> Bool) -> Parser Text
parseWithEscapeCharProper untilc = do
    cs       <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
    nextChar <- peekChar
    case nextChar of
        Nothing   -> pure cs
        Just '\\' -> do
            void $ char '\\'
            c    <- Parsec.take 1
            rest <- parseWithEscapeCharProper untilc
            pure $ cs <> c <> rest
        Just _ -> pure cs

eitherToMay :: Either a b -> Maybe b
eitherToMay (Left  _) = Nothing
eitherToMay (Right v) = Just v

-- | Parses a URI with scheme 'postgres' or 'postgresql', as per https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING.
-- The difference here is that URIs with a query string or with a fragment are not allowed.
uriConnParser :: Text -> Either String ConnectInfo
uriConnParser line = runIdentity $ runExceptT @String @_ @ConnectInfo $ do
    case parseURI (Text.unpack line) of
        Nothing       -> throwE "Connection string is not a URI"
        Just URI {..} -> do
            when
                    (         Text.toLower (Text.pack uriScheme)
                    `notElem` ["postgres:", "postgresql:"]
                    )
                $ throwE
                      "Connection string's URI scheme must be 'postgres' or 'postgresql'"
            case uriAuthority of
                Nothing -> throwE
                    "Connection string must contain at least user and host"
                Just URIAuth {..} -> do
                    let connectDatabase =
                            unEscapeString $ trimFirst '/' uriPath
                        hasQueryString = not $ null uriQuery
                        hasFragment    = not $ null uriFragment
                    when (null connectDatabase) $ throwE
                        "Connection string must contain a database name"
                    when (hasQueryString || hasFragment)
                        $ throwE
                              "Custom parameters are not supported in connection strings. Make sure your connection URI does not have a query string or query fragment"

                    -- Ports are not mandatory and are defaulted to 5432 when not present
                    let port = if null uriPort then Just 5432 else Nothing
                    case
                            port <|> eitherToMay
                                (parseOnly
                                    (Parsec.decimal <* endOfInput)
                                    (Text.pack $ trimFirst ':' uriPort)
                                )
                        of
                            Nothing ->
                                throwE "Invalid port in connection string"
                            Just connectPort -> do
                                let
                                    (unEscapeString . trimLast '@' -> connectUser, unEscapeString . trimLast '@' . trimFirst ':' -> connectPassword)
                                        = break (== ':') uriUserInfo
                                pure ConnectInfo
                                    { connectHost     = unEscapeString
                                        $ unescapeIPv6 uriRegName
                                    , connectPort
                                    , connectUser
                                    , connectPassword
                                    , connectDatabase
                                    }
  where
    unescapeIPv6 :: String -> String
    unescapeIPv6 = trimFirst '[' . trimLast ']'

    trimFirst :: Char -> String -> String
    trimFirst c s@(c1 : cs) = if c == c1 then cs else s
    trimFirst _ s           = s

    trimLast :: Char -> String -> String
    trimLast c s = case Text.unsnoc $ Text.pack s of
        Nothing            -> s
        Just (t, lastChar) -> if lastChar == c then Text.unpack t else s


keywordValueConnParser :: Text -> Either String ConnectInfo
keywordValueConnParser line = runIdentity $ runExceptT $ do
    kvs <- sortOn fst <$> parseOrFail
        (singleKeyVal `Parsec.sepBy` takeWhile1 (== ' '))
        (Text.strip line)
        "Invalid connection string"
    ConnectInfo
        <$> getVal "host"     Nothing     txtToString    kvs
        <*> getVal "port"     (Just 5432) Parsec.decimal kvs
        <*> getVal "user"     Nothing     txtToString    kvs
        <*> getVal "password" (Just "")   txtToString    kvs
        <*> getVal "dbname"   Nothing     txtToString    kvs
  where
    getVal key def parser pairs =
        case (map snd $ filter ((== key) . fst) pairs, def) of
            ([], Nothing) ->
                throwE
                    $  "Connection string must contain a value for '"
                    <> Text.unpack key
                    <> "'"
            ([], Just v) -> pure v
            ([vt], _) ->
                parseOrFail parser vt
                    $  "Connection string key '"
                    <> Text.unpack key
                    <> "' is in an unrecognizable format"
            _ ->
                throwE
                    $  "Duplicate key '"
                    <> Text.unpack key
                    <> "' found in connection string."

    txtToString = Text.unpack <$> Parsec.takeText
    parseOrFail parser txt errorMsg =
        case parseOnly (parser <* endOfInput) txt of
            Left  _ -> throwE errorMsg
            Right v -> pure v

    singleKeyVal = do
        key <- takeWhile1 (\c -> c /= ' ' && c /= '=')
        skipJustSpace
        void $ char '='
        skipJustSpace
        value <-
            takeQuotedString
            <|> parseWithEscapeCharProper
                    (\c -> c == ' ' || c == '\'' || c == '\\')
            <|> pure ""
        pure (key, value)

    takeQuotedString = do
        void $ char '\''
        s <- parseWithEscapeCharProper (== '\'')
        void $ char '\''
        pure s

-- | Parses a string in one of libpq allowed formats. See https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING.
-- The difference here is that only a subset of all connection parameters are allowed.
connStringParser :: Parser ConnectInfo
connStringParser = do
    connStr <- Parsec.takeWhile1 (not . Parsec.isEndOfLine)
        <|> fail "Empty connection string"
    -- Very poor connection string type handling here
    let connStrParser =
            if ("postgres://" `Text.isPrefixOf` Text.toLower connStr)
                || ("postgresql://" `Text.isPrefixOf` Text.toLower connStr)
            then
                uriConnParser
            else
                keywordValueConnParser
    case connStrParser connStr of
        Left err ->
            fail
                $ "Connection string is not a valid libpq connection string. A valid libpq connection string is either in the format 'postgres://username[:password]@host:port/database_name', with URI-encoded (percent-encoded) components except for the host and bracket-surround IPv6 addresses, or in the keyword value pairs format, e.g. 'dbname=database_name host=localhost user=postgres' with escaping for spaces, quotes or empty values. More info at https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING. Specific error: "
                <> err
        Right c -> pure c


optionParser :: Parser SectionOption
optionParser = do
    skipJustSpace
    x <-
        inTxn
        <|> noTxn
        <|> noParse
        <|> fail
                "Valid options after '-- codd:' are 'in-txn', 'no-txn' or 'no-parse' (the last implies in-txn)"
    skipJustSpace
    return x
  where
    inTxn   = string "in-txn" >> pure (OptInTxn True)
    noTxn   = string "no-txn" >> pure (OptInTxn False)
    noParse = string "no-parse" >> pure (OptNoParse True)


skipJustSpace :: Parser ()
skipJustSpace = skipWhile (== ' ')

data CoddCommentParseResult a = CoddCommentWithGibberish !Text | CoddCommentSuccess !a

-- | Parses a "-- codd: option1, option2" line. Consumes all available input in case
-- of malformed codd options!
coddCommentParser :: Parser (CoddCommentParseResult [SectionOption])
coddCommentParser = do
    void $ string "--"
    skipJustSpace
    void $ string "codd:"
    skipJustSpace
    parseOptions
        <|> CoddCommentWithGibberish
        .   Text.stripEnd
        <$> Parsec.takeText
  where
    parseOptions = do
        opts <-
            optionParser `sepBy1` (skipJustSpace >> char ',' >> skipJustSpace)
        endOfLine <|> endOfInput
        pure $ CoddCommentSuccess opts

-- | Parses a "-- codd-connection: some-connection-string" line. Consumes all available input
-- in case of a malformed connection string.
coddConnStringCommentParser :: Parser (CoddCommentParseResult ConnectInfo)
coddConnStringCommentParser = do
    void $ string "--"
    skipJustSpace
    void $ string "codd-connection:"
    skipJustSpace
    parseConn <|> CoddCommentWithGibberish . Text.stripEnd <$> Parsec.takeText
  where
    parseConn = do
        connInfo <- connStringParser
        endOfLine <|> endOfInput
        pure $ CoddCommentSuccess connInfo

-- | Parses a "-- codd-env-vars: VAR1, VAR2, ..." line. Consumes all available input
-- in case of malformed options.
-- Based on https://stackoverflow.com/a/2821201, but our regex for env vars is
-- even a bit more lax than the proposed one. It is [a-zA-Z_0-9]+
coddEnvVarsCommentParser :: Parser (CoddCommentParseResult [Text])
coddEnvVarsCommentParser = do
    void $ string "--"
    skipJustSpace
    void $ string "codd-env-vars:"
    skipJustSpace
    parseVarNames
        <|> CoddCommentWithGibberish
        .   Text.stripEnd
        <$> Parsec.takeText
  where
    parseVarNames = do
        vars <-
            singleVarNameParser
                `sepBy1` (skipJustSpace >> char ',' >> skipJustSpace)
        endOfLine <|> endOfInput
        pure $ CoddCommentSuccess vars
    singleVarNameParser = Parsec.takeWhile1
        (\c ->
            Char.isAsciiLower c
                || Char.isAsciiUpper c
                || c
                == '_'
                || Char.isDigit c
        )

isWhiteSpacePiece :: SqlPiece -> Bool
isWhiteSpacePiece (WhiteSpacePiece _) = True
isWhiteSpacePiece _                   = False

isCommentPiece :: SqlPiece -> Bool
isCommentPiece (CommentPiece _) = True
isCommentPiece _                = False

isTransactionEndingPiece :: SqlPiece -> Bool
isTransactionEndingPiece (RollbackTransaction _) = True
isTransactionEndingPiece (CommitTransaction   _) = True
isTransactionEndingPiece _                       = False

-- | Replaces arbitrary strings with env vars by direct text substition of ${ENVVARNAME}
textReplaceEnvVars :: Map Text Text -> Text -> Text
textReplaceEnvVars = Map.foldlWithKey'
    (\accF var val -> Text.replace ("${" <> var <> "}") val . accF)
    id

substituteEnvVarsInSqlPiecesStream
    :: Monad m
    => Map Text Text -- ^ Names and values of env vars to query from environment and substitute
    -> Stream (Of SqlPiece) m r
    -> Stream (Of SqlPiece) m r
substituteEnvVarsInSqlPiecesStream envVars s =
    let subsfunc = textReplaceEnvVars envVars
    in  Streaming.map (mapSqlPiece subsfunc) s

{-# INLINE parseAndClassifyMigration #-} -- See Note [Inlining and specialization]
-- | Parses only comments and white-space that precede the first SQL statement and
-- extracts from them custom options and a custom connection string when
-- they exist, or returns a good error message otherwise.
parseAndClassifyMigration
    :: (MonadThrow m, MigrationStream m s, EnvVars m)
    => s
    -> m
           ( Either
                 String
                 ( Map Text Text
                 , [SectionOption]
                 , Maybe ConnectInfo
                 , ParsedSql m
                 )
           )
parseAndClassifyMigration sqlStream = do
  -- There is no easy way to avoid parsing at least up until and including
  -- the first sql statement.
  -- This means always advancing `sqlStream` beyond its first sql statement
  -- too.
  -- We are relying a lot on the parser to do a good job, which it apparently does,
  -- but it may be interesting to think of alternatives here.
    consumedPieces :> sqlPiecesBodyStream <-
        Streaming.toList
        $ Streaming.span (\p -> isCommentPiece p || isWhiteSpacePiece p)
        $ parseSqlPiecesStreaming
        $ migStream sqlStream
    let firstComments      = filter isCommentPiece consumedPieces
        envVarParseResults = mapMaybe
            ( (\case
                  Left  _        -> Nothing
                  Right parseRes -> Just parseRes
              )
            . ( parseOnly (coddEnvVarsCommentParser <* endOfInput)
              . sqlPieceText
              )
            )
            firstComments
        malformedEnvVarComment = listToMaybe
            [ line | CoddCommentWithGibberish line <- envVarParseResults ]
    case malformedEnvVarComment of
        Just badLine ->
            pure
                $ Left
                $ "Malformed -- codd-env-vars comment. Make sure there is at least one environment variable, that they are separated by a comma and that they only contain letters, numbers and underscore. Original comment: "
                <> Text.unpack badLine
        Nothing -> do
            let
                envVarNames = mconcat
                    [ vars | CoddCommentSuccess vars <- envVarParseResults ]
            envVars <- getEnvVars envVarNames
            let allOptSections = mapMaybe
                    ( (\case
                          Left  _    -> Nothing
                          Right opts -> Just opts
                      )
                    . ( parseOnly (coddCommentParser <* endOfInput)
                      . textReplaceEnvVars envVars
                      . sqlPieceText
                      )
                    )
                    firstComments
                customConnString = mapMaybe
                    ( (\case
                          Left  _        -> Nothing
                          Right connInfo -> Just connInfo
                      )
                    . ( parseOnly (coddConnStringCommentParser <* endOfInput)
                      . textReplaceEnvVars envVars
                      . sqlPieceText
                      )
                    )
                    firstComments
                headMay []      = Nothing
                headMay (x : _) = Just x

            -- Urgh.. is there no better way of pattern matching the stuff below?
            eExtr <- case (allOptSections, customConnString) of
                (_ : _ : _, _) ->
                    pure
                        $ Left
                              "There must be at most one '-- codd:' comment in the first lines of a migration"
                (_, _ : _ : _) ->
                    pure
                        $ Left
                              "There must be at most one '-- codd-connection:' comment in the first lines of a migration"
                _ -> case (headMay allOptSections, headMay customConnString) of
                    (Just (CoddCommentWithGibberish badOptions), _) ->
                        pure
                            $  Left
                            $  "The options '"
                            <> Text.unpack badOptions
                            <> "' are invalid. Valid options are either 'in-txn' or 'no-txn'"
                    (_, Just (CoddCommentWithGibberish _badConn)) ->
                        pure
                            $ Left
                                  "Connection string is not a valid libpq connection string. A valid libpq connection string is either in the format 'postgres://username[:password]@host:port/database_name', with URI-encoded (percent-encoded) components except for the host and bracket-surround IPv6 addresses, or in the keyword value pairs format, e.g. 'dbname=database_name host=localhost user=postgres' with escaping for spaces, quotes or empty values. More info at https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING"
                    (Just (CoddCommentSuccess opts), Just (CoddCommentSuccess conn))
                        -> pure $ Right (opts, Just conn)
                    (Just (CoddCommentSuccess opts), Nothing) ->
                        pure $ Right (opts, Nothing)
                    (Nothing, Just (CoddCommentSuccess conn)) ->
                        pure $ Right ([], Just conn)
                    (Nothing, Nothing) -> pure $ Right ([], Nothing)

            case eExtr of
                Left err -> pure $ Left err
                Right (opts, customConnStr)
                    |
                  -- Detect "-- codd: no-parse"
                      OptNoParse True `elem` opts -> do
                    -- Remember: the original Stream (Of Text) has already been advanced
                    -- beyond its first SQL statement, but **only** if this is an IO-based
                    -- Stream. If this is a pure Stream it will start from the beginning when
                    -- consumed. That is why we prefer to use a special action to retrieve
                    -- the full migration's contents.
                        fullMigContents <- readFullContents sqlStream
                        pure
                            $ Right
                                  ( envVars
                                  , opts
                                  , customConnStr
                                  , UnparsedSql fullMigContents
                                  )
                    | otherwise -> pure $ Right
                        ( envVars
                        , opts
                        , customConnStr
                    -- Prepend consumedPieces to preserve the property that SqlMigration objects
                    -- will hold the full original SQL.
                        , WellParsedSql
                        $  substituteEnvVarsInSqlPiecesStream envVars
                        $  Streaming.each consumedPieces
                        <> sqlPiecesBodyStream
                        )



piecesToText :: Foldable t => t SqlPiece -> Text
piecesToText = foldr ((<>) . sqlPieceText) ""

{-# INLINE parseSqlMigration #-} -- See Note [Inlining and specialization]
parseSqlMigration
    :: forall m s
     . (MonadThrow m, MigrationStream m s, EnvVars m)
    => String
    -> s
    -> m (Either String (SqlMigration m))
parseSqlMigration name s = (toMig =<<) <$> parseAndClassifyMigration s
  where
    dupOpts opts = length (nub opts) < length opts
    checkOpts :: [SectionOption] -> Maybe String
    checkOpts opts
        | inTxn opts && noTxn opts
        = Just
            "Choose either in-txn, no-txn or leave blank for the default of in-txn"
        | dupOpts opts
        = Just "Some options are duplicated"
        | otherwise
        = Nothing
    inTxn opts = OptInTxn False `notElem` opts || OptInTxn True `elem` opts
    noTxn opts = OptInTxn False `elem` opts

    mkMig
        :: (Map Text Text, [SectionOption], Maybe ConnectInfo, ParsedSql m)
        -> SqlMigration m
    mkMig (migrationEnvVars, opts, customConnString, sql) = SqlMigration
        { migrationName           = name
        , migrationSql            = sql
        , migrationInTxn          = inTxn opts
        , migrationCustomConnInfo = customConnString
        , migrationEnvVars
        }

    toMig
        :: (Map Text Text, [SectionOption], Maybe ConnectInfo, ParsedSql m)
        -> Either String (SqlMigration m)
    toMig x@(_, ops, _, _) = case checkOpts ops of
        Just err -> Left err
        _        -> Right $ mkMig x


parseAddedSqlMigration
    :: (MonadThrow m, MigrationStream m s, EnvVars m)
    => String
    -> s
    -> m (Either String (AddedSqlMigration m))
parseAddedSqlMigration name s = do
    sqlMig <- parseSqlMigration name s
    pure $ AddedSqlMigration <$> sqlMig <*> parseMigrationTimestamp name

-- | Converts an arbitrary UTCTime (usually the system's clock when adding a migration) to a Postgres timestamptz
-- in by rounding it to the nearest second to ensure Haskell and Postgres times both behave well. Returns both the rounded UTCTime
-- and the Postgres timestamp.
toMigrationTimestamp :: UTCTime -> (UTCTime, DB.UTCTimestamp)
toMigrationTimestamp (UTCTime day diffTime) =
    let t = UTCTime day (fromInteger $ round diffTime) in (t, DB.Finite t)

-- | Parses the UTC timestamp from a migration's name.
parseMigrationTimestamp :: String -> Either String DB.UTCTimestamp
parseMigrationTimestamp name = parseOnly
    (migTimestampParser <|> fail
        ("Could not find migration timestamp from its name: '" <> name <> "'")
    )
    (Text.pack name)
  where
    dash               = void $ char '-'
    migTimestampParser = do
        yyyy <- Parsec.decimal
        dash
        monthMm <- Parsec.decimal
        dash
        dd <- Parsec.decimal
        dash
        hh <- Parsec.decimal
        guard $ hh >= 0 && hh <= 23
        dash
        minuteMm <- Parsec.decimal
        guard $ minuteMm >= 0 && minuteMm < 60
        dash
        ss <- Parsec.decimal
        guard $ ss >= 0 && ss < 60
        case fromGregorianValid yyyy monthMm dd of
            Nothing -> fail "Invalid date"
            Just day ->
                pure
                    $ DB.Finite
                    $ UTCTime day
                    $ secondsToDiffTime
                    $ hh
                    * 3600
                    + minuteMm
                    * 60
                    + ss


-- Note [Inlining and specialization]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Benchmarks of this module show specializing parsing functions called by other modules
-- has a big impact on performance: around 40% savings in total run time.
-- So one would expect SPECIALIZE (see https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html?highlight=specialize#specialize-pragma)
-- pragmas to appear in this module, but instead of that we use INLINE.
-- The reason is that SPECIALIZE requires a type signature to specialize to,
-- and while callers in benchmarks code are in IO, callers inside the app can
-- be in a more layered mtl stack. Thus it is easy to write code that specializes
-- in benchmarks but silently doesn't in the app.
-- INLINE is not really what we need, but it does seem to trigger specialization (at least
-- benchmarks match those with a specialized function). It is more cumbersome because
-- all functions in the call stack need their own INLINE each, but more reliable when it
-- comes to triggering in different modules.
