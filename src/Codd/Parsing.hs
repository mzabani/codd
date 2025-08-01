-- |
--
-- This module contains parsers that are helpful to separate SQL statements from each other by finding query boundaries: semi-colons, but not when inside a string or a parenthesised expression, for example.
--
-- Parsing is made necessary because multi-query statements are automatically enveloped in a single transaction by the server. This happens because according to
-- <https://www.postgresql.org/docs/12/libpq-exec.html>, /"Multiple queries sent in a single PQexec call are processed in a single transaction, unless there are explicit BEGIN\/COMMIT commands included in the query string to divide it into multiple transactions"/.
--
-- This creates problem for statements that can't run inside a single transaction (changing enums, creating dbs etc.).
-- Because that happens at libpq's level, we need to parse SQL (ouch..) and detect query boundaries so we send them one at a time.
--
-- Parsing is far from trivial, but has a few advantages once implemented:
--
--   * It makes reading migrations from disk in streaming fashion possible, meaning we can run arbitrarily large migrations with bounded memory.
--
--   * It makes SQL errors when applying migrations much more granular.
--
--   * It is necessary to support COPY, since that is not a statement we can just send to the server; it uses a different protocol.
--
-- Ideally we would use the parser from upstream, at <https://github.com/postgres/postgres/blob/master/src/fe_utils/psqlscan.l>, but that will require
-- some FFI since it contains C code. But long term we really should.
module Codd.Parsing
  ( AddedSqlMigration (..),
    AppliedMigration (..),
    CoddCommentParseResult (..),
    EnvVars (..),
    FileStream (..),
    MigrationApplicationStatus (..),
    SqlPiece (..),
    ParsedSql (..),
    PureStream (..),
    SqlMigration (..),
    connStringParser,
    hoistAddedSqlMigration,
    isCommentPiece,
    isTransactionEndingPiece,
    isWhiteSpacePiece,
    piecesToText,
    sqlPieceText,
    parsedSqlText,
    parseSqlMigration,
    parseWithEscapeCharProper,
    parseAddedSqlMigration,
    parseAndClassifyMigration,
    parseMigrationTimestamp,
    parseSqlPiecesStreaming,
    substituteEnvVarsInSqlPiecesStream,
    toMigrationTimestamp,
    -- Exported for tests
    ParserState (..),
    SectionOption (..),
    coddConnStringCommentParser,
    copyFromStdinAfterStatementParser,
    manyStreaming,
    parseSqlPiecesStreaming',
  )
where

import Codd.Types (ConnectionString (..))
import Control.Applicative
  ( optional,
    (<|>),
  )
import Control.Monad
  ( forM_,
    guard,
    unless,
    void,
    when,
  )
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans
  ( MonadTrans,
    lift,
  )
import Control.Monad.Trans.Except
  ( runExceptT,
    throwE,
  )
import Data.Attoparsec.Text
  ( Parser,
    asciiCI,
    char,
    endOfInput,
    endOfLine,
    many',
    many1,
    parseOnly,
    peekChar,
    sepBy1,
    skipWhile,
    string,
    takeWhile,
    takeWhile1,
  )
import qualified Data.Attoparsec.Text as Parsec
import Data.Bifunctor (first)
import qualified Data.Char as Char
import qualified Data.DList as DList
import Data.Either (partitionEithers)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List
  ( nub,
    sortOn,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
  ( listToMaybe,
    mapMaybe,
  )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time
  ( DiffTime,
    fromGregorianValid,
    secondsToDiffTime,
  )
import Data.Time.Clock (UTCTime (..))
import qualified Database.PostgreSQL.Simple.FromRow as DB
import qualified Database.PostgreSQL.Simple.Time as DB
import Network.URI
  ( URI (..),
    URIAuth (..),
    parseURI,
    unEscapeString,
  )
import Streaming
  ( MFunctor (hoist),
    Of (..),
  )
import qualified Streaming.Internal as S
import Streaming.Prelude (Stream)
import qualified Streaming.Prelude as Streaming
import System.FilePath (takeFileName)
import UnliftIO
  ( IORef,
    MonadIO,
    liftIO,
    readIORef,
  )
import UnliftIO.Environment (lookupEnv)
import UnliftIO.Exception (Exception)
import UnliftIO.Resource
  ( ReleaseKey,
    release,
  )
import Prelude hiding (takeWhile)

-- | Contains either SQL parsed in pieces or the full original SQL contents
-- for cases where parsing was foregone.
data ParsedSql m
  = -- | The full file contents
    UnparsedSql
      !Text
  | WellParsedSql (Stream (Of SqlPiece) m ())

data SqlMigration m = SqlMigration
  { migrationName :: FilePath,
    migrationSql :: ParsedSql m,
    migrationInTxn :: Bool,
    migrationRequiresCoddSchema :: Bool,
    migrationCustomConnInfo :: Maybe ConnectionString,
    migrationEnvVars :: Map Text Text
  }

data AddedSqlMigration m = AddedSqlMigration
  { addedSqlMig :: SqlMigration m,
    addedSqlTimestamp :: DB.UTCTimestamp
  }

-- | Holds applied status and number of applied statements.
data MigrationApplicationStatus = NoTxnMigrationFailed Int | MigrationAppliedSuccessfully Int

instance DB.FromRow MigrationApplicationStatus where
  fromRow = do
    numAppliedStmts :: Maybe Int <- DB.field
    noTxnFailedAt :: Maybe UTCTime <- DB.field
    case (numAppliedStmts, noTxnFailedAt) of
      (Nothing, _) ->
        -- old codd_schema version where only fully applied migs were registered
        pure $ MigrationAppliedSuccessfully 0
      (Just n, Nothing) -> pure $ MigrationAppliedSuccessfully n
      (Just n, Just _) -> pure $ NoTxnMigrationFailed n

data AppliedMigration = AppliedMigration
  { appliedMigrationName :: FilePath,
    -- | The migration's timestamp as extracted from its file name.
    appliedMigrationTimestamp :: DB.UTCTimestamp,
    -- | When the migration was effectively applied.
    appliedMigrationAt :: UTCTime,
    appliedMigrationDuration :: DiffTime,
    appliedMigrationStatus :: MigrationApplicationStatus,
    appliedMigrationTxnId :: Int64,
    appliedMigrationConnId :: Int
  }

data FileStream m = FileStream
  { filePath :: FilePath,
    releaseKey :: IORef (Maybe ReleaseKey),
    fileStream :: Stream (Of Text) m ()
  }

-- | Pure streams can be consumed as many times
-- as necessary because consuming doesn't advance
-- any stream state, unlike `FileStream`.
newtype PureStream m = PureStream
  { unPureStream :: Stream (Of Text) m ()
  }
  deriving newtype (Semigroup)

class MigrationStream (m :: Type -> Type) (s :: Type) where
  readFullContents :: s -> m Text
  migStream :: s -> Stream (Of Text) m ()

instance (Monad m) => MigrationStream m (PureStream m) where
  readFullContents PureStream {unPureStream} =
    Streaming.fold_ (<>) "" id unPureStream
  migStream PureStream {unPureStream} = unPureStream

instance (MonadIO m) => MigrationStream m (FileStream m) where
  -- \| Reads entire file from disk again as so to
  -- be immune to the state of the Stream.
  readFullContents FileStream {filePath, releaseKey} = liftIO $ do
    -- This contains a copy of `closeFileStream`, but importing that would introduce circular dependencies :(
    mrkey <- readIORef releaseKey
    forM_ mrkey release
    Text.readFile filePath
  migStream FileStream {fileStream} = fileStream

-- TODO: This should probably not be in Parsing.hs
hoistAddedSqlMigration ::
  (Monad m) =>
  (forall x. m x -> n x) ->
  AddedSqlMigration m ->
  AddedSqlMigration n
hoistAddedSqlMigration f (AddedSqlMigration sqlMig tst) =
  AddedSqlMigration
    (hoistSqlMig sqlMig)
    tst
  where
    hoistSqlMig mig = mig {migrationSql = hoistParsedSql $ migrationSql mig}
    hoistParsedSql (UnparsedSql t) = UnparsedSql t
    hoistParsedSql (WellParsedSql stream) = WellParsedSql $ hoist f stream

data SectionOption = OptInTxn | OptNoTxn | OptNoParse | OptRequiresCoddSchema
  deriving stock (Eq, Ord, Show)

data SqlPiece = CommentPiece !Text | WhiteSpacePiece !Text | CopyFromStdinStatement !Text | CopyFromStdinRows !Text | CopyFromStdinEnd !Text | BeginTransaction !Text | RollbackTransaction !Text | CommitTransaction !Text | OtherSqlPiece !Text
  deriving stock (Show, Eq)

data ParsingException = ParsingException
  { sqlFragment :: Text,
    errorMsg :: String
  }
  deriving stock (Show)

instance Exception ParsingException

-- | This class indicates the ability to query environment variables.
-- It might seem overly polymorphic to have such a thing, but
-- `parseAndClassifyMigration` would be almost pure if not only for querying
-- env vars!
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
        ( \var ->
            lookupEnv (Text.unpack var)
              >>= \mVal -> pure (var, maybe "" Text.pack mVal)
        )
        vars

instance (MonadTrans t, Monad m, EnvVars m) => EnvVars (t m) where
  getEnvVars = lift . getEnvVars

{-# INLINE parseSqlPiecesStreaming #-} -- See Note [Inlining and specialization]
parseSqlPiecesStreaming ::
  (Monad m) =>
  Stream (Of Text) m () ->
  Stream (Of SqlPiece) m ()
parseSqlPiecesStreaming = parseSqlPiecesStreaming' sqlPieceParser

{-# INLINE parseSqlPiecesStreaming' #-} -- See Note [Inlining and specialization]
parseSqlPiecesStreaming' ::
  forall m.
  (Monad m) =>
  (ParserState -> Parser ([SqlPiece], ParserState)) ->
  Stream (Of Text) m () ->
  Stream (Of SqlPiece) m ()
parseSqlPiecesStreaming' parser contents = go $ Streaming.concat $ manyStreaming parser OutsideCopy contents
  where
    go :: Stream (Of SqlPiece) m (Stream (Of Text) m (), ParserState) -> Stream (Of SqlPiece) m ()
    go = \case
      S.Step (sqlPiece :> rest) -> S.Step $ sqlPiece :> go rest
      S.Return (unparsedTextStream, _) -> S.Effect $ do
        allRemainingText <- Streaming.mconcat_ unparsedTextStream
        -- If there is white-space at the end of the migration, it would've been parsed as WhiteSpacePiece. If there is valid SQL, then it would've been parsed as some other SQL piece. And so on.
        -- Thus, if we're here, the end of the migration is either empty text or something we failed to parse. It is almost certainly
        -- the former, but if it's the latter we return it all in a single OtherSqlPiece to try to be helpful.
        if allRemainingText == ""
          then pure $ S.Return ()
          else
            pure $ Streaming.yield $ OtherSqlPiece allRemainingText
      S.Effect eff -> S.Effect $ go <$> eff

{-# INLINE manyStreaming #-} -- See Note [Inlining and specialization]

-- | This should be equivalent to attoparsec's `many`, but with streams and a stateful parser. Naturally, there are differences in the type signature given the Streaming nature of this function.
-- It returns as the Stream's result the unparsed text.
manyStreaming ::
  forall m a s.
  (Monad m, Show a) =>
  (s -> Parser (a, s)) ->
  s ->
  -- | The input stream/text. Empty strings are ignored, and end-of-stream interpreted as an EOF marker when parsing.
  Stream (Of Text) m () ->
  -- | Returns a stream of parsed values with the stream's result being any unparsed text at the end.
  Stream (Of a) m (Stream (Of Text) m (), s)
manyStreaming parser initialState inputStream = go initialState DList.empty (Parsec.parse (parser initialState)) inputStreamWithoutEmptyStrs
  where
    -- End-Of-Stream is EOF for us, but attoparsec understands the empty string as EOF, so we filter it out because empty strings
    -- are perfectly valid chunks for streams and should have no impact on parsing
    inputStreamWithoutEmptyStrs = Streaming.filter ("" /=) inputStream
    go :: s -> DList.DList Text -> (Text -> Parsec.Result (a, s)) -> Stream (Of Text) m () -> Stream (Of a) m (Stream (Of Text) m (), s)
    go s !partiallyParsedTexts parseFunc stream =
      case stream of
        S.Step (textPiece :> rest) -> case parseFunc textPiece of
          Parsec.Fail {} -> S.Return (Streaming.each (DList.toList partiallyParsedTexts) <> Streaming.yield textPiece <> rest, s)
          Parsec.Done unconsumedInput (parsedValue, newParserState) -> S.Step $ parsedValue :> go newParserState DList.empty (Parsec.parse (parser newParserState)) (if unconsumedInput == "" then rest else S.Step $ unconsumedInput :> rest)
          Parsec.Partial continueParsing -> go s (partiallyParsedTexts `DList.snoc` textPiece) continueParsing rest
        S.Effect m -> S.Effect $ go s partiallyParsedTexts parseFunc <$> m
        S.Return () ->
          -- End of stream is EOF, which is represented by the empty string for attoparsec parsers
          case parseFunc "" of
            Parsec.Fail {} ->
              S.Return (Streaming.each $ DList.toList partiallyParsedTexts, s)
            Parsec.Done !unconsumedInput (!parsedValue, !newParserState) -> S.Step $ parsedValue :> S.Return (Streaming.yield unconsumedInput, newParserState)
            Parsec.Partial _ ->
              -- What is this case? Partial match on EOF? I suppose it is possible for an arbitrary parser to do this..
              S.Return (Streaming.each $ DList.toList partiallyParsedTexts, s)

parsedSqlText :: (Monad m) => ParsedSql m -> m Text
parsedSqlText (UnparsedSql t) = pure t
parsedSqlText (WellParsedSql s) =
  Streaming.fold_ (<>) "" id $ Streaming.map sqlPieceText s

sqlPieceText :: SqlPiece -> Text
sqlPieceText (CommentPiece s) = s
sqlPieceText (WhiteSpacePiece s) = s
sqlPieceText (BeginTransaction s) = s
sqlPieceText (RollbackTransaction s) = s
sqlPieceText (CommitTransaction s) = s
sqlPieceText (OtherSqlPiece s) = s
sqlPieceText (CopyFromStdinStatement s) = s
sqlPieceText (CopyFromStdinRows s) = s
sqlPieceText (CopyFromStdinEnd s) = s

mapSqlPiece :: (Text -> Text) -> SqlPiece -> SqlPiece
mapSqlPiece f = \case
  CommentPiece s -> CommentPiece (f s)
  WhiteSpacePiece s -> WhiteSpacePiece (f s)
  BeginTransaction s -> BeginTransaction (f s)
  RollbackTransaction s -> RollbackTransaction (f s)
  CommitTransaction s -> CommitTransaction (f s)
  OtherSqlPiece s -> OtherSqlPiece (f s)
  CopyFromStdinStatement s -> CopyFromStdinStatement (f s)
  CopyFromStdinRows s -> CopyFromStdinRows (f s)
  CopyFromStdinEnd s -> CopyFromStdinEnd (f s)

data ParserState = OutsideCopy | InsideCopy
  deriving stock (Eq, Show)

sqlPieceParser :: ParserState -> Parser ([SqlPiece], ParserState)
sqlPieceParser parserState = case parserState of
  OutsideCopy -> first (: []) <$> outsideCopyParser
  InsideCopy -> copyFromStdinAfterStatementParser 65536
  where
    outsideCopyParser =
      (,OutsideCopy)
        . CommentPiece
        <$> commentParser
        <|> (,OutsideCopy)
          . WhiteSpacePiece
          <$> takeWhile1
            ( \c -> Char.isSpace c || c == '\n' || c == '\r' || c == '\t'
            )
        <|> (,InsideCopy)
          <$> copyFromStdinStatementParser
        <|> (,OutsideCopy)
          . BeginTransaction
          <$> beginTransactionParser
        <|> (,OutsideCopy)
          . RollbackTransaction
          <$> rollbackTransactionParser
        <|> (,OutsideCopy)
          . CommitTransaction
          <$> commitTransactionParser
        <|> (,OutsideCopy)
          . OtherSqlPiece
          <$> anySqlPieceParser
    beginTransactionParser =
      spaceSeparatedTokensToParser
        [CITextToken "BEGIN", AllUntilEndOfStatement]
        <|> spaceSeparatedTokensToParser
          [ CITextToken "START",
            CITextToken "TRANSACTION",
            AllUntilEndOfStatement
          ]
    rollbackTransactionParser =
      spaceSeparatedTokensToParser
        [CITextToken "ROLLBACK", AllUntilEndOfStatement]
    commitTransactionParser =
      spaceSeparatedTokensToParser
        [CITextToken "COMMIT", AllUntilEndOfStatement]
    anySqlPieceParser = do
      arbText <- spaceSeparatedTokensToParser [AllUntilEndOfStatement]
      when (arbText == "") $ fail "Please report this as a bug in codd: trying to parse empty string as SQL piece"
      pure arbText

data SqlToken = CITextToken !Text | SqlIdentifier | CommaSeparatedIdentifiers | Optional ![SqlToken] | CustomParserToken (Parser Text) | AllUntilEndOfStatement

spaceSeparatedTokensToParser :: [SqlToken] -> Parser Text
spaceSeparatedTokensToParser allTokens = case allTokens of
  [] -> pure ""
  [token1] -> parseToken token1
  (token1 : tokens) -> do
    s1 <- parseToken token1
    spaces <- case (s1, token1) of
      ("", Optional _) -> pure ""
      _ -> commentOrSpaceParser True <|> pure ""

    others <- spaceSeparatedTokensToParser tokens
    pure $ s1 <> spaces <> others
  where
    parseToken = \case
      Optional t -> spaceSeparatedTokensToParser t <|> pure ""
      CITextToken t -> asciiCI t
      CustomParserToken p -> p
      SqlIdentifier -> objIdentifier
      CommaSeparatedIdentifiers -> listOfAtLeast1 [SqlIdentifier] ","
      AllUntilEndOfStatement -> do
        t1 <-
          takeWhile
            (\c -> not (isPossibleBlockStartingChar c) && c /= ';')
        mc <- peekChar
        case mc of
          Nothing -> pure t1
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
  firstEl <- spaceSeparatedTokensToParser elementTokens
  -- We use pure "" only to allow for a space before the first separator..
  otherEls <-
    Text.concat
      <$> many'
        ( spaceSeparatedTokensToParser $
            CustomParserToken (pure "")
              : CITextToken separator
              : elementTokens
        )
  pure $ firstEl <> otherEls

-- Urgh.. parsing statements precisely would benefit a lot from importing the lex parser
copyFromStdinStatementParser :: Parser SqlPiece
copyFromStdinStatementParser = do
  stmt <-
    spaceSeparatedTokensToParser
      [ CITextToken "COPY",
        SqlIdentifier,
        Optional
          [ CITextToken "(",
            Optional [CommaSeparatedIdentifiers],
            CITextToken ")"
          ],
        CITextToken "FROM",
        CITextToken "STDIN",
        AllUntilEndOfStatement
      ]
  seol <- eol
  pure $ CopyFromStdinStatement $ stmt <> seol

-- | Parser to be used after "COPY FROM STDIN..." has been parsed with `copyFromStdinStatementParser`.
copyFromStdinAfterStatementParser :: Int -> Parser ([SqlPiece], ParserState)
copyFromStdinAfterStatementParser approxMaxChunkSize = do
  when (approxMaxChunkSize <= 0) $
    error "approxMaxChunkSize must be strictly positive"
  -- This stateful parser is tricky to get right but it's proven to be much faster than simpler
  -- alternatives I've tried (e.g. taking lines and concatenating them was incredibly slow for some reason)
  (contents, (_, _, terminatorLen)) <-
    Parsec.runScanner
      (0 :: Int, 0 :: Int, 0 :: Int)
      ( \(lenTotalParsed, lenCurrentLine, lenTerminatorSoFar) c ->
          if lenTotalParsed >= approxMaxChunkSize && lenCurrentLine == 0
            then Nothing -- Only stop at the beginning of a new line
            else
              if lenCurrentLine /= lenTerminatorSoFar && c == '\n'
                then Just (1 + lenTotalParsed, 0, 0)
                else
                  if lenCurrentLine /= lenTerminatorSoFar
                    then Just (1 + lenTotalParsed, 1 + lenCurrentLine, 0)
                    else case (lenTerminatorSoFar, c) of
                      (0, '\\') ->
                        Just (1 + lenTotalParsed, 1 + lenCurrentLine, 1)
                      (1, '.') ->
                        Just (1 + lenTotalParsed, 1 + lenCurrentLine, 2)
                      (2, '\n') ->
                        Just (1 + lenTotalParsed, 1 + lenCurrentLine, 3) -- Last char in terminator, but `Just` because it needs to be in the parsed contents
                      (3, _) -> Nothing -- Terminator with len=3 means it's been parsed, so end here.
                      (_, '\n') -> Just (1 + lenTotalParsed, 0, 0)
                      _ ->
                        Just (1 + lenTotalParsed, 1 + lenCurrentLine, 0)
      )
  isEnd :: Bool <- Parsec.atEnd
  let fullTerminator = "\\.\n"
      eofTerminator = "\\."
      terminatorFound
        | terminatorLen == 3 = fullTerminator
        | isEnd && terminatorLen == 2 = eofTerminator
        | otherwise = ""
      rows = Text.dropEnd terminatorLen contents
  case (rows, terminatorFound) of
    ("", "") -> pure ([], InsideCopy) -- This should be impossible
    ("", _) -> pure ([CopyFromStdinEnd terminatorFound], OutsideCopy)
    (_, "") -> pure ([CopyFromStdinRows rows], InsideCopy)
    (_, _) ->
      pure
        ( [CopyFromStdinRows rows, CopyFromStdinEnd terminatorFound],
          OutsideCopy
        )

-- | Parses 0 or more consecutive white-space or comments
commentOrSpaceParser :: Bool -> Parser Text
commentOrSpaceParser atLeastOne =
  if atLeastOne
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
      t <- takeWhile (/= '$')
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
  rest <- Parsec.takeWhile (\c -> c /= '\n' && c /= '\r')
  end <- eol <|> "" <$ endOfInput
  pure $ begin <> rest <> end

cStyleComment :: Parser Text
cStyleComment = do
  openComment <- string "/*"
  rest <-
    Parsec.scan
      (1 :: Int, False, False)
      ( \(openCommentCount, hasPartialOpening, hasPartialClosing) c ->
          nothingWhenDone $ case (hasPartialOpening, hasPartialClosing, c) of
            -- Handle slashes in all possible contexts
            (False, False, '/') -> Just (openCommentCount, True, False)
            (False, True, '/') -> Just (openCommentCount - 1, False, False)
            (True, False, '/') -> Just (openCommentCount, True, False)
            -- Handle asterisks in all possible contexts
            (False, False, '*') -> Just (openCommentCount, False, True)
            (True, False, '*') -> Just (openCommentCount + 1, False, False)
            (False, True, '*') -> Just (openCommentCount, False, True)
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
    nothingWhenDone x = x

parenthesisedExpression :: Parser Text
parenthesisedExpression = do
  openParen <- string "(" <|> fail "No open paren"
  rest <- insideParenParser
  pure $ openParen <> rest
  where
    insideParenParser :: Parser Text
    insideParenParser = do
      more <-
        takeWhile
          (\c -> not (isPossibleBlockStartingChar c) && c /= ')')
      nextChar <- peekChar
      case nextChar of
        Nothing -> pure more -- Be gentle with EOF
        Just ')' -> do
          closeParen <- string ")"
          pure $ more <> closeParen
        Just _ -> do
          blockOrOtherwise <- blockParser <|> Parsec.take 1
          rest <- insideParenParser -- We're still inside an openParen after parsing a block or a character
          pure $ more <> blockOrOtherwise <> rest

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Does not consume the ending char and RETURNS any
-- backslash escape chars in the result.
-- Use `parseWithEscapeCharProper` to exclude the escape chars from the result.
-- This function is useful if you want the original parsed contents.
parseWithEscapeCharPreserve :: (Char -> Bool) -> Parser Text
parseWithEscapeCharPreserve untilc = do
  cs <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
  nextChar <- peekChar
  case nextChar of
    Just '\\' -> do
      c <- Parsec.take 2
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
            ( \c ->
                not (Char.isSpace c)
                  && c
                    /= ','
                  && c
                    /= '.'
                  && c
                    /= ')'
            ) -- TODO: What are the valid chars for identifiers?? Figure it out!!
   in listOfAtLeast1 [CustomParserToken singleIdentifier] "."

doubleQuotedIdentifier :: Parser Text
doubleQuotedIdentifier = do
  openingQuote <- string "\""
  rest <- parseWithEscapeCharPreserve (== '"')
  ending <- string "\""
  pure $ openingQuote <> rest <> ending

-- | Parses a single quoted NON standard conforming string, i.e. strings that use backslash as an escape character, and are marked
-- by beginning with an `E`. Consecutive simple quotes are also treated as a single quote, just like in std conforming strings.
-- See https://www.postgresql.org/docs/current/sql-syntax-lexical.html
cStyleEscapedString :: Parser Text
cStyleEscapedString = do
  openingChars <- string "E'"
  rest <-
    Parsec.scan
      (False, False)
      ( \(lastCharWasBackslash, lastCharWasSingleQuote) c ->
          case ((lastCharWasBackslash, lastCharWasSingleQuote), c) of
            ((False, False), '\'') -> Just (False, True)
            ((False, True), '\'') -> Just (False, False) -- Two consecutive single quotes are not end of string
            ((False, True), _) -> Nothing -- A single quote not preceded by \ and followed by not-a-single-quote is end of string
            ((False, False), '\\') -> Just (True, False)
            ((False, False), _) -> Just (False, False) -- "regular" character
            ((True, False), _) -> Just (False, False) -- Any character after a backslash must be included in the string
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
  rest <-
    Parsec.scan
      False
      ( \lastCharWasQuote c -> case (lastCharWasQuote, c) of
          (False, '\'') -> Just True
          (True, '\'') -> Just False -- Two consecutive single quotes represent a single quote, not end of string
          (True, _) -> Nothing -- One single quote followed by any other character means that single quote was end of string
          (False, _) -> Just False
      )
  pure $ openingQuote <> rest

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Stops at and does not consume the first predicate-passing
-- char, and does not include escape chars in the returned value,
-- as one would expect.
parseWithEscapeCharProper :: (Char -> Bool) -> Parser Text
parseWithEscapeCharProper untilc = do
  cs <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
  nextChar <- peekChar
  case nextChar of
    Nothing -> pure cs
    Just '\\' -> do
      void $ char '\\'
      c <- Parsec.take 1
      rest <- parseWithEscapeCharProper untilc
      pure $ cs <> c <> rest
    Just _ -> pure cs

eitherToMay :: Either a b -> Maybe b
eitherToMay (Left _) = Nothing
eitherToMay (Right v) = Just v

-- | Parses a URI with scheme 'postgres' or 'postgresql', as per https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING.
-- The difference here is that URIs with a query string or with a fragment are not allowed.
uriConnParser :: Text -> Either String ConnectionString
uriConnParser line = runIdentity $ runExceptT @String @_ @ConnectionString $ do
  case parseURI (Text.unpack line) of
    Nothing -> throwE "Connection string is not a URI"
    Just URI {..} -> do
      unless
        (Text.toLower (Text.pack uriScheme) `elem` ["postgres:", "postgresql:"])
        $ throwE
          "Connection string's URI scheme must be 'postgres' or 'postgresql'"
      case uriAuthority of
        Nothing ->
          throwE
            "Connection string must contain at least user and host"
        Just URIAuth {..} -> do
          let database =
                unEscapeString $ trimFirst '/' uriPath
              hasQueryString = not $ null uriQuery
              hasFragment = not $ null uriFragment
          when (null database) $
            throwE
              "Connection string must contain a database name"
          when (hasQueryString || hasFragment) $
            throwE
              "Custom parameters are not supported in connection strings. Make sure your connection URI does not have a query string or query fragment"

          -- Ports are not mandatory and are defaulted to 5432 when not present
          let port = if null uriPort then Just 5432 else Nothing
          case port
            <|> eitherToMay
              ( parseOnly
                  (Parsec.decimal <* endOfInput)
                  (Text.pack $ trimFirst ':' uriPort)
              ) of
            Nothing ->
              throwE "Invalid port in connection string"
            Just parsedPort -> do
              let (unEscapeString . trimLast '@' -> user, unEscapeString . trimLast '@' . trimFirst ':' -> password) =
                    break (== ':') uriUserInfo
              pure
                ConnectionString
                  { hostname =
                      unEscapeString $
                        unescapeIPv6 uriRegName,
                    port = parsedPort,
                    user,
                    password,
                    database,
                    options = Nothing
                  }
  where
    unescapeIPv6 :: String -> String
    unescapeIPv6 = trimFirst '[' . trimLast ']'

    trimFirst :: Char -> String -> String
    trimFirst c s@(c1 : cs) = if c == c1 then cs else s
    trimFirst _ s = s

    trimLast :: Char -> String -> String
    trimLast c s = case Text.unsnoc $ Text.pack s of
      Nothing -> s
      Just (t, lastChar) -> if lastChar == c then Text.unpack t else s

keywordValueConnParser :: Text -> Either String ConnectionString
keywordValueConnParser line = runIdentity $ runExceptT $ do
  kvs <-
    sortOn fst
      <$> parseOrFail
        (singleKeyVal `Parsec.sepBy` takeWhile1 Char.isSpace)
        (Text.strip line)
        "Invalid connection string"
  ConnectionString
    <$> getVal "host" Nothing txtToString kvs
    <*> getVal "port" (Just 5432) Parsec.decimal kvs
    <*> getVal "user" Nothing txtToString kvs
    <*> getVal "password" (Just "") txtToString kvs
    <*> getVal "dbname" Nothing txtToString kvs
    <*> pure Nothing
  where
    getVal key def parser pairs =
      case (map snd $ filter ((== key) . fst) pairs, def) of
        ([], Nothing) ->
          throwE $
            "Connection string must contain a value for '"
              <> Text.unpack key
              <> "'"
        ([], Just v) -> pure v
        ([vt], _) ->
          parseOrFail parser vt $
            "Connection string key '"
              <> Text.unpack key
              <> "' is in an unrecognizable format"
        _ ->
          throwE $
            "Duplicate key '"
              <> Text.unpack key
              <> "' found in connection string."

    txtToString = Text.unpack <$> Parsec.takeText
    parseOrFail parser txt errorMsg =
      case parseOnly (parser <* endOfInput) txt of
        Left _ -> throwE errorMsg
        Right v -> pure v

    singleKeyVal = do
      key <- takeWhile1 (\c -> not (Char.isSpace c) && c /= '=')
      skipAllWhiteSpace
      void $ char '='
      skipAllWhiteSpace
      value <-
        takeQuotedString
          <|> parseWithEscapeCharProper
            (\c -> Char.isSpace c || c == '\'' || c == '\\')
          <|> pure ""
      pure (key, value)

    takeQuotedString = do
      void $ char '\''
      s <- parseWithEscapeCharProper (== '\'')
      void $ char '\''
      pure s

-- | Parses a string in one of libpq allowed formats. See https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING.
-- The difference here is that only a subset of all connection parameters are allowed.
-- I wish this function existed in postgresql-simple or some form of it in postgresql-libpq, but if it does I couldn't find it.
connStringParser :: Parser ConnectionString
connStringParser = do
  connStr <-
    Parsec.takeWhile1 (const True)
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
      fail $
        "Connection string is not a valid libpq connection string. A valid libpq connection string is either in the format 'postgres://username[:password]@host:port/database_name', with URI-encoded (percent-encoded) components except for the host and bracket-surround IPv6 addresses, or in the keyword value pairs format, e.g. 'dbname=database_name host=localhost user=postgres' with escaping for spaces, quotes or empty values. More info at https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING. Specific error: "
          <> err
    Right c -> pure c

optionParser :: Parser SectionOption
optionParser = do
  skipJustSpace
  x <-
    inTxn
      <|> noTxn
      <|> noParse
      <|> requiresCoddSchema
      <|> fail
        "Valid options after '-- codd:' are 'in-txn', 'no-txn', 'requires-codd-schema' or 'no-parse' (the last implies in-txn)"
  skipJustSpace
  return x
  where
    -- TODO: Parser for "require-codd-schema"
    requiresCoddSchema = string "requires-codd-schema" >> pure OptRequiresCoddSchema
    inTxn = string "in-txn" >> pure OptInTxn
    noTxn = string "no-txn" >> pure OptNoTxn
    noParse = string "no-parse" >> pure OptNoParse

-- | Parser that consumes only the space character, not other kinds of white space.
skipJustSpace :: Parser ()
skipJustSpace = skipWhile (== ' ')

-- | Parser that consumes any kind of Unicode space character, including \t, \n, \r, \f, \v.
skipAllWhiteSpace :: Parser ()
skipAllWhiteSpace = skipWhile Char.isSpace

data CoddCommentParseResult a = CoddCommentWithGibberish !Text | CoddCommentSuccess !a
  deriving stock (Eq, Show)

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
      . Text.stripEnd
      <$> Parsec.takeText
  where
    parseOptions = do
      opts <-
        optionParser `sepBy1` (skipJustSpace >> char ',' >> skipJustSpace)
      endOfLine <|> endOfInput
      pure $ CoddCommentSuccess opts

-- | Parses a "-- codd-connection: some-connection-string(newline|EOF)" line.
coddConnStringCommentParser :: Parser (CoddCommentParseResult ConnectionString)
coddConnStringCommentParser = do
  void $ string "--"
  skipJustSpace
  void $ string "codd-connection:"
  skipJustSpace
  -- Take all text and remove the ending newline char if there is one (there needs to be a newline or EOF after a single-line comment)
  connStringText <- Text.stripEnd <$> Parsec.takeText
  case parseOnly (connStringParser <* endOfInput) connStringText of
    Left _ -> pure $ CoddCommentWithGibberish connStringText
    Right cinfo -> pure $ CoddCommentSuccess cinfo

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
      . Text.stripEnd
      <$> Parsec.takeText
  where
    parseVarNames = do
      vars <-
        singleVarNameParser
          `sepBy1` (skipJustSpace >> char ',' >> skipJustSpace)
      endOfLine <|> endOfInput
      pure $ CoddCommentSuccess vars
    singleVarNameParser =
      Parsec.takeWhile1
        ( \c ->
            Char.isAsciiLower c
              || Char.isAsciiUpper c
              || c
                == '_'
              || Char.isDigit c
        )

isWhiteSpacePiece :: SqlPiece -> Bool
isWhiteSpacePiece (WhiteSpacePiece _) = True
isWhiteSpacePiece _ = False

isCommentPiece :: SqlPiece -> Bool
isCommentPiece (CommentPiece _) = True
isCommentPiece _ = False

isTransactionEndingPiece :: SqlPiece -> Bool
isTransactionEndingPiece (RollbackTransaction _) = True
isTransactionEndingPiece (CommitTransaction _) = True
isTransactionEndingPiece _ = False

-- | Replaces arbitrary strings with env vars by direct text substition of ${ENVVARNAME}
textReplaceEnvVars :: Map Text Text -> Text -> Text
textReplaceEnvVars =
  Map.foldlWithKey'
    (\accF var val -> Text.replace ("${" <> var <> "}") val . accF)
    id

substituteEnvVarsInSqlPiecesStream ::
  (Monad m) =>
  -- | Names and values of env vars to query from environment and substitute
  Map Text Text ->
  Stream (Of SqlPiece) m r ->
  Stream (Of SqlPiece) m r
substituteEnvVarsInSqlPiecesStream envVars s =
  let subsfunc = textReplaceEnvVars envVars
   in Streaming.map (mapSqlPiece subsfunc) s

{-# INLINE parseAndClassifyMigration #-} -- See Note [Inlining and specialization]

-- | Parses only comments and white-space that precede the first SQL statement and
-- extracts from them custom options and a custom connection string when
-- they exist, or returns a good error message otherwise.
parseAndClassifyMigration ::
  (Monad m, MigrationStream m s, EnvVars m) =>
  s ->
  m
    ( Either
        String
        ( Map Text Text,
          [SectionOption],
          Maybe ConnectionString,
          ParsedSql m
        )
    )
parseAndClassifyMigration sqlStream = do
  -- We parse the contents of the migration until we have read all leading white-space and comments
  -- so that we can find special `-- codd` directives and parse them.
  leadingWhiteSpaceAndComments :> restOfMigration <-
    Streaming.toList $
      Streaming.span (\p -> isCommentPiece p || isWhiteSpacePiece p) $
        parseSqlPiecesStreaming $
          migStream sqlStream
  let firstComments = filter isCommentPiece leadingWhiteSpaceAndComments
      envVarParseResults =
        mapMaybe
          ( ( \case
                Left _ -> Nothing
                Right parseRes -> Just parseRes
            )
              . ( parseOnly (coddEnvVarsCommentParser <* endOfInput)
                    . sqlPieceText
                )
          )
          firstComments
      malformedEnvVarComment =
        listToMaybe
          [line | CoddCommentWithGibberish line <- envVarParseResults]
  case malformedEnvVarComment of
    Just badLine ->
      pure $
        Left $
          "Malformed -- codd-env-vars comment. Make sure there is at least one environment variable, that they are separated by a comma and that they only contain letters, numbers and underscore. Original comment: "
            <> Text.unpack badLine
    Nothing -> do
      let envVarNames =
            mconcat
              [vars | CoddCommentSuccess vars <- envVarParseResults]
      envVars <- getEnvVars envVarNames
      let allOptSections =
            mapMaybe
              ( ( \case
                    Left _ -> Nothing
                    Right opts -> Just opts
                )
                  . ( parseOnly (coddCommentParser <* endOfInput)
                        . textReplaceEnvVars envVars
                        . sqlPieceText
                    )
              )
              firstComments
          customConnString =
            mapMaybe
              ( ( \case
                    Left _ -> Nothing
                    Right connInfo -> Just connInfo
                )
                  . ( parseOnly (coddConnStringCommentParser <* endOfInput)
                        . textReplaceEnvVars envVars
                        . sqlPieceText
                    )
              )
              firstComments
          headMay [] = Nothing
          headMay (x : _) = Just x

      -- Urgh.. is there no better way of pattern matching the stuff below?
      let (badOptSections, mconcat -> goodOptSections) =
            partitionEithers $
              map
                ( \case
                    CoddCommentWithGibberish badOpts -> Left badOpts
                    CoddCommentSuccess goodOpts -> Right goodOpts
                )
                allOptSections
          (badCustomConnStrings, goodCustomConnStrings) =
            partitionEithers $
              map
                ( \case
                    CoddCommentWithGibberish badConn -> Left badConn
                    CoddCommentSuccess goodConn -> Right goodConn
                )
                customConnString
          eExtr = case (badOptSections, badCustomConnStrings) of
            (badOptions : _, _) ->
              Left $
                "The options '"
                  <> Text.unpack badOptions
                  <> "' are invalid. Valid options after '-- codd:' are 'in-txn', 'no-txn', 'requires-codd-schema' or 'no-parse' (the last implies in-txn)"
            (_, _badConn : _) ->
              Left
                "Connection string is not a valid libpq connection string. A valid libpq connection string is either in the format 'postgres://username[:password]@host:port/database_name', with URI-encoded (percent-encoded) components except for the host and bracket-surround IPv6 addresses, or in the keyword value pairs format, e.g. 'dbname=database_name host=localhost user=postgres' with escaping for spaces, quotes or empty values. More info at https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING"
            ([], []) ->
              if length goodCustomConnStrings > 1
                then
                  Left
                    "There must be at most one '-- codd-connection:' comment in the first lines of a migration"
                else
                  if OptNoParse `elem` goodOptSections && OptNoTxn `elem` goodOptSections
                    then Left "It is not possible to set both 'no-txn' and 'no-parse', because the latter implies the entire migration will be applied as a single SQL statement and thus in its own implicit transaction"
                    else Right (goodOptSections, headMay goodCustomConnStrings)

      case eExtr of
        Left err -> pure $ Left err
        Right (opts, customConnStr)
          | -- Detect "-- codd: no-parse"
            OptNoParse `elem` opts -> do
              -- Remember: the original Stream (Of Text) has already been advanced
              -- beyond its first SQL statement, but **only** if this is an IO-based
              -- Stream. If this is a pure Stream it will start from the beginning when
              -- consumed. That is why we prefer to use a special action to retrieve
              -- the full migration's contents.
              fullMigContents <- readFullContents sqlStream
              pure $
                Right
                  ( envVars,
                    opts,
                    customConnStr,
                    UnparsedSql fullMigContents
                  )
          | otherwise ->
              pure $
                Right
                  ( envVars,
                    opts,
                    customConnStr,
                    -- Prepend leadingWhiteSpaceAndComments to preserve the property that SqlMigration objects
                    -- will hold the full original SQL.
                    WellParsedSql $
                      substituteEnvVarsInSqlPiecesStream envVars $
                        Streaming.each leadingWhiteSpaceAndComments
                          <> restOfMigration
                  )

piecesToText :: (Foldable t) => t SqlPiece -> Text
piecesToText = foldr ((<>) . sqlPieceText) ""

{-# INLINE parseSqlMigration #-} -- See Note [Inlining and specialization]
parseSqlMigration ::
  forall m s.
  (Monad m, MigrationStream m s, EnvVars m) =>
  String ->
  s ->
  m (Either String (SqlMigration m))
parseSqlMigration name s = (toMig =<<) <$> parseAndClassifyMigration s
  where
    dupOpts opts = length (nub opts) < length opts
    checkOpts :: [SectionOption] -> Maybe String
    checkOpts opts
      | inTxn opts && noTxn opts =
          Just
            "Choose either 'in-txn' or 'no-txn', but not both. If you don't specify anything the default is 'in-txn'"
      | dupOpts opts =
          Just "Some options are duplicated"
      | otherwise =
          Nothing
    inTxn opts = OptNoTxn `notElem` opts || OptInTxn `elem` opts
    noTxn opts = OptNoTxn `elem` opts

    mkMig ::
      (Map Text Text, [SectionOption], Maybe ConnectionString, ParsedSql m) ->
      SqlMigration m
    mkMig (migrationEnvVars, opts, customConnString, sql) =
      SqlMigration
        { migrationName = name,
          migrationSql = sql,
          migrationInTxn = inTxn opts,
          migrationRequiresCoddSchema = OptRequiresCoddSchema `elem` opts,
          migrationCustomConnInfo = customConnString,
          migrationEnvVars
        }

    toMig ::
      (Map Text Text, [SectionOption], Maybe ConnectionString, ParsedSql m) ->
      Either String (SqlMigration m)
    toMig x@(_, ops, _, _) = case checkOpts ops of
      Just err -> Left err
      _ -> Right $ mkMig x

parseAddedSqlMigration ::
  (Monad m, MigrationStream m s, EnvVars m) =>
  -- | Can be a pure file name or absolute path to file name of an already timestamp migration file
  String ->
  s ->
  m (Either String (AddedSqlMigration m))
parseAddedSqlMigration (takeFileName -> name) s = do
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
parseMigrationTimestamp name =
  parseOnly
    ( migTimestampParser
        <|> fail
          ("Could not find migration timestamp from its name: '" <> name <> "'")
    )
    (Text.pack name)
  where
    dash = void $ char '-'
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
          pure $
            DB.Finite $
              UTCTime day $
                secondsToDiffTime $
                  hh
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
