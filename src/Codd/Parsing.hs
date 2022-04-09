module Codd.Parsing
  ( SqlMigration(..)
  , AddedSqlMigration(..)
  , CoddCommentParseResult(..)
  , SqlPiece(..)
  , ParsedSql(..)
  , connStringParser
  , hoistAddedSqlMigration
  , isCommentPiece
  , isTransactionEndingPiece
  , isWhiteSpacePiece
  , mapSqlMigration
  , piecesToText
  , sqlPieceText
  , parsedSqlText
  , parseSqlMigration
  , parseWithEscapeCharProper
  , parseAddedSqlMigration
  , parseAndClassifyMigration
  , parseMigrationTimestamp
  , parseSqlPiecesStreaming
  , toMigrationTimestamp
  ) where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( guard
                                                , void
                                                , when
                                                )
import           Control.Monad.Trans.Resource   ( MonadThrow(throwM) )
import           Data.Attoparsec.Text           ( Parser
                                                , anyChar
                                                , asciiCI
                                                , atEnd
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
import qualified Data.Char                     as Char
import           Data.List                      ( nub )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( fromGregorianValid
                                                , secondsToDiffTime
                                                )
import           Data.Time.Clock                ( UTCTime(..) )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import           Prelude                 hiding ( takeWhile )
import           Streaming                      ( MFunctor(hoist)
                                                , Of(..)
                                                )
import qualified Streaming.Prelude             as Streaming
import           Streaming.Prelude              ( Stream )
import           UnliftIO.Exception             ( Exception )


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
  }

data AddedSqlMigration m = AddedSqlMigration
  { addedSqlMig       :: SqlMigration m
  , addedSqlTimestamp :: DB.UTCTimestamp
  }

-- TODO: This should probably not be in Parsing.hs
hoistAddedSqlMigration
  :: (Monad m, Monad n)
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

mapSqlMigration
  :: (SqlMigration m -> SqlMigration m)
  -> AddedSqlMigration m
  -> AddedSqlMigration m
mapSqlMigration f (AddedSqlMigration sqlMig tst) =
  AddedSqlMigration (f sqlMig) tst

data SectionOption = OptInTxn Bool | OptNoParse Bool
  deriving stock (Eq, Show)

data SqlPiece = CommentPiece !Text | WhiteSpacePiece !Text | CopyFromStdinStatement !Text | CopyFromStdinRow !Text | CopyFromStdinEnd !Text | BeginTransaction !Text | RollbackTransaction !Text | CommitTransaction !Text | OtherSqlPiece !Text
  deriving stock (Show, Eq)

data ParsingException = ParsingException
  { sqlFragment :: Text
  , errorMsg    :: String
  }
  deriving stock Show

instance Exception ParsingException

-- | This should be a rough equivalent to `many parseSqlPiece` for Streams.
parseSqlPiecesStreaming
  :: forall m
   . MonadThrow m
  => Stream (Of Text) m ()
  -> Stream (Of SqlPiece) m ()
parseSqlPiecesStreaming contents = Streaming.concat parseResultsStream
 where
  mkErrorMsg errorMsg =
    "An internal parsing error occurred. This is most likely a bug in codd. Please file a bug report.\
    \ In the meantime, you can add this migration by writing \"-- codd: no-parse\" as its very first line.\
    \ The downside is this migration will be in-txn and COPY will not be supported. It will also be put in\
    \ memory all at once instead of being read and applied in streaming fashion.\
    \ The more detailed parsing error is: "
      ++ errorMsg
    -- Important question: will the unconsumed Text remainders in the parsing Results
    -- be allocated and take a lot of memory? This probably depends on how lazily these are
    -- allocated by attoparsec and by Text's internal implementation. We might want to test this
    -- with very small chunks of Text coming from the input stream to compare.
    -- NOTE: It's unlikely Text's internal implementation
    -- copies byte arrays for new instances. It most likely references existing byte arrays
    -- with some offset and length integers to follow. Double-check just in case.
  parseOneInc
    :: ParserState
    -> Text
    -> m
         ( [SqlPiece]
         , Maybe (Text -> Parsec.Result (SqlPiece, ParserState))
         , ParserState
         )
  parseOneInc parserState t = if t == ""
    then pure ([], Nothing, parserState)
    else case Parsec.parse (sqlPieceParser parserState) t of
      Parsec.Fail _ _ errorMsg ->
        throwM $ ParsingException t $ mkErrorMsg errorMsg
      Parsec.Done unconsumedInput (sqlPiece, newParserState) ->
        first3 (sqlPiece :) <$> parseOneInc newParserState unconsumedInput
      Parsec.Partial continue -> pure ([], Just continue, parserState)

  parseResultsStream :: Stream (Of [SqlPiece]) m ()
  parseResultsStream =
    Streaming.scanM
        (\(_, !mContinue, !parserState) !textPiece -> case mContinue of
          Nothing       -> parseOneInc parserState textPiece
           -- TODO: The following case is almost essentially "parseOneInc".
           -- Reuse more code!
          Just continue -> case continue textPiece of
            Parsec.Fail _ _ errorMsg ->
              throwM $ ParsingException textPiece $ mkErrorMsg errorMsg
            Parsec.Done unconsumedInput (sqlPiece, newParserState) ->
              first3 (sqlPiece :) <$> parseOneInc newParserState unconsumedInput
            Parsec.Partial nextContinue ->
              pure ([], Just nextContinue, parserState)
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
sqlPieceText (CopyFromStdinRow       s) = s
sqlPieceText (CopyFromStdinEnd       s) = s

data ParserState = OutsideCopy | InsideCopy

sqlPieceParser :: ParserState -> Parser (SqlPiece, ParserState)
sqlPieceParser parserState = case parserState of
  OutsideCopy -> outsideCopyParser
  InsideCopy  -> copyFromStdinAfterStatementParser
 where
  outsideCopyParser =
    (, OutsideCopy)
      .   CommentPiece
      <$> commentParser
      <|> (, OutsideCopy)
      .   WhiteSpacePiece
      <$> takeWhile1 Char.isSpace
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
    spaceSeparatedTokensToParser [CITextToken "BEGIN", AllUntilEndOfStatement]
      <|> spaceSeparatedTokensToParser
            [ CITextToken "START"
            , CITextToken "TRANSACTION"
            , AllUntilEndOfStatement
            ]
  rollbackTransactionParser = spaceSeparatedTokensToParser
    [CITextToken "ROLLBACK", AllUntilEndOfStatement]
  commitTransactionParser =
    spaceSeparatedTokensToParser [CITextToken "COMMIT", AllUntilEndOfStatement]
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
    Optional          t -> spaceSeparatedTokensToParser t <|> pure ""
    CITextToken       t -> asciiCI t
    CustomParserToken p -> p
    SqlIdentifier ->
      let -- Identifiers can be in fully qualified form "database"."schema"."objectname",
          -- with and without double quoting, e.g.: "schema".tablename
          singleIdentifier =
            blockParserOfType (== DoubleQuotedIdentifier) <|> takeWhile1
              (\c -> not (Char.isSpace c) && c /= ',' && c /= '.' && c /= ')') -- TODO: What are the valid chars for identifiers?? Figure it out!!
      in  listOfAtLeast1 [CustomParserToken singleIdentifier] "."
    CommaSeparatedIdentifiers -> listOfAtLeast1 [SqlIdentifier] ","
    AllUntilEndOfStatement    -> do
      t1 <- takeWhile (\c -> not (isPossibleStartingChar c) && c /= ';')
      mc <- peekChar
      case mc of
        Nothing  -> pure t1
        Just ';' -> do
          void $ char ';'
          pure $ t1 <> ";"
        Just _ -> do
          t2 <- Text.concat . map snd <$> many1 blockParser <|> Parsec.take 1
          -- After reading blocks or just a char, we still need to find a semi-colon to get a statement from start to finish!
          t3 <- parseToken AllUntilEndOfStatement
          pure $ t1 <> t2 <> t3

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
      [CITextToken "(", Optional [CommaSeparatedIdentifiers], CITextToken ")"]
    , CITextToken "FROM"
    , CITextToken "STDIN"
    , AllUntilEndOfStatement
    ]
  seol <- eol
  pure $ CopyFromStdinStatement $ stmt <> seol

-- | Parser to be used after "COPY FROM STDIN..." has been parsed with `copyFromStdinStatementParser`.
copyFromStdinAfterStatementParser :: Parser (SqlPiece, ParserState)
copyFromStdinAfterStatementParser =
  -- The first alternative handles a special case: COPY without data (very common in DB dumps)
  (, OutsideCopy)
    .   CopyFromStdinEnd
    <$> terminatorOnly
    <|> (   (, InsideCopy)
        .   CopyFromStdinRow
        <$> ((<>) <$> parseWithEscapeCharPreserve (== '\n') <*> string "\n")
        )

 where
  terminatorOnly = do
    s    <- string "\\."
    eol2 <- eol <|> ("" <$ endOfInput)
    pure $ s <> eol2

-- | Parses 0 or more consecutive white-space or comments
commentOrSpaceParser :: Bool -> Parser Text
commentOrSpaceParser atLeastOne = if atLeastOne
  then Text.concat <$> many1 (commentParser <|> takeWhile1 Char.isSpace)
  else Text.concat <$> many' (commentParser <|> takeWhile1 Char.isSpace)

commentParser :: Parser Text
commentParser = do
  (commentType, commentInit) <-
    (DoubleDashComment, ) <$> string "--" <|> (CStyleComment, ) <$> string "/*"
  bRemaining <- blockInnerContentsParser commentType
  pure $ commentInit <> bRemaining

data BlockType = DoubleDashComment | CStyleComment | DollarQuotedBlock !Text | DoubleQuotedIdentifier | SingleQuotedString deriving stock (Show, Eq)

isPossibleStartingChar :: Char -> Bool
isPossibleStartingChar c =
  c == '-' || c == '/' || c == '"' || c == '$' || c == '\''

isPossibleEndingChar :: BlockType -> Char -> Bool
isPossibleEndingChar DoubleDashComment      c = c == '\n'
isPossibleEndingChar CStyleComment          c = c == '*'
isPossibleEndingChar (DollarQuotedBlock _)  c = c == '$'
isPossibleEndingChar DoubleQuotedIdentifier c = c == '"'
isPossibleEndingChar SingleQuotedString     c = c == '\''

blockBeginParser :: Parser (BlockType, Text)
blockBeginParser =
  (DoubleDashComment, )
    <$> string "--"
    <|> (CStyleComment, )
    <$> string "/*"
    <|> dollarBlockParser
    <|> (DoubleQuotedIdentifier, )
    <$> string "\""
    <|> (SingleQuotedString, )
    <$> string "'"
 where
  dollarBlockParser = do
    void $ char '$'
    b <- takeWhile (/= '$')
    void $ char '$'
    let tb = "$" <> b <> "$"
    pure (DollarQuotedBlock tb, tb)

blockEndingParser :: BlockType -> Parser Text
blockEndingParser = \case
  DoubleDashComment      -> eol <|> (pure "" <* endOfInput)
  CStyleComment          -> string "*/"
  DollarQuotedBlock q    -> asciiCI q -- TODO: will asciiCI break for invalid ascii chars?
  DoubleQuotedIdentifier -> string "\""
  SingleQuotedString     -> string "'"

eol :: Parser Text
eol = string "\n" <|> string "\t\n"

blockParser :: Parser (BlockType, Text)
blockParser = do
  (bt, bBegin) <- blockBeginParser
  bRemaining   <- blockInnerContentsParser bt
  pure (bt, bBegin <> bRemaining)

blockParserOfType :: (BlockType -> Bool) -> Parser Text
blockParserOfType p = do
  (bt, c) <- blockParser
  guard $ p bt
  pure c

blockInnerContentsParser :: BlockType -> Parser Text
blockInnerContentsParser bt = do
  t <- case bt of
    SingleQuotedString     -> parseWithEscapeCharPreserve (== '\'') -- '' escaping is not to be explicitly implemented, but this parser understands it as two consecutive strings, and that's good enough for now
    DoubleQuotedIdentifier -> parseWithEscapeCharPreserve (== '"') -- "" escaping seems to be the same as above..
    _                      -> takeWhile (not . isPossibleEndingChar bt)
  done <- atEnd
  if done
    then pure t
    else do
      mEndingQuote <- Just <$> blockEndingParser bt <|> pure Nothing
      case mEndingQuote of
        Nothing -> do
          -- Could mean we found e.g. '*' inside a C-Style comment block, but not followed by '/'
          c      <- anyChar
          remain <- blockInnerContentsParser bt
          pure $ Text.snoc t c <> remain
        Just endingQuote -> pure $ t <> endingQuote

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Does not consume the ending char and RETURNS any
-- backslash escape chars in the result.
-- Use parseWithEscapeCharProper to exclude the escape chars from the result.
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


-- | Parses a string in the format postgres://username[:password]@host:port/database_name
connStringParser :: Parser ConnectInfo
connStringParser = do
  void $ string "postgres://"
  usr <- idParser "username"
  pwd <- (char ':' *> idParser "password") <|> pure ""
  void $ char '@'
  host <- ipv6Parser <|> idParser "host" <|> fail
    "Failed parsing host in the connection string"
  void $ char ':' <|> fail "Missing colon after host"
  port <- Parsec.decimal
    <|> fail "Could not find a port in the connection string."
  void $ char '/'
  adminDb <- idParser "database"
  pure ConnectInfo { connectHost     = host
                   , connectPort     = port
                   , connectUser     = usr
                   , connectPassword = pwd
                   , connectDatabase = adminDb
                   }
 where
  idParser :: String -> Parser String
  idParser idName = do
    x <- Text.unpack
      <$> parseWithEscapeCharProper (\c -> c == ':' || c == '@' || c == '\n')
    when (x == "")
      $  fail
      $  "Could not find a "
      <> idName
      <> " in the connection string."
    pure x

  ipv6Parser :: Parser String
  ipv6Parser = do
    void $ char '['
    x <- Text.unpack <$> parseWithEscapeCharProper (== ']')
    void $ char ']'
    pure x


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
  parseOptions <|> CoddCommentWithGibberish . Text.stripEnd <$> Parsec.takeText
 where
  parseOptions = do
    opts <- optionParser `sepBy1` (skipJustSpace >> char ',' >> skipJustSpace)
    endOfLine
    pure $ CoddCommentSuccess opts

-- | Parses a "-- codd-connection: postgres://...." line. Consumes all available input
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
    endOfLine
    pure $ CoddCommentSuccess connInfo

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

-- | Parses only comments and white-space that precede the first SQL statement and
-- extracts from them custom options and a custom connection string when
-- they exist, or returns a good error message otherwise.
parseAndClassifyMigration
  :: MonadThrow m
  => Stream (Of Text) m ()
  -> m (Either String ([SectionOption], Maybe ConnectInfo, ParsedSql m))
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
    $ parseSqlPiecesStreaming sqlStream
  let firstComments  = filter isCommentPiece consumedPieces
      allOptSections = mapMaybe
        ( (\case
            Left  _    -> Nothing
            Right opts -> Just opts
          )
        . (parseOnly (coddCommentParser <* endOfInput) . sqlPieceText)
        )
        firstComments
      customConnString = mapMaybe
        ( (\case
            Left  _        -> Nothing
            Right connInfo -> Just connInfo
          )
        . ( parseOnly (coddConnStringCommentParser <* endOfInput)
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
      (_, Just (CoddCommentWithGibberish badConn)) ->
        pure
          $  Left
          $  "The connection string '"
          <> Text.unpack badConn
          <> "' is invalid. A valid connection string is in the format 'postgres://username[:password]@host:port/database_name', with backslash to escape '@' and ':', and IPv6 addresses in brackets (no need to escape colons for those)"
      (Just (CoddCommentSuccess opts), Just (CoddCommentSuccess conn)) ->
        pure $ Right (opts, Just conn)
      (Just (CoddCommentSuccess opts), Nothing) -> pure $ Right (opts, Nothing)
      (Nothing, Just (CoddCommentSuccess conn)) -> pure $ Right ([], Just conn)
      (Nothing, Nothing) -> pure $ Right ([], Nothing)

  case eExtr of
    Left err -> pure $ Left err
    Right (opts, customConnStr)
      |
      -- Detect "-- codd: no-parse"
        OptNoParse True `elem` opts -> do
        -- let consumedText = piecesToText consumedPieces
        -- Remember: the original Stream (Of Text) has already been advanced
        -- beyond its first SQL statement. We have to rely on the parser a little
        -- bit more to reconstruct the full contents of the original stream.
        -- We could figure out how Streaming.copy works and use that, probably.
        -- firstSqlStatement <- maybe "" sqlPieceText
        --   <$> Streaming.head_ sqlPiecesBodyStream
        -- TODO: Why does using only "remainingText" work???
        remainingText <- Streaming.fold_ (<>) "" id sqlStream
        pure $ Right
          ( opts
          , customConnStr
          , UnparsedSql remainingText --consumedText <> firstSqlStatement <> remainingText
          )
      | otherwise -> pure $ Right
        ( opts
        , customConnStr
        -- Prepend consumedPieces to preserve the property that SqlMigration objects
        -- will hold the full original SQL.
        , WellParsedSql $ Streaming.each consumedPieces <> sqlPiecesBodyStream
        )



piecesToText :: Foldable t => t SqlPiece -> Text
piecesToText = foldr ((<>) . sqlPieceText) ""

parseSqlMigration
  :: forall m
   . MonadThrow m
  => String
  -> Stream (Of Text) m ()
  -> m (Either String (SqlMigration m))
parseSqlMigration name t = (toMig =<<) <$> parseAndClassifyMigration t
 where
  dupOpts opts = length (nub opts) < length opts
  checkOpts :: [SectionOption] -> Maybe String
  checkOpts opts
    | inTxn opts && noTxn opts = Just
      "Choose either in-txn, no-txn or leave blank for the default of in-txn"
    | dupOpts opts = Just "Some options are duplicated"
    | otherwise = Nothing
  inTxn opts = OptInTxn False `notElem` opts || OptInTxn True `elem` opts
  noTxn opts = OptInTxn False `elem` opts

  mkMig :: ([SectionOption], Maybe ConnectInfo, ParsedSql m) -> SqlMigration m
  mkMig (opts, customConnString, sql) = SqlMigration
    { migrationName           = name
    , migrationSql            = sql
    , migrationInTxn          = inTxn opts
    , migrationCustomConnInfo = customConnString
    }

  toMig
    :: ([SectionOption], Maybe ConnectInfo, ParsedSql m)
    -> Either String (SqlMigration m)
  toMig x@(ops, _, _) = case checkOpts ops of
    Just err -> Left err
    _        -> Right $ mkMig x

parseAddedSqlMigration
  :: MonadThrow m
  => String
  -> Stream (Of Text) m ()
  -> m (Either String (AddedSqlMigration m))
parseAddedSqlMigration name t = do
  sqlMig <- parseSqlMigration name t
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
