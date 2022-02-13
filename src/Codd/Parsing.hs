module Codd.Parsing
  ( SqlMigration(..)
  , AddedSqlMigration(..)
  , CoddCommentParseResult(..)
  , SqlPiece(..)
  , ParsedSql(..)
  , ParsingOptions(..)
  , connStringParser
  , isTransactionEndingPiece
  , mapSqlMigration
  , nothingIfEmptyQuery
  , piecesToText
  , sqlPieceText
  , parsedSqlText
  , parseSqlMigration
  , parseSqlMigrationOpts
  , parseWithEscapeCharProper
  , parseAddedSqlMigration
  , parseMigrationTimestamp
  , parseSqlPieces
  , toMigrationTimestamp
  ) where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( guard
                                                , void
                                                , when
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , anyChar
                                                , asciiCI
                                                , atEnd
                                                , char
                                                , endOfInput
                                                , endOfLine
                                                , many'
                                                , many1
                                                , manyTill
                                                , match
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
import           Data.List                      ( nub )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time.Clock                ( UTCTime(..) )
import           Data.Time.Format.ISO8601       ( iso8601ParseM )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import           Prelude                 hiding ( takeWhile )
import qualified Prelude


data ParsedSql = ParseFailSqlText !Text | WellParsedSql !Text !(NonEmpty SqlPiece)
  deriving stock (Eq, Show)

data SqlMigration = SqlMigration
  { migrationName            :: FilePath
  , nonDestructiveSql        :: Maybe ParsedSql
  , nonDestructiveForce      :: Bool
  , nonDestructiveInTxn      :: Bool
  , nonDestructiveCustomConn :: Maybe ConnectInfo
  }
  deriving stock (Eq, Show)

data AddedSqlMigration = AddedSqlMigration
  { addedSqlMig       :: SqlMigration
  , addedSqlTimestamp :: DB.UTCTimestamp
  }
  deriving stock Show

mapSqlMigration
  :: (SqlMigration -> SqlMigration) -> AddedSqlMigration -> AddedSqlMigration
mapSqlMigration f (AddedSqlMigration sqlMig tst) =
  AddedSqlMigration (f sqlMig) tst

data SectionOption = OptForce !Bool | OptInTxn !Bool | OptDest !Bool deriving stock (Eq, Show)

data SqlPiece = CommentPiece !Text | WhiteSpacePiece !Text | CopyFromStdinPiece !Text !Text !Text | BeginTransaction !Text | RollbackTransaction !Text | CommitTransaction !Text | OtherSqlPiece !Text
  deriving stock (Show, Eq)

parseSqlPieces :: Text -> Either String (NonEmpty SqlPiece)
parseSqlPieces = parseOnly (sqlPiecesParser <* endOfInput)

parsedSqlText :: ParsedSql -> Text
parsedSqlText (ParseFailSqlText t) = t
parsedSqlText (WellParsedSql t _ ) = t

sqlPieceText :: SqlPiece -> Text
sqlPieceText (CommentPiece        s      ) = s
sqlPieceText (WhiteSpacePiece     s      ) = s
sqlPieceText (BeginTransaction    s      ) = s
sqlPieceText (RollbackTransaction s      ) = s
sqlPieceText (CommitTransaction   s      ) = s
sqlPieceText (OtherSqlPiece       s      ) = s
sqlPieceText (CopyFromStdinPiece s1 s2 s3) = s1 <> s2 <> s3

sqlPiecesParser :: Parser (NonEmpty SqlPiece)
sqlPiecesParser = do
  pcs <- manyTill sqlPieceParser endOfInput
  case pcs of
    []       -> fail "SQL migration is empty"
    (p : ps) -> pure (p :| ps)
 where
  sqlPieceParser =
    CommentPiece
      <$> commentParser
      <|> (WhiteSpacePiece <$> takeWhile1 Char.isSpace)
      <|> copyFromStdinParser
      <|> BeginTransaction
      <$> beginTransactionParser
      <|> RollbackTransaction
      <$> rollbackTransactionParser
      <|> CommitTransaction
      <$> commitTransactionParser
      <|> (OtherSqlPiece <$> anySqlPieceParser)
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
copyFromStdinParser :: Parser SqlPiece
copyFromStdinParser = do
  stmt <- spaceSeparatedTokensToParser
    [ CITextToken "COPY"
    , SqlIdentifier
    , Optional
      [CITextToken "(", Optional [CommaSeparatedIdentifiers], CITextToken ")"]
    , CITextToken "FROM"
    , CITextToken "STDIN"
    , AllUntilEndOfStatement
    ]
  seol                     <- eol
  -- The first alternatie handles a special case: COPY without data (very common in DB dumps)
  (copyData, parsedSuffix) <-
    ("", )
    <$> terminatorOnly
    <|> parseUntilSuffix '\n' newlineAndTerminator
    <|> parseUntilSuffix '\r' newlineAndTerminator -- Sorry Windows users, but you come second..
  pure $ CopyFromStdinPiece (stmt <> seol) copyData parsedSuffix
 where
  newlineAndTerminator = do
    eol1 <- eol
    t    <- terminatorOnly
    pure $ eol1 <> t
  terminatorOnly = do
    s    <- string "\\."
    eol2 <- eol <|> ("" <$ endOfInput)
    pure $ s <> eol2

-- | Parse until finding the suffix string, returning both contents and suffix separately, in this order.
--   Can return an empty string for the contents.
parseUntilSuffix :: Char -> Parser Text -> Parser (Text, Text)
parseUntilSuffix suffixFirstChar wholeSuffix = do
  s                         <- takeWhile (/= suffixFirstChar)
  (parsedSuffix, succeeded) <- (, True) <$> wholeSuffix <|> pure ("", False)
  if succeeded
    then pure (s, parsedSuffix)
    else do
      nc                        <- char suffixFirstChar
      (remain, recParsedSuffix) <- parseUntilSuffix suffixFirstChar wholeSuffix
      pure (Text.snoc s nc <> remain, recParsedSuffix)

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
  host <- idParser "host" -- TODO: IPv6 addresses such as ::1 ??
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

optionParser :: Parser SectionOption
optionParser = do
  skipJustSpace
  x <- force <|> nonDest <|> dest <|> inTxn <|> noTxn <|> fail
    "Valid options after '-- codd:' are 'in-txn', 'no-txn', 'force'"
  skipJustSpace
  return x
 where
  force   = string "force" >> pure (OptForce True)
  nonDest = string "non-destructive" >> pure (OptDest False)
  dest    = string "destructive" >> pure (OptDest True)
  inTxn   = string "in-txn" >> pure (OptInTxn True)
  noTxn   = string "no-txn" >> pure (OptInTxn False)


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

-- | Looks only at comments that precede the first SQL statement and
-- extracts from them custom options and a custom connection string when
-- they exist, or returns a good error message otherwise.
extractCoddOpts
  :: [SqlPiece] -> Either String ([SectionOption], Maybe ConnectInfo)
extractCoddOpts ps =
  let
    firstComments = filter isCommentPiece
      $ Prelude.takeWhile (\p -> isCommentPiece p || isWhiteSpacePiece p) ps
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
      . (parseOnly (coddConnStringCommentParser <* endOfInput) . sqlPieceText)
      )
      firstComments
    headMay []      = Nothing
    headMay (x : _) = Just x
  in
    -- Urgh.. is there no better way of pattern matching the stuff below?
    case (allOptSections, customConnString) of
      (_ : _ : _, _) ->
        Left
          "There must be at most one '-- codd:' comment in the first lines of a migration"
      (_, _ : _ : _) ->
        Left
          "There must be at most one '-- codd-connection:' comment in the first lines of a migration"
      _ -> case (headMay allOptSections, headMay customConnString) of
        (Just (CoddCommentWithGibberish badOptions), _) ->
          Left
            $ "The options '"
            <> Text.unpack badOptions
            <> "' are invalid. Valid options are a comma-separated list of 'in-txn', 'no-txn', 'force'"
        (_, Just (CoddCommentWithGibberish badConn)) ->
          Left
            $ "The connection string '"
            <> Text.unpack badConn
            <> "' is invalid. A valid connection string is in the format 'postgres://username[:password]@host:port/database_name', with backslash to escape 'at' signs and colons."
        (Just (CoddCommentSuccess opts), Just (CoddCommentSuccess conn)) ->
          Right (opts, Just conn)
        (Just (CoddCommentSuccess opts), Nothing) -> Right (opts, Nothing)
        (Nothing, Just (CoddCommentSuccess conn)) -> Right ([], Just conn)
        (Nothing, Nothing) -> Right ([], Nothing)



piecesToText :: Foldable t => t SqlPiece -> Text
piecesToText = foldr ((<>) . sqlPieceText) ""

migrationParserSimpleWorkflow
  :: Parser ([SectionOption], Maybe ConnectInfo, ParsedSql)
migrationParserSimpleWorkflow = do
  (text, sqlPieces) <- match sqlPiecesParser
  when (text /= piecesToText sqlPieces)
    $ fail
        "An internal error happened when parsing. Use '--no-parse' when adding to treat it as in-txn without support for COPY FROM STDIN if that's ok. Also, please report this as a bug."
  case extractCoddOpts (NE.toList sqlPieces) of
    Left err -> fail err
    Right (opts, customConnStr) ->
      pure (opts, customConnStr, WellParsedSql text sqlPieces)

parseSqlMigration :: String -> Text -> Either Text SqlMigration
parseSqlMigration name t = first Text.pack migE >>= toMig
 where
  migE = parseOnly (migrationParserSimpleWorkflow <* endOfInput) t
  dupOpts opts = length (nub opts) < length opts
  checkOpts :: [SectionOption] -> Maybe Text
  checkOpts opts
    | isDest opts
    = Just
      "Simple deployment workflow does not allow for a SQL section explicitly marked as Destructive"
    | inTxn opts && noTxn opts
    = Just
      "Choose either in-txn, no-txn or leave blank for the default of in-txn"
    | dupOpts opts
    = Just "Some options are duplicated"
    | otherwise
    = Nothing
  isDest opts = OptDest True `elem` opts
  inTxn opts = OptInTxn False `notElem` opts || OptInTxn True `elem` opts
  noTxn opts = OptInTxn False `elem` opts

  mkMig :: ([SectionOption], Maybe ConnectInfo, ParsedSql) -> SqlMigration
  mkMig (opts, customConnString, sql) = SqlMigration
    { migrationName            = name
    , nonDestructiveSql        = Just sql
    , nonDestructiveForce      = True
    , nonDestructiveInTxn      = inTxn opts
    , nonDestructiveCustomConn = customConnString
    }

  toMig
    :: ([SectionOption], Maybe ConnectInfo, ParsedSql)
    -> Either Text SqlMigration
  toMig x@(ops, _, _) = case checkOpts ops of
    Just err -> Left err
    _        -> Right $ mkMig x

data ParsingOptions = NoParse | DoParse
  deriving stock (Eq)

parseSqlMigrationOpts
  :: ParsingOptions -> String -> Text -> Either Text SqlMigration
parseSqlMigrationOpts popts name sql = case popts of
  NoParse ->
    Right $ SqlMigration name (Just $ ParseFailSqlText sql) True True Nothing
  _ -> parseSqlMigration name sql

parseAddedSqlMigration
  :: ParsingOptions -> String -> Text -> Either Text AddedSqlMigration
parseAddedSqlMigration popts name t =
  AddedSqlMigration
    <$> parseSqlMigrationOpts popts name t
    <*> parseMigrationTimestamp name

-- | This is supposed to be using a different parser which would double-check our parser in our tests. It's here only until
-- we find a way to remove it.
nothingIfEmptyQuery :: Text -> Maybe ParsedSql
nothingIfEmptyQuery t = case (parseSqlPieces t, t) of
  (Left  _  , "") -> Nothing
  (Left  _  , _ ) -> Just $ ParseFailSqlText t
  (Right pcs, _ ) -> if all (\x -> isWhiteSpacePiece x || isCommentPiece x) pcs
    then Nothing
    else Just (WellParsedSql t pcs)

-- | Converts an arbitrary UTCTime (usually the system's clock when adding a migration) to a Postgres timestamptz
-- in by rounding it to the nearest second to ensure Haskell and Postgres times both behave well. Returns both the rounded UTCTime
-- and the Postgres timestamp.
toMigrationTimestamp :: UTCTime -> (UTCTime, DB.UTCTimestamp)
toMigrationTimestamp (UTCTime day diffTime) =
  let t = UTCTime day (fromInteger $ round diffTime) in (t, DB.Finite t)

-- | Parses the UTC timestamp from a migration's name.
parseMigrationTimestamp :: String -> Either Text DB.UTCTimestamp
parseMigrationTimestamp name =
  case iso8601ParseM (Prelude.takeWhile (/= 'Z') name ++ "Z") of
    Nothing ->
      Left
        $  "Could not find migration timestamp from its name: '"
        <> Text.pack name
        <> "'"
    Just t -> Right $ DB.Finite t
