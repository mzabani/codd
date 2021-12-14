module Codd.Parsing
  ( SqlMigration(..)
  , AddedSqlMigration(..)
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
  , parseSqlMigrationBGS
  , parseSqlMigrationSimpleWorkflow
  , parseSqlMigration
  , parseWithEscapeCharProper
  , parseAddedSqlMigration
  , parseMigrationTimestamp
  , parseSqlPieces
  , toMigrationTimestamp
  ) where

import           Codd.Types                     ( DeploymentWorkflow(..) )
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
import           Data.List                      ( foldl'
                                                , nub
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromMaybe )
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
  { migrationName       :: FilePath
  , nonDestructiveSql   :: Maybe ParsedSql
  , nonDestructiveForce :: Bool
  , nonDestructiveInTxn :: Bool
  , destructiveSql      :: Maybe ParsedSql
  , destructiveInTxn    :: Bool
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


-- | Parses a string in the format protocol://username[:password]@host:port/database_name
connStringParser :: Parser ConnectInfo
connStringParser = do
  void $ string "postgres://"
  usr <- idParser "username"
  pwd <- (char ':' *> idParser "password") <|> pure ""
  void $ char '@'
  host <- idParser "host" -- TODO: IPv6 addresses such as ::1 ??
  void $ char ':'
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
    x <- Text.unpack <$> parseWithEscapeCharProper (\c -> c == ':' || c == '@')
    when (x == "")
      $  fail
      $  "Could not find a "
      <> idName
      <> " in the connection string."
    pure x

optionParser :: Parser SectionOption
optionParser = do
  skipJustSpace
  x <-
    force
    <|> nonDest
    <|> dest
    <|> inTxn
    <|> noTxn
    -- <|> customConnString
    <|> fail
          "Valid options after '-- codd:' are 'non-destructive', 'destructive', 'in-txn', 'no-txn', 'force'"
  skipJustSpace
  return x
 where
  force   = string "force" >> pure (OptForce True)
  nonDest = string "non-destructive" >> pure (OptDest False)
  dest    = string "destructive" >> pure (OptDest True)
  inTxn   = string "in-txn" >> pure (OptInTxn True)
  noTxn   = string "no-txn" >> pure (OptInTxn False)
  -- customConnString = string "connection=" >> OptConnString <$> connStringParser

skipJustSpace :: Parser ()
skipJustSpace = skipWhile (== ' ')

coddCommentParser :: Parser [SectionOption]
coddCommentParser = do
  void $ string "--"
  skipJustSpace
  void $ string "codd:"
  skipJustSpace
  opts <- optionParser `sepBy1` char ','
  endOfLine
  pure opts

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

-- | Splits SQL pieces into multiple segments separated by "-- codd: opts" comments.
-- - The CommentPieces that contain "-- codd: opts" themselves are also included in the returned lists.
-- - If no "-- codd: opts" comment is found, this returns a one-element list with all provided SQL pieces in it.
--   Any kind of SQL contents before the first "-- codd: opts" comment will make this return an error,
--   and the lack of any kind of non-white-space SQL contents after a "-- codd: opts" comment makes this return
--   an error as well.
-- - In case of success, every provided SqlPiece is contained in the response exactly once (without duplicates).
splitCoddOpts
  :: NonEmpty SqlPiece
  -> Either String (NonEmpty ([SectionOption], NonEmpty SqlPiece))
splitCoddOpts (p :| ps) = validate $ NE.reverse $ foldl' accum accInit ps
 where
  validate
    :: NonEmpty ([SectionOption], NonEmpty SqlPiece)
    -> Either String (NonEmpty ([SectionOption], NonEmpty SqlPiece))
  validate ls =
    let
      allPiecesInOrder :: [SqlPiece]
      allPiecesInOrder = mconcat $ NE.toList $ fmap (NE.toList . snd) ls
    in
      if or
         $ fmap (all (\x -> isWhiteSpacePiece x || isCommentPiece x) . snd) ls
      then
        Left
          "There must be some SQL we can run in for every section in the migration"
      else
        if length (p : ps) /= length allPiecesInOrder || any
           not
           (zipWith (==) (p : ps) allPiecesInOrder)
        then
          Left
            "A internal error in SQL section splitting happened. Please report this as a bug."
        else
          Right ls
  accInit = (fromMaybe [] (optsOrNothing p), p :| []) :| []
  optsOrNothing el = case el of
    CommentPiece c -> case parseOnly (coddCommentParser <* endOfInput) c of
      Left  _    -> Nothing
      Right opts -> Just opts
    _ -> Nothing
  accum acc@((currOpts, currPieces) :| xs) el = case optsOrNothing el of
    Just newSectionOpts -> NE.cons (newSectionOpts, el :| []) acc
    _                   -> (currOpts, currPieces <> (el :| [])) :| xs

piecesToText :: Foldable t => t SqlPiece -> Text
piecesToText = foldr ((<>) . sqlPieceText) ""

migrationParserSimpleWorkflow :: Parser ([SectionOption], ParsedSql)
migrationParserSimpleWorkflow = do
  (text, sqlPieces) <- match sqlPiecesParser
  when (text /= piecesToText sqlPieces)
    $ fail
        "An internal error happened when parsing. Use '--no-parse' when adding to treat it as in-txn without support for COPY FROM STDIN if that's ok. Also, please report this as a bug."
  let sections = splitCoddOpts sqlPieces

  -- At most one "-- codd: opts" can exist, but we are currently accepting multiple ones..
  -- If the migration is Blue-Green-Safe, then when it's treated as Simple, the destructive
  -- section will run right after the non-destructive, and it should all be ok.. hopefully
  case sections of
    Left  err               -> fail err
    -- Right (_ :| (_ : _)) -> fail "At most one '-- codd: options' section can exist in a migration and it must be the very first line in the file."
    Right ((opts1, _) :| _) -> pure (opts1, WellParsedSql text sqlPieces)

migrationParserBGS
  :: Parser ([SectionOption], ParsedSql, Maybe ([SectionOption], ParsedSql))
migrationParserBGS = do
  (text, sqlPieces) <- match sqlPiecesParser
  when (text /= piecesToText sqlPieces)
    $ fail
        "An internal error happened when parsing. Use '--no-parse' when adding to treat it as a purely non-destructive, in-txn migration without support for COPY FROM STDIN if that's ok. Also, please report this as a bug."
  let sections = splitCoddOpts sqlPieces

  -- At most _two_ "-- codd: opts" can exist
  case sections of
    Left err -> fail err
    Right (_ :| (_ : _ : _)) ->
      fail
        "At most two '-- codd: options' section can exist in a Blue-Green-Safe migration."
    Right ((opts1, sql1) :| []) ->
      pure (opts1, WellParsedSql (piecesToText sql1) sql1, Nothing)
    Right ((opts1, sql1) :| [(opts2, sql2)]) -> pure
      ( opts1
      , WellParsedSql (piecesToText sql1) sql1
      , Just (opts2, WellParsedSql (piecesToText sql2) sql2)
      )

parseSqlMigrationSimpleWorkflow :: String -> Text -> Either Text SqlMigration
parseSqlMigrationSimpleWorkflow name t = first Text.pack migE >>= toMig
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

  mkMig :: ([SectionOption], ParsedSql) -> SqlMigration
  mkMig (opts, sql) = SqlMigration { migrationName       = name
                                   , nonDestructiveSql   = Just sql
                                   , nonDestructiveForce = True
                                   , nonDestructiveInTxn = inTxn opts
                                   , destructiveSql      = Nothing
                                   , destructiveInTxn    = True
                                   }

  toMig :: ([SectionOption], ParsedSql) -> Either Text SqlMigration
  toMig x@(ops, _) = case checkOpts ops of
    Just err -> Left err
    _        -> Right $ mkMig x

parseSqlMigrationBGS :: String -> Text -> Either Text SqlMigration
parseSqlMigrationBGS name t = first Text.pack migE >>= toMig
 where
  migE = parseOnly (migrationParserBGS <* endOfInput) t
  dupOpts opts = length (nub opts) < length opts
  checkOpts :: [SectionOption] -> Maybe Text
  checkOpts opts
    | isDest opts && isNonDest opts = Just
      "Choose either destructive or non-destructive"
    | not (isDest opts) && not (isNonDest opts) = Just
      "Choose either destructive or non-destructive"
    | inTxn opts && noTxn opts = Just "Choose either in-txn or no-txn"
    | OptDest False `elem` opts && OptDest True `elem` opts = Just
      "Choose either 'force non-destructive' or just 'non-destructive'"
    | dupOpts opts = Just "Some options are duplicated"
    | otherwise = Nothing
  isNonDest opts = OptDest False `elem` opts
  isDest opts = OptDest True `elem` opts
  inTxn opts = OptInTxn False `notElem` opts
  noTxn opts = OptInTxn False `elem` opts
  isForce opts = OptForce True `elem` opts

  mkMig
    :: Maybe ([SectionOption], ParsedSql)
    -> Maybe ([SectionOption], ParsedSql)
    -> SqlMigration
  mkMig mndest mdest = SqlMigration
    { migrationName       = name
    , nonDestructiveSql   = snd <$> mndest
    , nonDestructiveForce = maybe False (isForce . fst) mndest
    , nonDestructiveInTxn = maybe True (inTxn . fst) mndest
    , destructiveSql      = snd <$> mdest
    , destructiveInTxn    = maybe True (inTxn . fst) mdest
    }

  toMig
    :: ([SectionOption], ParsedSql, Maybe ([SectionOption], ParsedSql))
    -> Either Text SqlMigration
  toMig (fsops, fssql, mss) = case (fsops, mss) of
    ([], _) ->
      Left
        "Migration needs at least one section marked as 'non-destructive' or 'destructive'"
    (_, Nothing) -> case (checkOpts fsops, isDest fsops) of
      (Just err, _    ) -> Left $ "Error in the first section: " <> err
      (Nothing , True ) -> Right $ mkMig Nothing $ Just (fsops, fssql)
      (Nothing , False) -> Right $ mkMig (Just (fsops, fssql)) Nothing
    (_, Just (ssops, sssql)) ->
      case (checkOpts fsops, checkOpts ssops, isDest fsops, isDest ssops) of
        (Just err, _, _, _) -> Left $ "Error in the first section: " <> err
        (_, Just err, _, _) -> Left $ "Error in the second section: " <> err
        (_, _, False, True) ->
          Right $ mkMig (Just (fsops, fssql)) (Just (ssops, sssql))
        (_, _, True, False) ->
          Right $ mkMig (Just (ssops, sssql)) (Just (fsops, fssql))
        (_, _, True, True) -> Left "There can't be two destructive sections"
        (_, _, False, False) ->
          Left "There can't be two non-destructive sections"

data ParsingOptions = NoParse | DoParse
  deriving stock (Eq)

parseSqlMigration
  :: DeploymentWorkflow
  -> ParsingOptions
  -> String
  -> Text
  -> Either Text SqlMigration
parseSqlMigration dw popts name sql = case (dw, popts) of
  (_, NoParse) -> Right
    $ SqlMigration name (Just $ ParseFailSqlText sql) True True Nothing True
  (SimpleDeployment, _) -> parseSqlMigrationSimpleWorkflow name sql
  (BlueGreenSafeDeploymentUpToAndIncluding _, _) ->
    parseSqlMigrationBGS name sql

parseAddedSqlMigration
  :: DeploymentWorkflow
  -> ParsingOptions
  -> String
  -> Text
  -> Either Text AddedSqlMigration
parseAddedSqlMigration depFlow popts name t =
  AddedSqlMigration
    <$> parseSqlMigration depFlow popts name t
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
