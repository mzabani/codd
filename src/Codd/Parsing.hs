module Codd.Parsing (parseSqlMigration, parseAddedSqlMigration, parseMigrationTimestamp, nothingIfEmptyQuery, toMigrationTimestamp) where

import Codd.Types (SqlMigration(..), AddedSqlMigration(..))
import Control.Applicative ((<|>))
import Control.Monad (void, guard)
import Data.Attoparsec.Text (Parser, anyChar, atEnd, char, endOfLine, endOfInput, endOfLine, manyTill, parseOnly, peekChar, skipMany, skipSpace, skipWhile, string, sepBy, takeText)
import qualified Data.Attoparsec.Text as Parsec
import Data.Bifunctor (bimap)
import qualified Data.Char as Char
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import qualified Database.PostgreSQL.Simple.Time as DB
import Data.Time.Format.ISO8601 (iso8601ParseM)

data SectionOption = OptForce Bool | OptInTxn Bool | OptDest Bool deriving stock (Ord, Eq, Show)

optionParser :: Parser SectionOption
optionParser = do
    skipJustSpace
    x <- force <|> nonDest <|> dest <|> inTxn <|> noTxn
    skipJustSpace
    return x
    where
        force = string "force" >> pure (OptForce True)
        nonDest = string "non-destructive" >> pure (OptDest False)
        dest = string "destructive" >> pure (OptDest True)
        inTxn = string "in-txn" >> pure (OptInTxn True)
        noTxn = string "no-txn" >> pure (OptInTxn False)

skipJustSpace :: Parser ()
skipJustSpace = skipWhile (== ' ')

coddComment :: Parser ()
coddComment = do
    void $ string "--"
    skipJustSpace
    void $ string "codd:"
    skipJustSpace

migrationParser :: Parser ([SectionOption], Text, Maybe ([SectionOption], Text))
migrationParser = do
    -- Any amount of white space, then first codd comment
    skipSpace
    coddComment <> fail "The first non-white-space line in your migration must begin with '-- codd:'"
    opts1 <- optionParser `sepBy` (char ',') <|> fail "Valid options after '-- codd:' are 'non-destructive', 'destructive', 'in-txn', 'no-txn', 'force'"
    endOfLine
    firstSectionSql <- everythingUpToCodd
    singleSection <- atEnd
    case singleSection of
        True -> pure (opts1, firstSectionSql, Nothing)
        False -> do
            opts2 <- optionParser `sepBy` (char ',')
            endOfLine <|> endOfInput
            secondSectionSql <- takeText
            pure (opts1, firstSectionSql, Just (opts2, secondSectionSql))
            -- TODO: Ideally, a third "-- codd:" would fail parsing

data EverythingUpToCoddAccum = EverythingUpToCoddAccum !Bool !Int
-- | Parses and returns everything from now until "\n-- codd:", consuming the "\n -- codd:" string but not returning its starting "\n".
--   This parser's behaviour is only valid if you use it at the start of a line.
everythingUpToCodd :: Parser Text
everythingUpToCodd = do
    -- Parse until finding a dash, "-", as long as it is only preceded by white-space in its line
    (piece, EverythingUpToCoddAccum _ lastLineLen) <-
        Parsec.runScanner (EverythingUpToCoddAccum True 0) (\(EverythingUpToCoddAccum onlySpacesInLine lenLastLine) c ->
                if c == '-' && onlySpacesInLine then Nothing
                else if c == '\n' then Just $ acc True 1
                else if not (Char.isSpace c) then Just $ acc False (lenLastLine + 1)
                else Just $ acc onlySpacesInLine (lenLastLine + 1))
    -- If we stopped at a dash, it could be a codd section.
    isCoddSection <- (True <$ coddComment) <|> pure False
    if isCoddSection then
        -- Remove last line from "piece"
        pure $ Text.dropEnd lastLineLen piece
    else do
        done <- atEnd
        if done
            then pure piece
        else do
            void $ char '-'
            -- Consume the dash to advance the parser. Since two dashes are so common in SQL, we could try to optimize that path in the future
            remaining <- everythingUpToCodd
            pure $ piece <> "-" <> remaining

    where
        acc = EverythingUpToCoddAccum

takeCommentsUnit :: Parser ()
takeCommentsUnit = skipComment1 <|> skipComment2
    where
        skipComment1 = () <$ (string "--" >> manyTill anyChar (endOfInput <|> endOfLine))
        skipComment2 = () <$ (string "/*" >> manyTill anyChar (string "*/"))

skipBlanksAndCommentsNoFail :: Parser ()
skipBlanksAndCommentsNoFail = skipMany (takeSpaceUnit <|> takeCommentsUnit)
    where
        takeSpaceUnit = do
            c <- peekChar
            guard $ maybe False Char.isSpace c
            skipSpace

-- | Given some SQL, returns a Nothing if it doesn't contain any SQL Commands. Useful
--   because you can't run SQL that does not contain any commands.
nothingIfEmptyQuery :: Text -> Maybe Text
nothingIfEmptyQuery t
    | Text.all Char.isSpace t = Nothing
    | parseOnly notJustBlanksAndCommentsParser t /= Right True = Nothing
    | otherwise = Just t
    where notJustBlanksAndCommentsParser :: Parser Bool
          notJustBlanksAndCommentsParser = skipBlanksAndCommentsNoFail >> (not <$> atEnd)


parseSqlMigration :: String -> Text -> Either Text SqlMigration
parseSqlMigration name t = bimap Text.pack id migE >>= toMig
    where
        migE = parseOnly (migrationParser <* endOfInput) t
        dupOpts (sort -> opts) = any (==True) $ zipWith (==) opts (drop 1 opts)
        checkOpts :: [SectionOption] -> Maybe Text
        checkOpts opts
            | isDest opts && isNonDest opts = Just "Choose either destructive or non-destructive"
            | not (isDest opts) && not (isNonDest opts) = Just "Choose either destructive or non-destructive"
            | inTxn opts && noTxn opts = Just "Choose either in-txn or no-txn"
            | OptDest False `elem` opts && OptDest True `elem` opts = Just "Choose either 'force non-destructive' or just 'non-destructive'"
            | dupOpts opts = Just "Some options are duplicated"
            | otherwise = Nothing
        isNonDest opts = OptDest False `elem` opts
        isDest opts = OptDest True `elem` opts
        inTxn opts = OptInTxn False `notElem` opts
        noTxn opts = OptInTxn False `elem` opts
        isForce opts = OptForce True `elem` opts

        mkMig :: Maybe ([SectionOption], Text) -> Maybe ([SectionOption], Text) -> SqlMigration
        mkMig mndest mdest = SqlMigration {
            migrationName = name
            , nonDestructiveSql = nothingIfEmptyQuery $ fromMaybe "" (snd <$> mndest)
            , nonDestructiveForce = fromMaybe False (isForce . fst <$> mndest)
            , nonDestructiveInTxn = fromMaybe True (inTxn . fst <$> mndest)
            , destructiveSql = nothingIfEmptyQuery $ fromMaybe "" (snd <$> mdest)
            , destructiveInTxn = fromMaybe True (inTxn . fst <$> mdest)
        }

        toMig :: ([SectionOption], Text, Maybe ([SectionOption], Text)) -> Either Text SqlMigration
        toMig (fsops, fssql, mss) =
            case (fsops, mss) of
                ([], _) -> Left "Migration needs at least one section marked as 'non-destructive' or 'destructive'"
                (_, Nothing) ->
                    case (checkOpts fsops, isDest fsops) of
                        (Just err, _) -> Left $ "Error in the first section: " <> err
                        (Nothing, True) -> Right $ mkMig Nothing $ Just (fsops, fssql)
                        (Nothing, False) -> Right $ mkMig (Just (fsops, fssql)) Nothing
                (_, Just (ssops, sssql)) ->
                    case (checkOpts fsops, checkOpts ssops, isDest fsops, isDest ssops) of
                        (Just err, _, _, _) -> Left $ "Error in the first section: " <> err
                        (_, Just err, _, _) -> Left $ "Error in the second section: " <> err
                        (_, _, False, True) -> Right $ mkMig (Just (fsops, fssql)) (Just (ssops, sssql))
                        (_, _, True, False) -> Right $ mkMig (Just (ssops, sssql)) (Just (fsops, fssql))
                        (_, _, True, True)  -> Left "There can't be two destructive sections"
                        (_, _, False, False)  -> Left "There can't be two non-destructive sections"

parseAddedSqlMigration :: String -> Text -> Either Text AddedSqlMigration
parseAddedSqlMigration name t = AddedSqlMigration <$> parseSqlMigration name t <*> parseMigrationTimestamp name

-- | Converts an arbitrary UTCTime (usually the system's clock when adding a migration) to a Postgres timestamptz
-- in by rounding it to the nearest second to ensure Haskell and Postgres times both behave well. Returns both the rounded UTCTime
-- and the Postgres timestamp.
toMigrationTimestamp :: UTCTime -> (UTCTime, DB.UTCTimestamp)
toMigrationTimestamp (UTCTime day diffTime) = let t = UTCTime day (fromInteger $ round diffTime) in (t, DB.Finite t)

-- | Parses the UTC timestamp from a migration's name.
parseMigrationTimestamp :: String -> Either Text DB.UTCTimestamp
parseMigrationTimestamp name =
    case iso8601ParseM (takeWhile (/= 'Z') name ++ "Z") of
        Nothing -> Left $ "Could not find migration timestamp from its name: '" <> Text.pack name <> "'"
        Just t -> Right $ DB.Finite t