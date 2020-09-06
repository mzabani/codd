module Codd.Parsing (parseSqlMigration, nothingIfEmptyQuery) where

import Codd.Types (SqlMigration(..))
import Control.Applicative ((<|>))
import Control.Monad (void, guard)
import Data.Attoparsec.Text (Parser, anyChar, atEnd, char, endOfLine, endOfInput, endOfLine, manyTill, parseOnly, peekChar, skipMany, skipSpace, skipWhile, string, sepBy, takeText)
import Data.Bifunctor (bimap)
import qualified Data.Char as Char
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)

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
    coddComment
    opts1 <- optionParser `sepBy` (char ',')
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
    
    where
        everythingUpToCodd = Text.concat <$> manyTill fullLine (endOfInput <|> coddComment)

fullLine :: Parser Text
fullLine = Text.pack <$> manyTill anyChar (endOfLine <|> endOfInput)

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


-- skipBlanksAndComments :: Parser ()
-- skipBlanksAndComments = do
--     -- TODO: Does skipSpace fail on endOfInput? If not, this code can be simplified
--     done <- atEnd
--     case done of
--         True -> pure ()
--         False -> do
--             skipSpace
--             startsComment <- (True <$ string "--") <|> pure False
--             case startsComment of
--                 False -> pure ()
--                 True -> do
--                     void $ manyTill anyChar (endOfInput <|> endOfLine)
--                     skipBlanksAndComments

-- | Given some SQL, returns a Nothing if it doesn't contain any SQL Commands. Useful
--   because you can't run SQL that does not contain any commands.
nothingIfEmptyQuery :: Text -> Maybe Text
nothingIfEmptyQuery t
    | Text.all Char.isSpace t = Nothing
    | parseOnly notJustBlanksAndCommentsParser t /= Right True = Nothing
    | otherwise = Just t
    where notJustBlanksAndCommentsParser :: Parser Bool
          notJustBlanksAndCommentsParser = skipBlanksAndCommentsNoFail >> not <$> atEnd


parseSqlMigration :: FilePath -> Text -> Either Text SqlMigration
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