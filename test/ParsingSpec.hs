module ParsingSpec where

import Codd.Internal.MultiQueryStatement (SqlPiece(..), parseMultiStatement, blockToText)
import Codd.Parsing (parseSqlMigrationBGS, parseSqlMigrationSimpleWorkflow, nothingIfEmptyQuery)
import Codd.Types (SqlMigration(..))
import Control.Monad (when, forM_)
import qualified Data.Char as Char
import Data.Either (isLeft)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

newtype RandomSql = RandomSql { unRandomSql :: Text } deriving newtype Show
-- | Syntatically valid SQL must contain at least one statement!
newtype SyntaticallyValidRandomSql = SyntaticallyValidRandomSql { unSyntRandomSql :: Text } deriving newtype Show

genSingleSqlStatement :: Gen SqlPiece
genSingleSqlStatement = elements validSqlStatements

otherStatementNoComments :: Text -> SqlPiece
otherStatementNoComments t = OtherSqlPiece t


validSqlStatements :: [SqlPiece]
validSqlStatements = [
            otherStatementNoComments "SELECT 'so\\'m -- not a comment' FROM ahahaha;"
            , otherStatementNoComments $ "DO"
                                <> "\n$do$"
                                <> "\nBEGIN"
                                <> "\n   IF NOT EXISTS ("
                                <> "\n      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN"
                                <> "\n"
                                <> "\n      CREATE USER \"codd-user\";"
                                <> "\n   END IF;"
                                <> "\nEND"
                                <> "\n$do$;"
            , otherStatementNoComments "CREATE TABLE \"escaped--table /* nasty */\";"
            , otherStatementNoComments "CREATE TABLE any_table();"
            , otherStatementNoComments $ "CREATE FUNCTION sales_tax(subtotal real) RETURNS real AS $$"
                                        <> "\nBEGIN"
                                        <> "\n    RETURN subtotal * 0.06;"
                                        <> "\nEND;"
                                        <> "\n$$ LANGUAGE plpgsql;"
            , otherStatementNoComments $ "CREATE FUNCTION instr(varchar, integer) RETURNS integer AS $$"
                    <> "\nDECLARE"
                    <> "\n    v_string ALIAS FOR $1;"
                    <> "\n    index ALIAS FOR $2;"
                    <> "\nBEGIN"
                    <> "\n    -- some computations using v_string and index here"
                    <> "\nEND;"
                    <> "\n$$ LANGUAGE plpgsql;"
            , otherStatementNoComments "select U&'d\\0061t\\+000061', U&'\\0441\\043B\\043E\\043D', U&'d!0061t!+000061' UESCAPE '!', X'1FF', B'1001';"
            , otherStatementNoComments "SELECT 'some''quoted ''string';"
            , otherStatementNoComments "SELECT \"some\"\"quoted identifier\";"
            , otherStatementNoComments "SELECT 'double quotes \" inside single quotes \" - 2';"
            , otherStatementNoComments "SELECT \"single quotes ' inside double quotes ' - 2\";"
            , otherStatementNoComments $ "$function$"
                <> "\nBEGIN"
                <> "\n    RETURN ($1 ~ $q$[\t\r\n\v\\]$q$);"
                <> "\nEND;"
                <> "\n$function$;"
            , otherStatementNoComments "SELECT COALESCE(4, 1 - 2) - 3 + 4 - 5;"
            , otherStatementNoComments "SELECT (1 - 4) / 5 * 3 / 9.1;"
            , CopyFromStdinPiece "COPY employee FROM STDIN WITH (FORMAT CSV);\n" "5,Dracula,master\n6,The Grinch,master" "\n\\.\n"

            -- TODO: Nested C-Style comments (https://www.postgresql.org/docs/9.2/sql-syntax-lexical.html)
            -- , "/* multiline comment"
            --   <> "\n  * with nesting: /* nested block comment */"
            --   <> "\n  */ SELECT 1;"
            ]

genSql :: Bool -> Gen Text
genSql onlySyntaticallyValid =
    frequency [
        (if onlySyntaticallyValid then 0 else 1, pure "")
        , (50, randomSqlGen)
        ]

    where
        emptyLineGen = pure "\n"
        bizarreLineGen = (<> "\n") . Text.pack . getUnicodeString <$> arbitrary
        lineGen = frequency [ (if onlySyntaticallyValid then 0 else 1, bizarreLineGen), (1, emptyLineGen), (5, blockToText <$> genSingleSqlStatement) ]
        -- Note: the likelihood that QuickCheck will randomly generate text that has a line starting with "-- codd:"
        -- is so low that we can just ignore it
        dashDashCommentGen = (<> "\n") . ("-- " <>) . (Text.replace "\n" "") <$> bizarreLineGen
        cStyleCommentGen = ("/*" <>) . (<> "*/") . (Text.replace "*/" "") <$> bizarreLineGen
        commentGen = frequency [ (5, dashDashCommentGen), (1, cStyleCommentGen) ]
        lineOrCommentGen = frequency [ (5, lineGen), (1, commentGen) ]
        randomSqlGen = fmap Text.concat $ do
            l1 <- listOf lineOrCommentGen
            l2 <- listOf lineOrCommentGen
            atLeastOneStmt <- blockToText <$> genSingleSqlStatement
            let
                finalList = 
                    if onlySyntaticallyValid then
                        l1 ++ (atLeastOneStmt : l2)
                    else
                        l1 ++ l2

                mapLast :: (a -> a) -> [a] -> [a]
                mapLast _ [] = []
                mapLast f (x:[]) = [f x]
                mapLast f (x:xs) = x : mapLast f xs
            
            -- Optionally remove semi-colon from the last command if it ends with one
            removeLastSemiColon <- arbitrary
            pure $
                if removeLastSemiColon then mapLast (\t -> fromMaybe t (Text.stripSuffix ";" t)) finalList
                else finalList

instance Arbitrary RandomSql where
    arbitrary = fmap RandomSql (genSql False)

instance Arbitrary SyntaticallyValidRandomSql where
    arbitrary = fmap SyntaticallyValidRandomSql (genSql True)

spec :: Spec
spec = do
    describe "Parsing tests" $ do
        context "Multi Query Statement Parser" $ do
            it "Single command with and without semi-colon" $ do
                    let
                        eblks :: Either String [[SqlPiece]] =
                            traverse parseMultiStatement [
                                    "CREATE TABLE hello;"
                                  , "CREATE TABLE hello"
                                  , "CREATE TABLE hello; -- Comment"
                                  , "CREATE TABLE hello -- Comment"
                                  , "CREATE TABLE hello -- Comment\n;"
                                ]
                    eblks `shouldBe` Right [ [ otherStatementNoComments "CREATE TABLE hello;" ]
                                           , [ otherStatementNoComments "CREATE TABLE hello" ]
                                           , [ otherStatementNoComments "CREATE TABLE hello;", WhiteSpacePiece " ", CommentPiece "-- Comment" ]
                                           , [ otherStatementNoComments "CREATE TABLE hello -- Comment" ]
                                           , [ otherStatementNoComments "CREATE TABLE hello -- Comment\n;" ]
                                    ]
            it "Statement separation boundaries are good" $
                forAll (listOf1 genSingleSqlStatement) $ \blocks -> do
                    let
                        estm = parseMultiStatement $ Text.concat (map blockToText blocks)
                    estm `shouldBe` Right blocks
            it "Statements concatenation matches original and statements end with semi-colon" $ do
                    property $ \(unSyntRandomSql -> plainSql) -> do
                        let eblks = parseMultiStatement plainSql
                        -- putStrLn "--------------------------------------------------"
                        -- print plainSql
                        case eblks of
                            Left e -> eblks `shouldNotSatisfy` isLeft
                            Right blks -> do
                                let
                                    comments = [ t | CommentPiece t <- blks ]
                                    whtspc = [ t | WhiteSpacePiece t <- blks ]
                                Text.concat (map blockToText blks) `shouldBe` plainSql
                                forM_ blks $ \case
                                    CommentPiece t -> t `shouldSatisfy` (\c -> "--" `Text.isPrefixOf` c || "/*" `Text.isPrefixOf` c)
                                    WhiteSpacePiece t -> t `shouldSatisfy` (\c -> Text.strip c == "")
                                    CopyFromStdinPiece c d ll -> ll `shouldSatisfy` (\l -> l == "\n\\.\n" || l == "\r\n\\.\r\n")
                                    
        context "Simple mode" $ do
            context "Valid SQL Migrations" $ do
                it "Plain Sql Migration, missing optional options" $ do
                    property $ \(unRandomSql -> plainSql) ->
                        let parsedMig = parseSqlMigrationSimpleWorkflow "any-name.sql" plainSql
                        in
                        case nothingIfEmptyQuery plainSql of
                            Nothing -> parsedMig `shouldSatisfy` isLeft
                            _ -> parsedMig `shouldBe`
                                        Right SqlMigration {
                                            migrationName = "any-name.sql"
                                            , nonDestructiveSql = Just plainSql -- That's right. Simple mode is just Blue-Green-Safe with force-non-destructive enabled
                                            , nonDestructiveForce = True
                                            , nonDestructiveInTxn = True
                                            , destructiveSql = Nothing
                                            , destructiveInTxn = True
                                        }
                it "Sql Migration options parsed correctly" $
                    let
                        plainSql = "SOME SQL"
                        sql = "-- codd: no-txn\n"
                            <> plainSql
                    in
                        parseSqlMigrationSimpleWorkflow "any-name.sql" sql `shouldBe` Right SqlMigration {
                                migrationName = "any-name.sql"
                                , nonDestructiveSql = Just plainSql
                                , nonDestructiveForce = True
                                , nonDestructiveInTxn = False
                                , destructiveSql = Nothing
                                , destructiveInTxn = True
                            }

                it "in-txn and no-txn are mutually exclusive" $
                    let
                        plainSql = "SOME SQL"
                        sql = "-- codd: no-txn, in-txn\n"
                            <> plainSql
                    in
                        parseSqlMigrationSimpleWorkflow "any-name.sql" sql `shouldSatisfy` isLeft
        
        context "Blue-Green-Safe mode" $ do
            context "Valid SQL Migrations" $ do
                it "Sql Migration with two sections, missing optional options" $ do
                    property $ \(unRandomSql -> nonDestSql, unRandomSql -> destSql) ->
                        let
                            sql = "-- codd: non-destructive\n"
                                <> nonDestSql
                                <> "\n-- codd: destructive\n"
                                <> destSql
                        in
                            parseSqlMigrationBGS "any-name.sql" sql `shouldBe` Right SqlMigration {
                                migrationName = "any-name.sql"
                                , nonDestructiveSql = nothingIfEmptyQuery nonDestSql
                                , nonDestructiveForce = False
                                , nonDestructiveInTxn = True
                                , destructiveSql = nothingIfEmptyQuery destSql
                                , destructiveInTxn = True
                            }
                it "Sql Migration with one section, missing optional options" $ do
                    property $ \(unRandomSql -> sectionSql, nonDest :: Bool) ->
                        let
                            sql = (if nonDest then "-- codd: non-destructive\n" else "\n-- codd: destructive\n")
                                    <> sectionSql
                            (nonDestSql, destSql) = if nonDest then (sectionSql, "") else ("", sectionSql)
                        in
                            parseSqlMigrationBGS "any-name.sql" sql `shouldBe` Right SqlMigration {
                                migrationName = "any-name.sql"
                                , nonDestructiveSql = nothingIfEmptyQuery nonDestSql
                                , nonDestructiveForce = False
                                , nonDestructiveInTxn = True
                                , destructiveSql = nothingIfEmptyQuery destSql
                                , destructiveInTxn = True
                            }
                it "Sql Migration options parsed correctly" $
                    let
                        nonDestSql = "SOME SQL"
                        destSql = "MORE SQL"
                        sql = "-- codd: force, non-destructive, in-txn\n"
                            <> nonDestSql
                            <> "\n-- codd: no-txn, destructive\n"
                            <> destSql
                    in
                        parseSqlMigrationBGS "any-name.sql" sql `shouldBe` Right SqlMigration {
                                migrationName = "any-name.sql"
                                , nonDestructiveSql = Just nonDestSql
                                , nonDestructiveForce = True
                                , nonDestructiveInTxn = True
                                , destructiveSql = Just destSql
                                , destructiveInTxn = False
                            }

                it "A real-life SqlMigration parsed correctly" $
                    let
                        nonDestSql = 
                            "\n\n"
                            <> "-- README:\n"
                            -- <> "-- This is an example of a Migration that renames a column in a Blue-Green-Safe way. Both Old and New Apps\n"
                            -- <> "-- need not be concerned of the new/old column names here. We recommend caution and testing when using this.\n\n"

                            -- <> "-- 1. Add the column and set its values, initially\n"
                            <> "ALTER TABLE employee ADD COLUMN employee_name TEXT; -- TODO: Remember to set a good DEFAULT if you need one.\n"
                            <> "UPDATE employee SET employee_name=name WHERE name IS DISTINCT FROM employee_name;\n"

                        destSql = "DROP TRIGGER employee_old_app_update_column_name ON employee;\nDROP TRIGGER employee_old_app_insert_column_name ON employee;\n"

                        sql = "-- codd: non-destructive\n"
                            <> nonDestSql
                            <> "\n-- codd: destructive\n"
                            <> destSql
                    in
                        parseSqlMigrationBGS "any-name.sql" sql `shouldBe` Right SqlMigration {
                                migrationName = "any-name.sql"
                                , nonDestructiveSql = Just nonDestSql
                                , nonDestructiveForce = False
                                , nonDestructiveInTxn = True
                                , destructiveSql = Just destSql
                                , destructiveInTxn = True
                            }
            
            context "Invalid SQL Migrations" $ do
                it "Sql Migration Parser never blocks for random text" $ do
                    property $ \(unRandomSql -> anyText) -> do
                        parseSqlMigrationBGS "any-name.sql" anyText `shouldSatisfy` const True
                        parseSqlMigrationSimpleWorkflow "any-name.sql" anyText `shouldSatisfy` const True

                it "Gibberish after -- codd:" $
                    let
                        sql = "-- codd: complete gibberish\n"
                            <> "ANY SQL HERE"
                    in
                        parseSqlMigrationBGS "any-name.sql" sql `shouldSatisfy` isLeft
                
                it "Duplicate options" $
                    let
                        sql = "-- codd: force, non-destructive, in-txn, non-destructive\n"
                            <> "ANY SQL HERE"
                    in
                        parseSqlMigrationBGS "any-name.sql" sql `shouldSatisfy` \case
                                                                    Right _ -> False
                                                                    Left err -> "duplicate" `Text.isInfixOf` err
                it "Unknown / mistyped options" $
                    let
                        sql = "-- codd: force, NON-Destructive, in-txn\n"
                            <> "ANY SQL HERE"
                    in
                        parseSqlMigrationBGS "any-name.sql" sql `shouldSatisfy` isLeft

                it "Missing non-destructive and destructive, otherwise valid options" $
                    let
                        sql = "-- codd: in-txn\n"
                            <> "ANY SQL HERE"
                    in
                        parseSqlMigrationBGS "any-name.sql" sql `shouldSatisfy` isLeft

                it "Two non-destructive sections" $
                    let
                        sql = "-- codd: non-destructive\n"
                            <> "ANY SQL HERE"
                            <> "\n--codd: non-destructive\n"
                            <> "MORE SQL HERE"
                    in
                        parseSqlMigrationBGS "any-name.sql" sql `shouldSatisfy` isLeft

                it "Two destructive sections" $
                    let
                        sql = "-- codd: destructive\n"
                            <> "ANY SQL HERE"
                            <> "\n--codd: destructive\n"
                            <> "MORE SQL HERE"
                    in
                        parseSqlMigrationBGS "any-name.sql" sql `shouldSatisfy` isLeft

                it "Can't BEGIN, COMMIT or ROLLBACK Transactions inside SQL migrations" $ pendingWith "Testing this by selecting txid_current() might be more effective"

                it "SAVEPOINTs need to be released or rolled back inside SQL migrations" $ pendingWith "Testing this by selecting txid_current() might be more effective"

        context "Other important behaviours to test" $ do
            it "Empty queries detector works well" $ do
                let
                    emptyQueries = [
                        ""
                        , "           "
                        , "      --Some comment           "
                        , "    /* Just comment */ \n"
                        , "      --Some comment    \n-- Some other comment       "
                        , "      --Some comment    \n-- Some other comment\n\n\n       "
                        , "    /* Just comment */ \n -- Other comment \n\n\n\n"
                        , "    /* Just comment */ \n -- Other comment \n\n\n\n"
                        , "\n\n"
                            <> "-- README:\n"
                            <> "-- This is an example of a Migration that renames a column in a Blue-Green-Safe way. Both Old and New Apps\n"
                            <> "-- need not be concerned of the new/old column names here. We recommend caution and testing when using this.\n\n"

                            <> "-- 1. Add the column and set its values, initially\n"
                            <> "-- ALTER TABLE employee ADD COLUMN employee_name TEXT; -- TODO: Remember to set a good DEFAULT if you need one.\n"
                            <> "/* UPDATE employee SET employee_name=name WHERE name IS DISTINCT FROM employee_name; */ \n"
                        ]
                    nonEmptyQueries = [
                        "      --Some comment    \n-- Some other comment\n\n\n Some SQL Command      "
                        , "      --Some comment    \n-- Some other comment\n\n\n - - This is not a valid comment and should be considered SQL"
                        , "\n\n--Some comment    \n-- Some other comment\n\n\n -- Other comment\n\n\n SQL command"
                        , "SOME SQL COMMAND      --Some comment    \n-- Some other comment\n\n\n - - This is not a valid comment and should be considered SQL"
                        , "Regular sql COMMANDS -- this is a comment"
                        , "Regular sql COMMANDS \n-- this is a comment\n"
                        , "Regular sql /* With comment */ COMMANDS"
                        , "/* With comment */ SQL COMMANDS"
                        , "/* With comment */\n\nSQL COMMANDS\n-- Comment"
                        , "\n\n"
                            <> "-- README:\n"
                            <> "-- This is an example of a Migration that renames a column in a Blue-Green-Safe way. Both Old and New Apps\n"
                            <> "-- need not be concerned of the new/old column names here. We recommend caution and testing when using this.\n\n"

                            <> "-- 1. Add the column and set its values, initially\n"
                            <> "ALTER TABLE employee ADD COLUMN employee_name TEXT; -- TODO: Remember to set a good DEFAULT if you need one.\n"
                            <> "UPDATE employee SET employee_name=name WHERE name IS DISTINCT FROM employee_name;\n"
                        ]
                forM_ emptyQueries $ \q ->
                    (q, nothingIfEmptyQuery q) `shouldBe` (q, Nothing)
                forM_ (nonEmptyQueries ++ map blockToText validSqlStatements) $ \q ->
                    nothingIfEmptyQuery q `shouldBe` Just q