module ParsingSpec where

import Codd.Internal.MultiQueryStatement (parseMultiStatement)
import Codd.Parsing (parseSqlMigrationBGS, parseSqlMigrationSimpleWorkflow, nothingIfEmptyQuery)
import Codd.Types (SqlMigration(..))
import Control.Monad (when, forM_)
import qualified Data.Char as Char
import Data.Either (isLeft)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

newtype RandomSql = RandomSql { unRandomSql :: Text } deriving newtype Show
newtype SyntaticallyValidRandomSql = SyntaticallyValidRandomSql { unSyntRandomSql :: Text } deriving newtype Show

genSql :: Bool -> Gen Text
genSql onlySyntaticallyValid =
    frequency [
        (if onlySyntaticallyValid then 0 else 1, pure "")
        , (50, randomSqlGen)
        ]

    where
        emptyLineGen = pure "\n"
        bizarreLineGen = (<> "\n") . Text.pack . getUnicodeString <$> arbitrary
        cleanerSqlLineGen = elements [
            "SELECT 'so\\'m -- not a comment' FROM ahahaha;"
            , "DO"
                                <> "\n$do$"
                                <> "\nBEGIN"
                                <> "\n   IF NOT EXISTS ("
                                <> "\n      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN"
                                <> "\n"
                                <> "\n      CREATE USER \"codd-user\";"
                                <> "\n   END IF;"
                                <> "\nEND"
                                <> "\n$do$;"
            , "CREATE TABLE \"escaped--table /* nasty */\";"
            , "CREATE TABLE any_table();\n-- ? $1 $2 ? ? ?"
            ]
        lineGen = frequency [ (if onlySyntaticallyValid then 0 else 1, bizarreLineGen), (3, cleanerSqlLineGen) ]
        -- Note: the likelihood that QuickCheck will randomly generate text that has a line starting with "-- codd:"
        -- is so low that we can just ignore it
        commentGen = ("-- " <>) <$> lineGen
        lineOrCommentGen = frequency [ (5, lineGen), (1, commentGen), (1, emptyLineGen) ]
        randomSqlGen = Text.concat <$> listOf1 lineOrCommentGen

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
                        estm1 = parseMultiStatement "CREATE TABLE hello;"
                        estm2 = parseMultiStatement "CREATE TABLE hello"
                    estm1 `shouldBe` Right [ "CREATE TABLE hello;" ]
                    estm2 `shouldBe` Right [ "CREATE TABLE hello" ]
            it "Multiple commands with comments and other stuff" $ do
                    let
                        blocks = ["CREATE TABLE hello;"," SELECT 'so\\'m -- not a comment' FROM ahahaha;","YEAH\n\n-- comment\nABC;"
                                    , "\nDO"
                                    <> "\n$do$"
                                    <> "\nBEGIN"
                                    <> "\n   IF NOT EXISTS ("
                                    <> "\n      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN"
                                    <> "\n"
                                    <> "\n      CREATE USER \"codd-user\";"
                                    <> "\n   END IF;"
                                    <> "\nEND"
                                    <> "\n$do$;"
                                    , "ALTER TABLE \"some-table\";/* some comment ***** with asterisks */ "
                                    , "GO" ]
                        estm = parseMultiStatement $ Text.concat blocks
                    estm `shouldBe` Right blocks
            it "Statements concatenation matches original and statements end with semi-colon" $ do
                    property $ \(unSyntRandomSql -> plainSql) ->
                        let estms = parseMultiStatement plainSql
                        in
                        case estms of
                            Left e -> estms `shouldNotSatisfy` isLeft -- pure () -- Not every generated SQL is valid, and we only care about valid SQL for this parser
                            Right stms -> do
                                Text.concat stms `shouldBe` plainSql
                                -- forM_ (init stms) $ \stm -> stm `shouldSatisfy` (";" `Text.isSuffixOf`)
                                -- The condition above does not always hold: fragments can end with comments..
                                forM_ (init stms) $ \stm -> nothingIfEmptyQuery stm `shouldSatisfy` isJust
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
                        , "Regular sql COMMANDS -- this is not a comment"
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
                forM_ nonEmptyQueries $ \q ->
                    nothingIfEmptyQuery q `shouldBe` Just q