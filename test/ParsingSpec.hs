module ParsingSpec where

import           Codd.Parsing                   ( ParsedSql(..)
                                                , ParsingOptions(..)
                                                , SqlMigration(..)
                                                , SqlPiece(..)
                                                , nothingIfEmptyQuery
                                                , parseSqlMigration
                                                , parseSqlMigrationOpts
                                                , parseSqlPieces
                                                , piecesToText
                                                , sqlPieceText
                                                )
import           Control.Monad                  ( forM_
                                                , when
                                                )
import qualified Data.Char                     as Char
import           Data.Either                    ( isLeft )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           DbUtils                        ( mkValidSql )
import           EnvironmentSpec                ( ConnStringGen(..) )
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSuccess )
import           Test.QuickCheck

newtype RandomSql = RandomSql {unRandomSql :: Text} deriving newtype (Show)

-- | Syntactically valid SQL must contain at least one statement!
newtype SyntacticallyValidRandomSql = SyntacticallyValidRandomSql {unSyntRandomSql :: Text} deriving newtype (Show)

genSingleSqlStatement :: Gen SqlPiece
genSingleSqlStatement = elements validSqlStatements

piecesToParsedSql :: NonEmpty SqlPiece -> ParsedSql
piecesToParsedSql pcs = WellParsedSql (piecesToText pcs) pcs

validSqlStatements :: [SqlPiece]
validSqlStatements =
  [ OtherSqlPiece "SELECT 'so\\'m -- not a comment' FROM ahahaha;"
  , OtherSqlPiece
    $  "DO"
    <> "\n$do$"
    <> "\nBEGIN"
    <> "\n   IF NOT EXISTS ("
    <> "\n      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN"
    <> "\n"
    <> "\n      CREATE USER \"codd-user\";"
    <> "\n   END IF;"
    <> "\nEND"
    <> "\n$do$;"
  , OtherSqlPiece "CREATE TABLE \"escaped--table /* nasty */\";"
  , OtherSqlPiece "CREATE TABLE any_table();"
  , OtherSqlPiece
    $  "CREATE FUNCTION sales_tax(subtotal real) RETURNS real AS $$"
    <> "\nBEGIN"
    <> "\n    RETURN subtotal * 0.06;"
    <> "\nEND;"
    <> "\n$$ LANGUAGE plpgsql;"
  , OtherSqlPiece
    $  "CREATE FUNCTION instr(varchar, integer) RETURNS integer AS $$"
    <> "\nDECLARE"
    <> "\n    v_string ALIAS FOR $1;"
    <> "\n    index ALIAS FOR $2;"
    <> "\nBEGIN"
    <> "\n    -- some computations using v_string and index here"
    <> "\nEND;"
    <> "\n$$ LANGUAGE plpgsql;"
  , OtherSqlPiece
    "select U&'d\\0061t\\+000061', U&'\\0441\\043B\\043E\\043D', U&'d!0061t!+000061' UESCAPE '!', X'1FF', B'1001';"
  , OtherSqlPiece "SELECT 'some''quoted ''string';"
  , OtherSqlPiece "SELECT \"some\"\"quoted identifier\";"
  , OtherSqlPiece "SELECT 'double quotes \" inside single quotes \" - 2';"
  , OtherSqlPiece "SELECT \"single quotes ' inside double quotes ' - 2\";"
  , OtherSqlPiece
    $  "$function$"
    <> "\nBEGIN"
    <> "\n    RETURN ($1 ~ $q$[\t\r\n\v\\]$q$);"
    <> "\nEND;"
    <> "\n$function$;"
  , OtherSqlPiece "SELECT COALESCE(4, 1 - 2) - 3 + 4 - 5;"
  , OtherSqlPiece "SELECT (1 - 4) / 5 * 3 / 9.1;"
  , CopyFromStdinPiece "COPY employee FROM STDIN WITH (FORMAT CSV);\n"
                       "5,Dracula,master\n6,The Grinch,master"
                       "\n\\.\n"
  , CopyFromStdinPiece "COPY employee FROM STDIN WITH (FORMAT CSV);\n"
                       ""
                       "\\.\n"
  , -- Empty COPY is possible
    CopyFromStdinPiece
    "copy \"schema\".employee FROM stdin WITH (FORMAT CSV);\n"
    "DATA"
    "\n\\.\n"
  , -- Fully qualified identifiers part 1
    CopyFromStdinPiece
    "CoPy \"some-database\"   .  \"schema\"  .  employee from stdin with (FORMAT CSV);\n"
    ""
    "\\.\n"
  , -- Fully qualified identifiers part 2
    CopyFromStdinPiece
    "CoPy \"employee\"   (col1,\"col2\"   ,   col4  ) from stdin with (FORMAT CSV);\n"
    "Lots of data\nin\neach line"
    "\n\\.\n" -- Specifying columns
  , BeginTransaction "begin;"
  , BeginTransaction "BEGiN/*a*/;"
  , BeginTransaction "BEgIN   ;"
  , RollbackTransaction "ROllBaCk;"
  , RollbackTransaction "ROllBaCk/*a*/;"
  , RollbackTransaction "ROllBaCk   ;"
  , CommitTransaction "COmmIT;"
  , CommitTransaction "COMMIT/*a*/;"
  , CommitTransaction "cOMMIT   ;"
    -- TODO: Nested C-Style comments (https://www.postgresql.org/docs/9.2/sql-syntax-lexical.html)
    -- , "/* multiline comment"
    --   <> "\n  * with nesting: /* nested block comment */"
    --   <> "\n  */ SELECT 1;"
  ]

genSql :: Bool -> Gen Text
genSql onlySyntacticallyValid = frequency
  [(if onlySyntacticallyValid then 0 else 1, pure ""), (50, randomSqlGen)]
 where
  emptyLineGen   = pure "\n"
  bizarreLineGen = (<> "\n") . Text.pack . getUnicodeString <$> arbitrary
  lineGen        = frequency
    [ (if onlySyntacticallyValid then 0 else 1, bizarreLineGen)
    , (1, emptyLineGen)
    , (5, sqlPieceText <$> genSingleSqlStatement)
    ]
  -- Note: the likelihood that QuickCheck will randomly generate text that has a line starting with "-- codd:"
  -- is so low that we can just ignore it
  dashDashCommentGen =
    (<> "\n") . ("-- " <>) . (Text.replace "\n" "") <$> bizarreLineGen
  cStyleCommentGen =
    ("/*" <>) . (<> "*/") . (Text.replace "*/" "") <$> bizarreLineGen
  commentGen       = frequency [(5, dashDashCommentGen), (1, cStyleCommentGen)]
  lineOrCommentGen = frequency [(5, lineGen), (1, commentGen)]
  randomSqlGen     = fmap Text.concat $ do
    l1             <- listOf lineOrCommentGen
    l2             <- listOf lineOrCommentGen
    atLeastOneStmt <- sqlPieceText <$> genSingleSqlStatement
    let finalList = if onlySyntacticallyValid
          then l1 ++ (atLeastOneStmt : l2)
          else l1 ++ l2

        mapLast :: (a -> a) -> [a] -> [a]
        mapLast _ []       = []
        mapLast f [x     ] = [f x]
        mapLast f (x : xs) = x : mapLast f xs

    -- Optionally remove semi-colon from the last command if it ends with one
    removeLastSemiColon <- arbitrary
    pure $ if removeLastSemiColon
      then mapLast (\t -> fromMaybe t (Text.stripSuffix ";" t)) finalList
      else finalList

instance Arbitrary RandomSql where
  arbitrary = fmap RandomSql (genSql False)

instance Arbitrary SyntacticallyValidRandomSql where
  arbitrary = fmap SyntacticallyValidRandomSql (genSql True)

spec :: Spec
spec = do
  describe "Parsing tests" $ do
    context "Multi Query Statement Parser" $ do
      it "Single command with and without semi-colon" $ do
        let eblks :: Either String [NonEmpty SqlPiece] = traverse
              parseSqlPieces
              [ "CREATE TABLE hello;"
              , "CREATE TABLE hello"
              , "CREATE TABLE hello; -- Comment"
              , "CREATE TABLE hello -- Comment"
              , "CREATE TABLE hello -- Comment\n;"
              ]
        eblks `shouldBe` Right
          [ OtherSqlPiece "CREATE TABLE hello;" :| []
          , OtherSqlPiece "CREATE TABLE hello" :| []
          , OtherSqlPiece "CREATE TABLE hello;"
            :| [WhiteSpacePiece " ", CommentPiece "-- Comment"]
          , OtherSqlPiece "CREATE TABLE hello -- Comment" :| []
          , OtherSqlPiece "CREATE TABLE hello -- Comment\n;" :| []
          ]
      it "Statement separation boundaries are good"
        $ forAll (listOf1 genSingleSqlStatement)
        $ \blocks -> do
            let estm = parseSqlPieces $ Text.concat (map sqlPieceText blocks)
            NE.toList <$> estm `shouldBe` Right blocks
      it
          "Statements concatenation matches original and statements end with semi-colon"
        $ do
            property $ \(unSyntRandomSql -> plainSql) -> do
              let eblks = parseSqlPieces plainSql
              -- putStrLn "--------------------------------------------------"
              -- print plainSql
              case eblks of
                Left  e    -> eblks `shouldNotSatisfy` isLeft
                Right blks -> do
                  let comments = [ t | CommentPiece t <- NE.toList blks ]
                      whtspc   = [ t | WhiteSpacePiece t <- NE.toList blks ]
                  piecesToText blks `shouldBe` plainSql
                  forM_ blks $ \case
                    CommentPiece t ->
                      t
                        `shouldSatisfy` (\c ->
                                          "--"
                                            `Text.isPrefixOf` c
                                            ||                "/*"
                                            `Text.isPrefixOf` c
                                            &&                "*/"
                                            `Text.isSuffixOf` c
                                        )
                    WhiteSpacePiece t ->
                      t `shouldSatisfy` (\c -> Text.strip c == "")
                    CopyFromStdinPiece c d ll ->
                      ll
                        `shouldSatisfy` (\l ->
                                          d
                                            /= ""
                                            && (  l
                                               == "\n\\.\n"
                                               || l
                                               == "\r\n\\.\r\n"
                                               )
                                            || d
                                            == ""
                                            && (l == "\\.\n" || l == "\\.\r\n")
                                        )
                    _ -> pure ()

    context "Simple mode" $ do
      context "Valid SQL Migrations" $ do
        it "Plain Sql Migration, missing optional options" $ do
          property $ \(unSyntRandomSql -> plainSql) -> do
            let parsedMig = parseSqlMigration "any-name.sql" plainSql
            parsedMig `shouldBe` Right SqlMigration
              { migrationName           = "any-name.sql"
              , migrationSql            = Just $ mkValidSql plainSql
              , migrationInTxn          = True
              , migrationCustomConnInfo = Nothing
              }
        it "Sql Migration options parsed correctly"
          $ let sql = "-- codd: no-txn\nSOME SQL"
            in  parseSqlMigration "any-name.sql" sql `shouldBe` Right
                  SqlMigration { migrationName           = "any-name.sql"
                               , migrationSql            = Just $ mkValidSql sql
                               , migrationInTxn          = False
                               , migrationCustomConnInfo = Nothing
                               }

        it "Sql Migration connection and custom options"
          $ property
          $ \(ConnStringGen connStr connInfo) -> do
              let
                sql1 =
                  "\n-- codd: no-txn\n"
                    <> "\n\n-- codd-connection: "
                    <> connStr
                    <> "\n\nSOME SQL"
                sql2 =
                  "\n\n-- codd-connection: "
                    <> connStr
                    <> "\n-- codd: in-txn\n"
                    <> "SOME SQL"
              parseSqlMigration "any-name.sql" sql1 `shouldBe` Right
                SqlMigration { migrationName           = "any-name.sql"
                             , migrationSql            = Just $ mkValidSql sql1
                             , migrationInTxn          = False
                             , migrationCustomConnInfo = Just connInfo
                             }

              parseSqlMigration "any-name.sql" sql2 `shouldBe` Right
                SqlMigration { migrationName           = "any-name.sql"
                             , migrationSql            = Just $ mkValidSql sql2
                             , migrationInTxn          = True
                             , migrationCustomConnInfo = Just connInfo
                             }

        it "Sql Migration connection option alone"
          $ property
          $ \(ConnStringGen connStr connInfo) ->
              let
                sql =
                  "-- random comment\n-- codd-connection: "
                    <> connStr
                    <> "\nSOME SQL"
              in  parseSqlMigration "any-name.sql" sql `shouldBe` Right
                    SqlMigration { migrationName           = "any-name.sql"
                                 , migrationSql = Just $ mkValidSql sql
                                 , migrationInTxn          = True
                                 , migrationCustomConnInfo = Just connInfo
                                 }


        it "in-txn and no-txn are mutually exclusive"
          $ let plainSql = "SOME SQL"
                sql      = "-- codd: no-txn, in-txn\n" <> plainSql
            in  parseSqlMigration "any-name.sql" sql `shouldSatisfy` isLeft

    context "Invalid SQL Migrations" $ do
      it "Sql Migration Parser never blocks for random text" $ do
        property $ \(unRandomSql -> anyText) -> do
          parseSqlMigration "any-name.sql" anyText
            `shouldSatisfy` \p -> seq p True

      it "Gibberish after -- codd:"
        $ let sql = "-- codd: complete gibberish\n" <> "ANY SQL HERE"
          in
            parseSqlMigration "any-name.sql" sql `shouldSatisfy` \(Left err) ->
                                                                      -- Error message is specific about what is wrong
              "complete gibberish" `Text.isInfixOf` err

      it "Duplicate options"
        $ let sql = "-- codd: in-txn, in-txn\n" <> "ANY SQL HERE"
          in  parseSqlMigration "any-name.sql" sql `shouldSatisfy` \case
                Right _   -> False
                Left  err -> "duplicate" `Text.isInfixOf` err

      it "Unknown / mistyped options"
        $ let sql = "-- codd: unknown-txn\n" <> "ANY SQL HERE"
          in  parseSqlMigration "any-name.sql" sql `shouldSatisfy` isLeft

      it "Mistyped connection string option"
        $ let sql = "-- codd-connection: blah\n" <> "ANY SQL HERE"
          in
            parseSqlMigration "any-name.sql" sql
              `shouldSatisfy` \(Left err) ->
                                                -- Nice error message explaining valid format
                                             "postgres://" `Text.isInfixOf` err

      it "Two connection strings"
        $ let
            sql =
              "-- codd-connection: postgres://codd_admin@127.0.0.1:5433/codd-experiments\n"
                <> "-- codd-connection: postgres://codd_admin@127.0.0.1:5433/codd-experiments\n"
                <> "ANY SQL HERE"
          in  parseSqlMigration "any-name.sql" sql `shouldSatisfy` isLeft

      it "Two -- codd comments"
        $ let sql = "-- codd: in-txn\n--codd: in-txn\n" <> "MORE SQL HERE"
          in  parseSqlMigration "any-name.sql" sql `shouldSatisfy` isLeft

      it "--no-parse forcefully returns a SqlMigration"
        $          parseSqlMigrationOpts NoParse
                                         "failed-parsing-migration.sql"
                                         "-- SQL with comments only!"
        `shouldBe` Right SqlMigration
                     { migrationName           = "failed-parsing-migration.sql"
                     , migrationSql            = Just
                       $ ParseFailSqlText "-- SQL with comments only!"
                     , migrationInTxn          = True
                     , migrationCustomConnInfo = Nothing
                     }

      it "SAVEPOINTs need to be released or rolled back inside SQL migrations"
        $ pendingWith
            "Testing this by selecting txid_current() might be more effective"

    context "Other important behaviours to test" $ do
      it "Empty queries detector works well" $ do
        let
          emptyQueries =
            [ ""
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
          nonEmptyQueries =
            [ "      --Some comment    \n-- Some other comment\n\n\n Some SQL Command      "
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
        forM_ emptyQueries
          $ \q -> (q, nothingIfEmptyQuery q) `shouldBe` (q, Nothing)
        forM_ (nonEmptyQueries ++ map sqlPieceText validSqlStatements) $ \q ->
          nothingIfEmptyQuery q `shouldSatisfy` \case
            Just (WellParsedSql _ _) -> True
            _                        -> False
