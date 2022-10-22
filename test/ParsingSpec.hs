{-# LANGUAGE DuplicateRecordFields #-}
module ParsingSpec where

import           Codd.Internal                  ( BlockOfMigrations(..)
                                                , parseMigrationFiles
                                                )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , EnvVars(..)
                                                , ParsedSql(..)
                                                , PureStream(..)
                                                , SqlMigration(..)
                                                , SqlPiece(..)
                                                , isCommentPiece
                                                , isWhiteSpacePiece
                                                , parseAndClassifyMigration
                                                , parseSqlMigration
                                                , parseSqlPiecesStreaming
                                                , parsedSqlText
                                                , piecesToText
                                                , sqlPieceText
                                                )
import           Control.Monad                  ( (>=>)
                                                , forM_
                                                , when
                                                )
import           Control.Monad.Identity         ( Identity(runIdentity) )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , ask
                                                )
import           Control.Monad.Trans            ( MonadTrans )
import           Control.Monad.Trans.Resource   ( MonadThrow(..) )
import qualified Data.Char                     as Char
import           Data.Either                    ( isLeft )
import qualified Data.List                     as List
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NE
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple    as SQL
import           DbUtils                        ( mkValidSql
                                                , parseSqlMigrationIO
                                                )
import           EnvironmentSpec                ( ConnStringGen(..) )
import           Streaming                      ( Of
                                                , Stream
                                                )
import qualified Streaming.Prelude             as Streaming
import           System.Random                  ( mkStdGen
                                                , randomR
                                                )
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSuccess )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck
import           UnliftIO                       ( MonadIO
                                                , liftIO
                                                )
import           UnliftIO.Resource              ( runResourceT )

data RandomSql m = RandomSql
  { unRandomSql  :: PureStream m
  , fullContents :: Text
  }
instance Show (RandomSql m) where
  show RandomSql { fullContents } = show fullContents

-- | Syntactically valid SQL must contain at least one statement!
data SyntacticallyValidRandomSql m = SyntacticallyValidRandomSql
  { unSyntRandomSql :: PureStream m
  , fullContents    :: Text
  }
instance Show (SyntacticallyValidRandomSql m) where
  show SyntacticallyValidRandomSql { fullContents } = show fullContents

-- | This is a list because it can be a sequence of `CopyFromStdinStatement`+`CopyFromStdinRow`+`CopyFromStdinEnd`.
-- For all other cases it shall be a single statement.
genSingleSqlStatement :: Gen [SqlPiece]
genSingleSqlStatement = elements validSqlStatements

-- | Sql statements that can be interleaved in any form and should still form syntactically valid SQL.
-- The inner lists contain statements that _must_ be put consecutively.
validSqlStatements :: [[SqlPiece]]
validSqlStatements =
  map
      (: [])
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
    ++ [ [ CopyFromStdinStatement
           "COPY employee FROM STDIN WITH (FORMAT CSV);\n"
         , CopyFromStdinRow "5,Dracula,master\n"
         , CopyFromStdinRow "6,The Grinch,master\n"
         , CopyFromStdinEnd "\\.\n"
         ]
       , [ CopyFromStdinStatement
           "COPY employee FROM STDIN WITH (FORMAT CSV);\n"
    -- COPY without any rows is syntactically valid
         , CopyFromStdinEnd "\\.\n"
         ]
       , [ CopyFromStdinStatement
           "copy \"schema\".employee FROM stdin WITH (FORMAT CSV);\n"
         , CopyFromStdinRow "DATA\n"
         , CopyFromStdinEnd "\\.\n"
         ]
       , [
     -- Fully qualified identifiers part 1 + table without columns, but with one row (this is possible!)
           CopyFromStdinStatement
           "CoPy \"some-database\"   .  \"schema\"  .  employee from stdin with (FORMAT CSV);\n"
         , CopyFromStdinRow "\n"
         , CopyFromStdinEnd "\\.\n"
         ]
       , [
    -- Fully qualified identifiers part 2 + specifying columns
           CopyFromStdinStatement
           "CoPy \"employee\"   (col1,\"col2\"   ,   col4  ) from stdin with (FORMAT CSV);\n"
         , CopyFromStdinRow "Lots of data\n"
         , CopyFromStdinRow "in\n"
         , CopyFromStdinRow "each line\n"
         , CopyFromStdinEnd "\\.\n"
         ]
       ,
         -- COPY with lots of custom escaping
         [ CopyFromStdinStatement
           "COPY whatever FROM STDIN WITH (FORMAT CSV);\n"
         , CopyFromStdinRow "\\b \\0 \\x1 \\t \\v \\r \\n \\f \\b \n"
         , CopyFromStdinRow "\b \0 \r \t\n" -- I think these characters are actually invalid in COPY according to postgres.
         , CopyFromStdinEnd "\\.\n"
         ]
       ]

genTextStream :: Monad m => Text -> Gen (PureStream m)
genTextStream t = do
  n <- arbitrary
  pure $ mkRandStream n t

mkRandStream :: Monad m => Int -> Text -> PureStream m
mkRandStream seed t = PureStream $ go (mkStdGen seed) t
 where
  go g t =
    let (n, g') = randomR (0, len) g
        remainder =
          if t == "" then Streaming.yield "" else go g' (Text.drop n t)
    in  Streaming.yield (Text.take n t) <> remainder
    where len = Text.length t

genSql :: Monad m => Bool -> Gen (PureStream m, Text)
genSql onlySyntacticallyValid = do
  t <- frequency
    [(if onlySyntacticallyValid then 0 else 1, pure ""), (50, randomSqlGen)]
  (, t) <$> genTextStream t
 where
  emptyLineGen   = pure "\n"
  bizarreLineGen = (<> "\n") . Text.pack . getUnicodeString <$> arbitrary
  lineGen        = frequency
    [ (if onlySyntacticallyValid then 0 else 1, bizarreLineGen)
    , (1, emptyLineGen)
    , (5, piecesToText <$> genSingleSqlStatement)
    ]
  -- Note: the likelihood that QuickCheck will randomly generate text that has a line starting with "-- codd:"
  -- is so low that we can just ignore it
  dashDashCommentGen =
    (<> "\n") . ("-- " <>) . Text.replace "\n" "" <$> bizarreLineGen
  cStyleCommentGen =
    ("/*" <>) . (<> "*/") . Text.replace "*/" "" <$> bizarreLineGen
  commentGen       = frequency [(5, dashDashCommentGen), (1, cStyleCommentGen)]
  lineOrCommentGen = frequency [(5, lineGen), (1, commentGen)]
  randomSqlGen     = fmap Text.concat $ do
    l1             <- listOf lineOrCommentGen
    l2             <- listOf lineOrCommentGen
    atLeastOneStmt <- piecesToText <$> genSingleSqlStatement
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

instance Monad m => Arbitrary (RandomSql m) where
  arbitrary = uncurry RandomSql <$> genSql False

instance Monad m => Arbitrary (SyntacticallyValidRandomSql m) where
  arbitrary = uncurry SyntacticallyValidRandomSql <$> genSql True

shouldHaveWellParsedSql :: MonadIO m => SqlMigration m -> Text -> m ()
mig `shouldHaveWellParsedSql` sql = case migrationSql mig of
  UnparsedSql _        -> liftIO $ expectationFailure "Got UnparsedSql"
  ps@(WellParsedSql _) -> do
    psql <- parsedSqlText ps
    liftIO $ psql `shouldBe` sql

shouldHaveUnparsedSql :: MonadIO m => SqlMigration m -> Text -> m ()
shouldHaveUnparsedSql mig expectedSql = case migrationSql mig of
  WellParsedSql _   -> liftIO $ expectationFailure "Got WellParsedSql instead"
  UnparsedSql   sql -> liftIO $ sql `shouldBe` expectedSql

-- | Same as a monadic `shouldSatisfy` isLeft, but does not require a Show instance.
shouldReturnLeft :: MonadIO m => m (Either a b) -> m ()
shouldReturnLeft mv = mv >>= \case
  Left  _ -> pure ()
  Right _ -> liftIO $ expectationFailure "Got Right but was expecting Left"


{- A quick-n-dirty transformer to serve as a mock of the EnvVars monad -}
newtype EnvVarsT m a = EnvVarsT { runEnvVarsT :: Map Text Text -> m a }
  deriving stock (Functor)

-- | This isn't really overlapping. The compiler says so probably
-- because instance resolution doesn't check constraints.
instance {-# OVERLAPPING #-} EnvVars (EnvVarsT Identity) where
  -- Important invariant: 
  -- All asked-for environment variables must appear in the result Map.
  getEnvVars vars = EnvVarsT $ \r -> pure $ foldr
    (\k res -> let v = Map.findWithDefault "" k r in Map.insert k v res)
    mempty
    vars

instance Monad m => MonadThrow (EnvVarsT m) where
  throwM = error "throwM not implemented"

instance Applicative m => Applicative (EnvVarsT m) where
  pure v = EnvVarsT $ const (pure v)
  EnvVarsT run1 <*> EnvVarsT run2 = EnvVarsT $ \r -> run1 r <*> run2 r
{- ------------------------------------------------------------------- -}

instance Monad m => Monad (EnvVarsT m) where
  EnvVarsT run1 >>= f = EnvVarsT $ \r -> do
    v <- run1 r
    let EnvVarsT run2 = f v
    run2 r

spec :: Spec
spec = do
  describe "Parsing tests" $ do
    context "Multi Query Statement Parser" $ do
      it "Single command with and without semi-colon"
        $ property
        $ \randomSeed -> do
            blks :: [[SqlPiece]] <- traverse
              ( (Streaming.toList_ . parseSqlPiecesStreaming)
              . unPureStream
              . mkRandStream randomSeed
              )
              [ "CREATE TABLE hello;"
              , "CREATE TABLE hello"
              , "CREATE TABLE hello; -- Comment"
              , "CREATE TABLE hello -- Comment"
              , "CREATE TABLE hello -- Comment\n;"
              ]
            blks
              `shouldBe` [ [OtherSqlPiece "CREATE TABLE hello;"]
                         , [OtherSqlPiece "CREATE TABLE hello"]
                         , [ OtherSqlPiece "CREATE TABLE hello;"
                           , WhiteSpacePiece " "
                           , CommentPiece "-- Comment"
                           ]
                         , [OtherSqlPiece "CREATE TABLE hello -- Comment"]
                         , [OtherSqlPiece "CREATE TABLE hello -- Comment\n;"]
                         ]
      it "Statement separation boundaries are good"
        $ forAll ((,) <$> listOf1 genSingleSqlStatement <*> arbitrary @Int)
        $ \(origPieces, randomSeed) -> do
            parsedPieces <-
              Streaming.toList_
              $ parseSqlPiecesStreaming
              $ unPureStream
              $ mkRandStream randomSeed
              $ Text.concat (map piecesToText origPieces)
            parsedPieces `shouldBe` mconcat origPieces
      it
          "Statements concatenation matches original and statements end with semi-colon"
        $ do
            property $ \SyntacticallyValidRandomSql {..} -> do
              blks <- Streaming.toList_ $ parseSqlPiecesStreaming $ unPureStream
                unSyntRandomSql
              let comments = [ t | CommentPiece t <- blks ]
                  whtspc   = [ t | WhiteSpacePiece t <- blks ]
              piecesToText blks `shouldBe` fullContents
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
                CopyFromStdinEnd ll -> ll `shouldBe` "\\.\n"
                _                   -> pure ()

    context "Valid SQL Migrations" $ do
      it "Plain Sql Migration, missing optional options"
        $ property @(SyntacticallyValidRandomSql IO -> IO ())
        $ \SyntacticallyValidRandomSql {..} -> do
            Right parsedMig <- parseSqlMigration "any-name.sql" unSyntRandomSql
            migrationName parsedMig `shouldBe` "any-name.sql"
            parsedMig `shouldHaveWellParsedSql` fullContents
            migrationInTxn parsedMig `shouldBe` True
            migrationCustomConnInfo parsedMig `shouldBe` Nothing
      it "Sql Migration options parsed correctly" $ property $ \randomSeed -> do
        let sql = "-- codd: no-txn\nSOME SQL"
        Right parsedMig <- parseSqlMigrationIO "any-name.sql"
          $ mkRandStream randomSeed sql
        migrationName parsedMig `shouldBe` "any-name.sql"
        parsedMig `shouldHaveWellParsedSql` sql
        migrationInTxn parsedMig `shouldBe` False
        migrationCustomConnInfo parsedMig `shouldBe` Nothing

      it "Sql Migration connection and custom options"
        $ property
        $ \(ConnStringGen connStr connInfo, randomSeed) -> do
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
            parsedMig1 <- either (error "Oh no") id
              <$> parseSqlMigrationIO
                    "any-name.sql"
                    (mkRandStream randomSeed sql1)
            migrationName parsedMig1 `shouldBe` "any-name.sql"
            parsedMig1 `shouldHaveWellParsedSql` sql1
            migrationInTxn parsedMig1 `shouldBe` False
            migrationCustomConnInfo parsedMig1 `shouldBe` Just connInfo

            parsedMig2 <- either (error "Oh nooo") id
              <$> parseSqlMigrationIO
                    "any-name.sql"
                    (mkRandStream randomSeed sql2)
            migrationName parsedMig2 `shouldBe` "any-name.sql"
            parsedMig2 `shouldHaveWellParsedSql` sql2
            migrationInTxn parsedMig2 `shouldBe` True
            migrationCustomConnInfo parsedMig2 `shouldBe` Just connInfo

      it "Sql Migration connection option alone"
        $ property
        $ \(ConnStringGen connStr connInfo, randomSeed) -> do
            let
              sql =
                "-- random comment\n-- codd-connection: "
                  <> connStr
                  <> "\nSOME SQL"
            mig <- either (error "Oh no") id
              <$> parseSqlMigrationIO
                    "any-name.sql"
                    (mkRandStream randomSeed sql)
            migrationName mig `shouldBe` "any-name.sql"
            mig `shouldHaveWellParsedSql` sql
            migrationInTxn mig `shouldBe` True
            migrationCustomConnInfo mig `shouldBe` Just connInfo

      it "Sql Migration parsed from disk with full contents"
        $ runResourceT @IO
        $ runStdoutLoggingT
        $ do
            [BlockOfMigrations { allMigs = asqlmig :| [] }] <-
              parseMigrationFiles []
                $ Left ["test/migrations/normal-parse-test/"]
            rawFileContents <-
              liftIO
                $ Text.readFile
                    "test/migrations/normal-parse-test/2000-01-01-00-00-00-normal-parse-migration.sql"
            let (AddedSqlMigration mig _) = asqlmig
            mig `shouldHaveWellParsedSql` rawFileContents

      it "--codd: no-parse returns UnparsedSql"
        $ property
        $ \(RandomSql { unRandomSql, fullContents }, randomSeed) -> do
            let coddOpts = "-- codd: no-parse\n"
                sql      = coddOpts <> fullContents

            mig <- either (error "Oops") id <$> parseSqlMigrationIO
              "no-parse-migration.sql"
              (mkRandStream randomSeed coddOpts <> unRandomSql)
            migrationName mig `shouldBe` "no-parse-migration.sql"
            mig `shouldHaveUnparsedSql` sql
            migrationInTxn mig `shouldBe` True
            migrationCustomConnInfo mig `shouldBe` Nothing

      it "--codd: no-parse preserves all of migrations file contents"
        $ runResourceT @IO
        $ runStdoutLoggingT
        $ do
            [BlockOfMigrations { allMigs = asqlmig :| [] }] <-
              parseMigrationFiles [] $ Left ["test/migrations/no-parse-test/"]
            rawFileContents <-
              liftIO
                $ Text.readFile
                    "test/migrations/no-parse-test/2000-01-01-00-00-00-no-parse-migration.sql"
            let (AddedSqlMigration mig _) = asqlmig
            mig `shouldHaveUnparsedSql` rawFileContents

      it "--codd: no-parse with custom connection-string"
        $ property
        $ \randomSeed -> do
            let
              sql
                = "-- codd: no-parse\n\
            \-- codd-connection: user=codd_admin dbname='codd-experiments' host=127.0.0.1 port=5433\n\
            \-- Anything here!\
            \Some statement; -- Some comment\n\
            \Other statement"

            mig <- either (error "Oops") id
              <$> parseSqlMigrationIO
                    "no-parse-migration.sql"
                    (mkRandStream randomSeed sql)
            migrationName mig `shouldBe` "no-parse-migration.sql"
            mig `shouldHaveUnparsedSql` sql
            migrationInTxn mig `shouldBe` True
            migrationCustomConnInfo mig `shouldBe` Just DB.ConnectInfo
              { DB.connectUser     = "codd_admin"
              , DB.connectHost     = "127.0.0.1"
              , DB.connectPort     = 5433
              , DB.connectPassword = ""
              , DB.connectDatabase = "codd-experiments"
              }

      it "-- codd-env-vars templating" $ do
        let
          sqlWithVars
            = "-- codd: no-txn\n\
                  \-- codd-connection: postgres://${PGSUPERUSER}@${PGHOST}:${PGPORT}/postgres\n\
                  \-- codd-env-vars: PGSUPERUSER ,PGHOST,PGUSER, PGPORT  , UNDEFINED_VAR23, PGDATABASE\n\
                  \CREATE DATABASE ${PGDATABASE};\n\
                  \SELECT \"${PGUSER}\";\n\
                  \SELECT ${NOTAVARIABLE};\n\
                  \SELECT ${UNDEFINED_VAR23}"
          vars = Map.fromList
            [ ("PGSUPERUSER", "dbsuperuser")
            , ("PGHOST"     , "someaddress")
            , ("PGPORT"     , "5432")
            , ("PGUSER"     , "codd_user")
            , ("PGDATABASE" , "codd_db")
            ]
          templatedSql
            = "-- codd: no-txn\n\
                  \-- codd-connection: postgres://dbsuperuser@someaddress:5432/postgres\n\
                  \-- codd-env-vars: PGSUPERUSER ,PGHOST,PGUSER, PGPORT  , UNDEFINED_VAR23, PGDATABASE\n\
                  \CREATE DATABASE codd_db;\n\
                  \SELECT \"codd_user\";\n\
                  \SELECT ${NOTAVARIABLE};\n\
                  \SELECT " -- Declared in the header but not defined is replaced by empty

        -- EnvVarsT over Identity is our way to mock environment variables
        let parsedSql = runIdentity $ flip runEnvVarsT vars $ do
              res <-
                parseAndClassifyMigration
                $ PureStream @(EnvVarsT Identity)
                $ Streaming.yield sqlWithVars
              case res of
                Left  err                   -> error err
                Right (_, _, UnparsedSql _) -> error "Got UnparsedSql"
                Right (_, _, WellParsedSql pieces) ->
                  mconcat . map sqlPieceText <$> Streaming.toList_ pieces
        parsedSql `shouldBe` templatedSql

    context "Invalid SQL Migrations" $ do
      it "Sql Migration Parser never blocks for random text" $ do
        property $ \RandomSql { unRandomSql, fullContents } -> do
          emig <- parseSqlMigrationIO "any-name.sql" unRandomSql
          case migrationSql <$> emig of
            Left  _                      -> error "Should not be Left!"
            Right (WellParsedSql pieces) -> do
              t <- piecesToText <$> Streaming.toList_ pieces
              t `shouldBe` fullContents
            Right (UnparsedSql t) -> t `shouldBe` fullContents

      it "in-txn and no-txn are mutually exclusive" $ property $ \randomSeed ->
        do
          let plainSql = "SOME SQL"
              sql      = "-- codd: no-txn, in-txn\n" <> plainSql
          shouldReturnLeft $ parseSqlMigrationIO "any-name.sql" $ mkRandStream
            randomSeed
            sql

      it "Gibberish after -- codd:" $ property $ \randomSeed -> do
        let sql = "-- codd: complete gibberish\n" <> "ANY SQL HERE"
        mig <- parseSqlMigrationIO "any-name.sql" $ mkRandStream randomSeed sql
        case mig of
          Left err ->
            err `shouldSatisfy`
                          -- Error message is specific about what is wrong
                                List.isInfixOf "complete gibberish"
          Right _ -> expectationFailure "Got Right"

      it "Duplicate options" $ property $ \randomSeed -> do
        let sql = "-- codd: in-txn, in-txn\n" <> "ANY SQL HERE"
        mig <- parseSqlMigrationIO "any-name.sql" $ mkRandStream randomSeed sql
        case mig of
          Right _   -> expectationFailure "Got Right"
          Left  err -> err `shouldSatisfy` List.isInfixOf "duplicate"

      it "Unknown / mistyped options" $ property $ \randomSeed -> do
        let sql = "-- codd: unknown-txn\n" <> "ANY SQL HERE"
        shouldReturnLeft $ parseSqlMigrationIO "any-name.sql" $ mkRandStream
          randomSeed
          sql

      it "Mistyped connection string option" $ property $ \randomSeed -> do
        let sql = "-- codd-connection: blah\n" <> "ANY SQL HERE"
        mig <- parseSqlMigrationIO "any-name.sql" $ mkRandStream randomSeed sql
        case mig of
          Left err ->
            -- Nice error message explaining valid format
            err `shouldSatisfy` ("postgresql.org" `List.isInfixOf`)
          Right _ -> expectationFailure "Got Right"

      it "Two connection strings" $ property $ \randomSeed -> do
        let
          sql =
            "-- codd-connection: postgres://codd_admin@127.0.0.1:5433/codd-experiments\n"
              <> "-- codd-connection: postgres://codd_admin@127.0.0.1:5433/codd-experiments\n"
              <> "ANY SQL HERE"
        shouldReturnLeft $ parseSqlMigrationIO "any-name.sql" $ mkRandStream
          randomSeed
          sql

      it "Two -- codd comments" $ property $ \randomSeed -> do
        let sql = "-- codd: in-txn\n--codd: in-txn\n" <> "MORE SQL HERE"
        shouldReturnLeft $ parseSqlMigrationIO "any-name.sql" $ mkRandStream
          randomSeed
          sql

      it "--codd: no-parse does not mix with -- codd: no-txn"
        $ property
        $ \randomSeed -> do
            let sql = "-- codd: no-parse\n-- codd: no-txn\nSome statement"
            shouldReturnLeft
              $ parseSqlMigrationIO "no-parse-migration.sql"
              $ mkRandStream randomSeed sql

      it "SAVEPOINTs need to be released or rolled back inside SQL migrations"
        $ pendingWith
            "Testing this by selecting txid_current() might be more effective"

    context "Other important behaviours to test" $ do
      it "Empty queries detector works well" $ property $ \randomSeed -> do
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
        forM_ emptyQueries $ \q -> do
          let
            parsed =
              parseSqlPiecesStreaming $ unPureStream $ mkRandStream randomSeed q
          Streaming.all_ (\p -> isWhiteSpacePiece p || isCommentPiece p) parsed
            `shouldReturn` True
        forM_ (nonEmptyQueries ++ map piecesToText validSqlStatements) $ \q ->
          do
            let parsed = parseSqlPiecesStreaming $ unPureStream $ mkRandStream
                  (randomSeed + 1)
                  q
            Streaming.any_
                (\p -> not (isWhiteSpacePiece p || isCommentPiece p))
                parsed
              `shouldReturn` True
