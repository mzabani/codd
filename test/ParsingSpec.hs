{-# LANGUAGE DuplicateRecordFields #-}

module ParsingSpec where

import Codd.Internal
  ( BlockOfMigrations (..),
    ConsecutiveInTxnMigrations (..),
    parseMigrationFiles,
  )
import Codd.Internal.MultiQueryStatement
  ( forceStreamConcurrently,
    forceStreamConcurrentlyInspect,
    skipCountableRunnableStatements,
  )
import Codd.Logging (runCoddLogger)
import Codd.Parsing (AddedSqlMigration (..), EnvVars (..), ParsedSql (..), PureStream (..), SectionOption (..), SqlMigration (..), SqlPiece (..), copyFromStdinAfterStatementParser, isCommentPiece, isWhiteSpacePiece, manyStreaming, parseAndClassifyMigration, parseSqlMigration, parseSqlPiecesStreaming, parseSqlPiecesStreaming', parsedSqlText, piecesToText, sqlPieceText)
import Codd.Types (ConnectionString (..))
import Control.Monad
  ( forM,
    forM_,
    forever,
    guard,
    unless,
    void,
    when,
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Trans.Resource (MonadThrow (..))
import qualified Data.Attoparsec.Text as Parsec
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Database.PostgreSQL.Simple as DB
import DbUtils (parseSqlMigrationIO)
import EnvironmentSpec (ConnStringGen (..), connInfoFromConnStringGen, renderConnStringGen)
import GHC.Num (Natural)
import Streaming (Of (..), Stream)
import qualified Streaming.Internal as S
import qualified Streaming.Prelude as S
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC, performMajorGC)
import System.Random
  ( mkStdGen,
    randomR,
  )
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import UnliftIO
  ( IORef,
    MonadIO,
    evaluate,
    liftIO,
    modifyIORef',
    newEmptyMVar,
    newIORef,
    readIORef,
    readMVar,
  )
import UnliftIO.Async (race, withAsync)
import UnliftIO.Concurrent (MVar, forkIO, threadDelay)
import UnliftIO.Exception (SomeException, try)
import UnliftIO.Resource (runResourceT)

data RandomSql m = RandomSql
  { unRandomSql :: PureStream m,
    fullContents :: Text
  }

instance Show (RandomSql m) where
  show RandomSql {fullContents} = show fullContents

-- | Syntactically valid SQL must contain at least one statement!
data SyntacticallyValidRandomSql m = SyntacticallyValidRandomSql
  { unSyntRandomSql :: PureStream m,
    fullContents :: Text
  }

instance Show (SyntacticallyValidRandomSql m) where
  show SyntacticallyValidRandomSql {fullContents} = show fullContents

-- | This is a list because it can be a sequence of `CopyFromStdinStatement`+`CopyFromStdinRows`+`CopyFromStdinEnd`.
-- For all other cases it shall be a single statement.
genSingleSqlStatement :: Gen [SqlPiece]
genSingleSqlStatement = elements validSqlStatements

-- | Sql statements that can be interleaved in any form and should still form syntactically valid SQL.
-- The inner lists contain statements that _must_ be put consecutively.
validSqlStatements :: [[SqlPiece]]
validSqlStatements =
  map
    (: [])
    [ OtherSqlPiece "SELECT 'so''m -- not a comment' FROM ahahaha;",
      OtherSqlPiece
        "SELECT\nE'so\\'; \\; m -- c-style string, (( not a comment \\\\ \\abc' FROM ahahaha;",
      OtherSqlPiece
        "SELECT \n\n  E'Consecutive single quotes are also escaped here ''; see?'   \n ;",
      OtherSqlPiece "SELECT E'''20s' = E'\\'20s';",
      OtherSqlPiece $
        "DO"
          <> "\n$do$"
          <> "\nBEGIN"
          <> "\n   IF NOT EXISTS ("
          <> "\n      SELECT FROM pg_catalog.pg_roles WHERE rolname = 'codd-user') THEN"
          <> "\n"
          <> "\n      CREATE USER \"codd-user\";"
          <> "\n   END IF;"
          <> "\nEND"
          <> "\n$do$;",
      OtherSqlPiece "CREATE TABLE \"escaped--table /* nasty */\";",
      OtherSqlPiece "CREATE TABLE any_table();",
      OtherSqlPiece $
        "CREATE FUNCTION sales_tax(subtotal real) RETURNS real AS $$"
          <> "\nBEGIN"
          <> "\n    RETURN subtotal * 0.06;"
          <> "\nEND;"
          <> "\n$$ LANGUAGE plpgsql;",
      OtherSqlPiece $
        "CREATE FUNCTION instr(varchar, integer) RETURNS integer AS $$"
          <> "\nDECLARE"
          <> "\n    v_string ALIAS FOR $1;"
          <> "\n    index ALIAS FOR $2;"
          <> "\nBEGIN"
          <> "\n    -- some computations using v_string and index here"
          <> "\nEND;"
          <> "\n$$ LANGUAGE plpgsql;",
      OtherSqlPiece "select U&'d\\0061t\\+000061';",
      OtherSqlPiece "select U&'\\0441\\043B\\043E\\043D';",
      OtherSqlPiece "select U&'d!0061t!+000061' UESCAPE '!';",
      OtherSqlPiece "select U&'d\\0061t\\+000061' UESCAPE '\\';",
      OtherSqlPiece "select X'1FF';",
      OtherSqlPiece "select B'1001';",
      OtherSqlPiece "SELECT 'some''quoted ''string';",
      OtherSqlPiece "SELECT \"some\"\"quoted identifier\";",
      OtherSqlPiece
        "SELECT \n\n 'double quotes \" inside single quotes \" - 2';",
      OtherSqlPiece
        "SELECT \"single quotes ' inside double quotes ' - 2\";",
      OtherSqlPiece $
        "$function$"
          <> "\nBEGIN"
          <> "\n    RETURN ($1 ~ $q$[\t\r\n\v\\]$q$); /* Some would-be non terminated comment, but it's fine inside dollar quotes"
          <> "\nEND;"
          <> "\n$function$;",
      OtherSqlPiece "SELECT COALESCE(4, 1 - 2) - 3 + 4 - 5;",
      OtherSqlPiece "SELECT (1 - 4) / 5 * 3 / 9.1;",
      -- Semi-colons inside parenthesised blocks are not statement boundaries
      OtherSqlPiece
        "create rule \n name as on \n some_event to \n\n some_table do (command1; command2; command3;);",
      -- String blocks can be opened inside parentheses, just like comments, dollar-quoted strings and others
      OtherSqlPiece
        "create rule name as on some_event to some_table do ('abcdef);'; other;)\n\n;",
      OtherSqlPiece
        "some statement with spaces (((U&'d\\0061t\\+000061', 'abc''def' ; ; ; /* comment /* nested */ ((( */ $hey$dollar string$hey$)) 'more''of'; this; ());",
      OtherSqlPiece
        "select 'unclosed parentheses inside string (((((', (('string (('));",
      OtherSqlPiece $
        "CREATE STATISTICS IF NOT EXISTS test_stat_expr"
          <> "\n( dependencies,mcv) ON employee_id, lower(employee_name)"
          <> "\nFROM employee;",
      -- We still want the following to be parsed; it's best to run invalid statements than have
      -- a parser so strict that it might refuse valid ones.
      OtherSqlPiece "invalid statement, bad parentheses ()));",
      BeginTransaction "begin;",
      BeginTransaction "BEGiN/*a*/;",
      BeginTransaction "BEgIN \n  ;",
      RollbackTransaction "ROllBaCk;",
      RollbackTransaction "ROllBaCk/*a*/;",
      RollbackTransaction "ROllBaCk   ;",
      CommitTransaction "COmmIT;",
      CommitTransaction "COMMIT/*a*/;",
      CommitTransaction "cOMMIT   ;",
      -- Nested C-Style comments (https://www.postgresql.org/docs/9.2/sql-syntax-lexical.html)
      CommentPiece "/* multiline comment\n comment */",
      CommentPiece "/* nested block /* comment */ still a comment */"
    ]
    ++ [ [ CopyFromStdinStatement
             "COPY employee FROM STDIN WITH (FORMAT CSV);\n",
           CopyFromStdinRows "5,Dracula,master\n",
           CopyFromStdinRows "6,The Grinch,master\n",
           CopyFromStdinEnd "\\.\n"
         ],
         [ CopyFromStdinStatement
             "COPY employee FROM STDIN WITH (FORMAT CSV);\n",
           -- COPY without any rows is syntactically valid
           CopyFromStdinEnd "\\.\n"
         ],
         [ CopyFromStdinStatement
             "copy \"schema\".employee FROM stdin \n WITH (FORMAT CSV);\n",
           CopyFromStdinRows "DATA\n",
           CopyFromStdinEnd "\\.\n"
         ],
         [ -- Fully qualified identifiers part 1 + table without columns, but with one row (this is possible!)
           CopyFromStdinStatement
             "CoPy \"some-database\"   .  \"schema\"  .  employee from stdin with \n (FORMAT CSV);\n",
           CopyFromStdinRows "\n",
           CopyFromStdinEnd "\\.\n"
         ],
         [ -- Fully qualified identifiers part 2 + specifying columns
           CopyFromStdinStatement
             "CoPy \"employee\" \n  (col1,\"col2\"   , \n\n  col4  ) from stdin with (FORMAT CSV);\n",
           CopyFromStdinRows "Lots of data\n",
           CopyFromStdinRows "in\n",
           CopyFromStdinRows "each line\n",
           CopyFromStdinEnd "\\.\n"
         ],
         -- COPY with lots of custom escaping
         [ CopyFromStdinStatement
             "COPY\nwhatever FROM STDIN WITH (FORMAT CSV);\n",
           CopyFromStdinRows "\\b \\0 \\x1 \\t \\v \\r \\n \\f \\b \n",
           CopyFromStdinRows "\b \0 \r \t\n", -- I think these characters are actually invalid in COPY according to postgres.
           CopyFromStdinEnd "\\.\n"
         ]
       ]

newtype ShuffleOfPieces = ShuffleOfPieces [[SqlPiece]] deriving stock (Show)

instance Arbitrary ShuffleOfPieces where
  arbitrary = ShuffleOfPieces <$> shuffle validSqlStatements

genTextStream :: (Monad m) => Text -> Gen (PureStream m)
genTextStream t = do
  n <- arbitrary
  pure $ mkRandStream n t

-- | Generates a stream that yields chunks of arbitrary sizes according to the supplied seed.
mkRandStream :: (Monad m) => Int -> Text -> PureStream m
mkRandStream seed text = PureStream $ go (mkStdGen seed) text
  where
    go g t =
      let (n, g') = randomR (1, Text.length t) g
          (thisChunk, remainder) = Text.splitAt n t
          -- To make it even harder on the streaming parser, split up chunks where there is white-space
          withEvenMoreSeparation1 = List.intersperse " " $ Text.split (== ' ') thisChunk
          withEvenMoreSeparation2 = concatMap (List.intersperse "\n" . Text.split (== '\n')) withEvenMoreSeparation1
       in if t == ""
            then S.each []
            else
              S.each withEvenMoreSeparation2 <> go g' remainder

genSql :: (Monad m) => Bool -> Gen (PureStream m, Text)
genSql onlySyntacticallyValid = do
  t <-
    frequency
      [(if onlySyntacticallyValid then 0 else 1, pure ""), (50, randomSqlGen)]
  (,t) <$> genTextStream t
  where
    emptyLineGen = pure "\n"
    bizarreLineGen = (<> "\n") . Text.pack . getUnicodeString <$> arbitrary
    lineGen =
      frequency
        [ (if onlySyntacticallyValid then 0 else 1, bizarreLineGen),
          (1, emptyLineGen),
          (5, piecesToText <$> genSingleSqlStatement)
        ]
    -- Note: the likelihood that QuickCheck will randomly generate text that has a line starting with "-- codd:"
    -- is so low that we can just ignore it
    dashDashCommentGen =
      (<> "\n") . ("-- " <>) . Text.replace "\n" "" <$> bizarreLineGen
    cStyleCommentGen =
      ("/*" <>) . (<> "*/") . Text.replace "*/" "" <$> bizarreLineGen
    commentGen = frequency [(5, dashDashCommentGen), (1, cStyleCommentGen)]
    lineOrCommentGen = frequency [(5, lineGen), (1, commentGen)]
    randomSqlGen = fmap Text.concat $ do
      l1 <- listOf lineOrCommentGen
      l2 <- listOf lineOrCommentGen
      atLeastOneStmt <- piecesToText <$> genSingleSqlStatement
      let finalList =
            if onlySyntacticallyValid
              then l1 ++ (atLeastOneStmt : l2)
              else l1 ++ l2

          mapLast :: (a -> a) -> [a] -> [a]
          mapLast _ [] = []
          mapLast f [x] = [f x]
          mapLast f (x : xs) = x : mapLast f xs

      -- Optionally remove semi-colon from the last command if it ends with one
      removeLastSemiColon <- arbitrary
      pure $
        if removeLastSemiColon
          then mapLast (\t -> fromMaybe t (Text.stripSuffix ";" t)) finalList
          else finalList

instance (Monad m) => Arbitrary (RandomSql m) where
  arbitrary = uncurry RandomSql <$> genSql False

instance (Monad m) => Arbitrary (SyntacticallyValidRandomSql m) where
  arbitrary = uncurry SyntacticallyValidRandomSql <$> genSql True

data CopyBody m = CopyBody
  { numLines :: Int,
    chunkSize :: Int,
    terminator :: Text,
    rows :: [Text]
  }
  deriving stock (Show)

instance Arbitrary (CopyBody m) where
  arbitrary = do
    numLines <- chooseInt (0, 99)
    chunkSize <- chooseInt (1, 10)
    terminator <- elements ["\\.", "\\.\n", ""] -- We also accept no terminator (maybe that's overly relaxed compared to psql, but oh well)
    rows <- forM [1 .. numLines] $ \_ -> do
      notEnding <- elements ["\\", ".", "\\.", ".\\", ""] -- Any characters that look like a terminator and could confuse the parser
      someCharsBefore <- arbitrary
      someCharsAfter <- arbitrary
      bef <-
        Text.pack
          <$> if someCharsBefore
            then listOf1 (arbitrary @Char `suchThat` (/=) '\n')
            else pure ""
      aft <-
        Text.pack
          <$> if someCharsAfter
            then listOf1 (arbitrary @Char `suchThat` (/=) '\n')
            else pure ""
      let row' = bef <> notEnding <> aft <> "\n"
          row = if row' == "\\.\n" then "x\\.\n" else row'
      pure row
    pure $ CopyBody {..}

shouldHaveWellParsedSql :: (MonadIO m) => SqlMigration m -> Text -> m ()
mig `shouldHaveWellParsedSql` sql = case migrationSql mig of
  UnparsedSql _ -> liftIO $ expectationFailure "Got UnparsedSql"
  ps@(WellParsedSql _) -> do
    psql <- parsedSqlText ps
    liftIO $ psql `shouldBe` sql

shouldHaveUnparsedSql :: (MonadIO m) => SqlMigration m -> Text -> m ()
shouldHaveUnparsedSql mig expectedSql = case migrationSql mig of
  WellParsedSql _ -> liftIO $ expectationFailure "Got WellParsedSql instead"
  UnparsedSql sql -> liftIO $ sql `shouldBe` expectedSql

-- | Same as a monadic `shouldSatisfy` isLeft, but does not require a Show instance.
shouldReturnLeft :: (MonadIO m) => m (Either a b) -> m ()
shouldReturnLeft mv =
  mv >>= \case
    Left _ -> pure ()
    Right _ -> liftIO $ expectationFailure "Got Right but was expecting Left"

{- A quick-n-dirty transformer to serve as a mock of the EnvVars monad -}
newtype EnvVarsT m a = EnvVarsT {runEnvVarsT :: Map Text Text -> m a}
  deriving stock (Functor)

-- | This isn't really overlapping. The compiler says so probably
-- because instance resolution doesn't check constraints.
instance {-# OVERLAPPING #-} EnvVars (EnvVarsT Identity) where
  -- Important invariant:
  -- All asked-for environment variables must appear in the result Map.
  getEnvVars vars = EnvVarsT $ \r ->
    pure $
      foldr
        (\k res -> let v = Map.findWithDefault "" k r in Map.insert k v res)
        mempty
        vars

instance (Monad m) => MonadThrow (EnvVarsT m) where
  throwM = error "throwM not implemented"

instance (Applicative m) => Applicative (EnvVarsT m) where
  pure v = EnvVarsT $ const (pure v)
  EnvVarsT run1 <*> EnvVarsT run2 = EnvVarsT $ \r -> run1 r <*> run2 r

{- ------------------------------------------------------------------- -}

instance (Monad m) => Monad (EnvVarsT m) where
  EnvVarsT run1 >>= f = EnvVarsT $ \r -> do
    v <- run1 r
    let EnvVarsT run2 = f v
    run2 r

-- | Concatenates text inside consecutive `CopyFromStdinRows` pieces into a single `CopyFromStdinRows`. Keeps other
-- pieces intact.
groupCopyRows :: [SqlPiece] -> [SqlPiece]
groupCopyRows =
  map concatCopy
    . List.groupBy
      ( \a b -> case (a, b) of
          (CopyFromStdinRows _, CopyFromStdinRows _) -> True
          _ -> False
      )
  where
    concatCopy :: [SqlPiece] -> SqlPiece
    concatCopy [] = error "Empty list!"
    concatCopy [x] = x
    concatCopy copyRows =
      CopyFromStdinRows $
        Text.concat $
          map
            ( \case
                CopyFromStdinRows r -> r
                _ -> error "Not a Copy row!!"
            )
            copyRows

newtype ArbitraryEffectAndStepsStream = ArbitraryEffectAndStepsStream (Stream (Of Integer) IO (IORef Integer))

instance Show ArbitraryEffectAndStepsStream where
  show _ = "arbitrary-stream"

instance Arbitrary ArbitraryEffectAndStepsStream where
  arbitrary = do
    -- Generate a bunch of random numbers and determine which ones of them will be effects and which will be steps
    stepsAndEffects :: [(Bool, Integer)] <- listOf arbitrary
    pure $ ArbitraryEffectAndStepsStream $ S.Effect $ do
      effSum <- newIORef 0
      pure $ go effSum stepsAndEffects
    where
      go :: IORef Integer -> [(Bool, Integer)] -> Stream (Of Integer) IO (IORef Integer)
      go effSum [] = S.Return effSum
      go effSum ((isEffect, n) : xs) = do
        if isEffect
          then S.Effect $ do
            modifyIORef' effSum (+ n)
            pure $ S.Step $ n :> go effSum xs
          else S.Step $ n :> go effSum xs

spec :: Spec
spec = do
  describe "Parsing tests" $ do
    it "forceStreamConcurrently for Streams returns the same stream in the absence of exceptions" $ property $ \(ArbitraryEffectAndStepsStream originalStream, numLookAhead :: Int) -> do
      let forcedStream = forceStreamConcurrently (fromIntegral $ max 0 numLookAhead) originalStream
      originalStreamAsList :> originalEffectSumIORef <- S.toList originalStream
      extractedStreamAsList :> extractedEffectSumIORef <- S.toList forcedStream
      originalEffectSum <- readIORef originalEffectSumIORef
      extractedEffectSum <- readIORef extractedEffectSumIORef
      extractedStreamAsList `shouldBe` originalStreamAsList
      extractedEffectSum `shouldBe` originalEffectSum
    it "forceStreamConcurrently for Streams with exceptions throws when consumed" $ property $ \(ArbitraryEffectAndStepsStream originalStream, numLookAhead :: Int) -> do
      sideEffCountStreamWithExceptions <- newIORef (0 :: Integer)
      sideEffCountForcedStream <- newIORef (0 :: Integer)
      let exampleException = userError "Failed to read from disk"
          streamWithExceptions = void originalStream <> S.Effect (throwM exampleException)
          forcedStream = forceStreamConcurrently (fromIntegral $ max 0 numLookAhead) streamWithExceptions
          mkIORefModifyingStream ior = S.mapM (\n -> modifyIORef' ior (+ n) >> pure n)
      -- A naive implementation of `forceStreamConcurrently` would not re-throw exceptions and could
      -- block forever
      S.toList (mkIORefModifyingStream sideEffCountStreamWithExceptions forcedStream) `shouldThrow` (== exampleException)
      S.toList (mkIORefModifyingStream sideEffCountForcedStream streamWithExceptions) `shouldThrow` (== exampleException)
      -- And the same side-effects for both in this case
      expectedSideEffCount <- readIORef sideEffCountStreamWithExceptions
      readIORef sideEffCountForcedStream `shouldReturn` expectedSideEffCount
    it "forceStreamConcurrently terminates background thread when returned stream is not consumed linearly" $ property $ \(ArbitraryEffectAndStepsStream originalStream, numLookAhead :: Int) -> do
      returnedStreamNotConsumedLinearly <- newEmptyMVar
      l <- S.toList_ $ S.take 10 $ forceStreamConcurrentlyInspect (Just returnedStreamNotConsumedLinearly) (fromIntegral $ max 0 numLookAhead) $ void originalStream <> S.repeat 0
      length l `shouldBe` 10

      -- We explicitly perform GCs because that's the mechanism relied on to detect no more downstream consumers
      -- of the returned stream exist. If this test doesn't terminate, it means it's failed.
      res <- race (forever $ performMajorGC >> threadDelay 1_000) (readMVar returnedStreamNotConsumedLinearly)
      case res of
        Left _ -> error "Impossible!"
        Right () -> pure ()
    context "manyStreaming" $ do
      it "Successful cases with or without unparsed text at the end" $ property $ \randomInt -> do
        let expectedParsedList = replicate (min 100 randomInt) "ab"
            parseableString :: Text = mconcat expectedParsedList
            extraUnparsed :: Text = if even randomInt then "" else "xxxx"
            string = parseableString <> extraUnparsed
            stream = unPureStream $ mkRandStream randomInt string
            parser :: Parsec.Parser Text = Parsec.string "a" >> Parsec.string "b" >> pure "ab"
        parsedList1 S.:> (unparsedText1, ()) <- S.toList $ manyStreaming (const $ (,()) <$> parser) () stream
        let runChecks unparsedText2 parsedList2 = do
              parsedList1 `shouldBe` parsedList2
              parsedList1 `shouldBe` expectedParsedList
              S.mconcat_ unparsedText1 `shouldReturn` unparsedText2
              unparsedText2 `shouldBe` extraUnparsed

        case Parsec.parse (Parsec.many' parser) string of
          Parsec.Fail {} -> error "failed to parse"
          Parsec.Partial continueParsing -> do
            extraUnparsed `shouldBe` ""
            case continueParsing "" of -- Pass EOF to say that we're finished
              Parsec.Fail {} -> error "nested failed to parse"
              Parsec.Partial _ -> error "partial parse not possible"
              Parsec.Done unparsedText2 parsedList2 -> runChecks unparsedText2 parsedList2
          Parsec.Done unparsedText2 parsedList2 -> runChecks unparsedText2 parsedList2

      it "Partially parsed at the end still returns unparsed text" $ property $ \randomInt -> do
        let expectedParsedList = replicate (min 100 randomInt) "ab"
            string :: Text = mconcat expectedParsedList <> "a"
            stream = unPureStream $ mkRandStream randomInt string
            parser :: Parsec.Parser Text = Parsec.string "a" >> Parsec.string "b" >> pure "ab"
        parsedList1 S.:> (unparsedText1, ()) <- S.toList $ manyStreaming (const $ (,()) <$> parser) () stream
        let runChecks unparsedText2 parsedList2 = do
              parsedList1 `shouldBe` parsedList2
              parsedList1 `shouldBe` expectedParsedList
              S.mconcat_ unparsedText1 `shouldReturn` unparsedText2
              unparsedText2 `shouldBe` "a"

        case Parsec.parse (Parsec.many' parser) string of
          Parsec.Fail {} -> error "failed to parse"
          Parsec.Partial continueParsing -> do
            case continueParsing "" of
              Parsec.Done unparsedText2 parsedList2 -> runChecks unparsedText2 parsedList2
              _ -> error "ooops"
          Parsec.Done {} -> error "should have been a partial parse"

    context "Multi Query Statement Parser" $ do
      it "Single command with and without semi-colon" $
        property $
          \randomSeed -> do
            blks :: [[SqlPiece]] <-
              traverse
                ( (S.toList_ . parseSqlPiecesStreaming)
                    . unPureStream
                    . mkRandStream randomSeed
                )
                [ "CREATE TABLE hello;",
                  "CREATE TABLE hello",
                  "CREATE TABLE hello; -- Comment",
                  "CREATE TABLE hello -- Comment",
                  "CREATE TABLE hello -- Comment\n;"
                ]
            blks
              `shouldBe` [ [OtherSqlPiece "CREATE TABLE hello;"],
                           [OtherSqlPiece "CREATE TABLE hello"],
                           [ OtherSqlPiece "CREATE TABLE hello;",
                             WhiteSpacePiece " ",
                             CommentPiece "-- Comment"
                           ],
                           [ OtherSqlPiece
                               "CREATE TABLE hello -- Comment"
                           ],
                           [ OtherSqlPiece
                               "CREATE TABLE hello -- Comment\n;"
                           ]
                         ]
      modifyMaxSuccess (const 10000)
        $ it "Statement separation boundaries are good"
        $ forAll
          ((,) <$> listOf1 genSingleSqlStatement <*> arbitrary @Int)
        $ \(origPieces, randomSeed) -> do
          parsedPieces <-
            S.toList_ $
              parseSqlPiecesStreaming $
                unPureStream $
                  mkRandStream randomSeed $
                    Text.concat (map piecesToText origPieces)
          groupCopyRows parsedPieces
            `shouldBe` groupCopyRows (mconcat origPieces)
      modifyMaxSuccess (const 10000)
        $ it
          "Statements concatenation matches original and statements end with semi-colon"
        $ do
          property $ \SyntacticallyValidRandomSql {..} -> do
            blks <-
              S.toList_ $
                parseSqlPiecesStreaming $
                  unPureStream unSyntRandomSql
            piecesToText blks `shouldBe` fullContents
            forM_ blks $ \case
              CommentPiece t ->
                t
                  `shouldSatisfy` ( \c ->
                                      "--"
                                        `Text.isPrefixOf` c
                                        || "/*"
                                          `Text.isPrefixOf` c
                                          && "*/"
                                            `Text.isSuffixOf` c
                                  )
              WhiteSpacePiece t ->
                t `shouldSatisfy` (\c -> Text.strip c == "")
              CopyFromStdinEnd ll -> ll `shouldBe` "\\.\n"
              _ -> pure ()

      modifyMaxSuccess (const 10000) $
        it "Parser of the body of COPY works for odd inputs" $
          property $
            \CopyBody {..} -> do
              -- We want to stress-test this parser at its boundaries: lines ending close to chunkSize, terminators in the contents etc.
              let rowsText = Text.concat rows
                  copyBody =
                    S.each rows <> S.yield terminator
              parsed <-
                S.toList_ $
                  parseSqlPiecesStreaming'
                    ( \_parserState ->
                        copyFromStdinAfterStatementParser chunkSize
                    )
                    copyBody

              groupCopyRows parsed
                `shouldBe` [ CopyFromStdinRows rowsText
                             | numLines /= 0
                           ]
                  ++ [CopyFromStdinEnd terminator | terminator /= ""]
              forM_ [row | CopyFromStdinRows row <- parsed] $ \row ->
                row `shouldSatisfy` ("\n" `Text.isSuffixOf`)

    context "Valid SQL Migrations" $ do
      it "Plain Sql Migration, missing optional options" $
        property @(SyntacticallyValidRandomSql IO -> IO ()) $
          \SyntacticallyValidRandomSql {..} -> do
            Right parsedMig <-
              parseSqlMigration
                "any-name.sql"
                unSyntRandomSql
            migrationName parsedMig `shouldBe` "any-name.sql"
            parsedMig `shouldHaveWellParsedSql` fullContents
            migrationInTxn parsedMig `shouldBe` True
            migrationCustomConnInfo parsedMig `shouldBe` Nothing
      it "Sql Migration options parsed correctly" $
        property $
          \randomSeed -> do
            let sql = "-- codd: no-txn\nSOME SQL"
            Right parsedMig <-
              parseSqlMigrationIO "any-name.sql" $
                mkRandStream randomSeed sql
            migrationName parsedMig `shouldBe` "any-name.sql"
            parsedMig `shouldHaveWellParsedSql` sql
            migrationInTxn parsedMig `shouldBe` False
            migrationCustomConnInfo parsedMig `shouldBe` Nothing

      it "Sql Migration connection and custom options" $
        property $
          \(connGen :: ConnStringGen, randomSeed) -> do
            let sql1 =
                  "\n-- codd: no-txn\n"
                    <> "\n\n-- codd-connection: "
                    <> renderConnStringGen connGen
                    <> "\n\nSOME SQL"
                sql2 =
                  "\n\n-- codd-connection: "
                    <> renderConnStringGen connGen
                    <> "\n-- codd: in-txn\n"
                    <> "SOME SQL"
            parsedMig1 <-
              either (error "Oh no") id
                <$> parseSqlMigrationIO
                  "any-name.sql"
                  (mkRandStream randomSeed sql1)
            migrationName parsedMig1 `shouldBe` "any-name.sql"
            parsedMig1 `shouldHaveWellParsedSql` sql1
            migrationInTxn parsedMig1 `shouldBe` False
            migrationCustomConnInfo parsedMig1
              `shouldBe` Just (connInfoFromConnStringGen connGen)

            parsedMig2 <-
              either (error "Oh nooo") id
                <$> parseSqlMigrationIO
                  "any-name.sql"
                  (mkRandStream randomSeed sql2)
            migrationName parsedMig2 `shouldBe` "any-name.sql"
            parsedMig2 `shouldHaveWellParsedSql` sql2
            migrationInTxn parsedMig2 `shouldBe` True
            migrationCustomConnInfo parsedMig2
              `shouldBe` Just (connInfoFromConnStringGen connGen)

      it "Sql Migration connection option alone" $
        property $
          \(connGen :: ConnStringGen, randomSeed) -> do
            let sql =
                  "-- random comment\n-- codd-connection: "
                    <> renderConnStringGen connGen
                    <> "\nSOME SQL"
            mig <-
              either (error "Oh no") id
                <$> parseSqlMigrationIO
                  "any-name.sql"
                  (mkRandStream randomSeed sql)
            migrationName mig `shouldBe` "any-name.sql"
            mig `shouldHaveWellParsedSql` sql
            migrationInTxn mig `shouldBe` True
            migrationCustomConnInfo mig `shouldBe` Just (connInfoFromConnStringGen connGen)

      it "Sql Migration parsed from disk with full contents" $
        runResourceT @IO $
          runCoddLogger $
            do
              [BlockInTxn ConsecutiveInTxnMigrations {inTxnMigs = asqlmig :| []}] <-
                parseMigrationFiles mempty $
                  Left ["test/migrations/normal-parse-test/"]
              rawFileContents <-
                liftIO $
                  Text.readFile
                    "test/migrations/normal-parse-test/2000-01-01-00-00-00-normal-parse-migration.sql"
              let (AddedSqlMigration mig _) = asqlmig
              mig `shouldHaveWellParsedSql` rawFileContents

      it "--codd: no-parse returns UnparsedSql" $
        property $
          \(RandomSql {unRandomSql, fullContents}, randomSeed) -> do
            let coddOpts = "-- codd: no-parse\n"
                sql = coddOpts <> fullContents

            mig <-
              either (error "Oops") id
                <$> parseSqlMigrationIO
                  "no-parse-migration.sql"
                  (mkRandStream randomSeed coddOpts <> unRandomSql)
            migrationName mig `shouldBe` "no-parse-migration.sql"
            mig `shouldHaveUnparsedSql` sql
            migrationInTxn mig `shouldBe` True
            migrationCustomConnInfo mig `shouldBe` Nothing

      it "--codd: no-parse preserves all of migrations file contents" $
        runResourceT @IO $
          runCoddLogger $
            do
              [BlockInTxn ConsecutiveInTxnMigrations {inTxnMigs = asqlmig :| []}] <-
                parseMigrationFiles mempty $
                  Left ["test/migrations/no-parse-test/"]
              rawFileContents <-
                liftIO $
                  Text.readFile
                    "test/migrations/no-parse-test/2000-01-01-00-00-00-no-parse-migration.sql"
              let (AddedSqlMigration mig _) = asqlmig
              mig `shouldHaveUnparsedSql` rawFileContents

      it "--codd: no-parse with custom connection-string" $
        property $
          \randomSeed -> do
            let sql =
                  "-- codd: no-parse\n\
                  \-- codd-connection: user=codd_admin dbname='codd-experiments' host=127.0.0.1 port=5433\n\
                  \-- Anything here!\
                  \Some statement; -- Some comment\n\
                  \Other statement"

            mig <-
              either (error "Oops") id
                <$> parseSqlMigrationIO
                  "no-parse-migration.sql"
                  (mkRandStream randomSeed sql)
            migrationName mig `shouldBe` "no-parse-migration.sql"
            mig `shouldHaveUnparsedSql` sql
            migrationInTxn mig `shouldBe` True
            migrationCustomConnInfo mig
              `shouldBe` Just
                ConnectionString
                  { user = "codd_admin",
                    hostname = "127.0.0.1",
                    port = 5433,
                    password = "",
                    database = "codd-experiments",
                    options = Nothing
                  }

      it "-- codd-env-vars templating" $ do
        let sqlWithVars =
              "-- codd: no-txn\n\
              \-- codd-connection: postgres://${PGSUPERUSER}@${PGHOST}:${PGPORT}/postgres\n\
              \-- codd-env-vars: PGSUPERUSER ,PGHOST,PGUSER, PGPORT  , UNDEFINED_VAR23, PGDATABASE\n\
              \CREATE DATABASE ${PGDATABASE};\n\
              \SELECT \"${PGUSER}\";\n\
              \SELECT ${NOTAVARIABLE};\n\
              \SELECT ${UNDEFINED_VAR23}"
            vars =
              Map.fromList
                [ ("PGSUPERUSER", "dbsuperuser"),
                  ("PGHOST", "someaddress"),
                  ("PGPORT", "5432"),
                  ("PGUSER", "codd_user"),
                  ("PGDATABASE", "codd_db")
                ]
            templatedSql =
              "-- codd: no-txn\n\
              \-- codd-connection: postgres://dbsuperuser@someaddress:5432/postgres\n\
              \-- codd-env-vars: PGSUPERUSER ,PGHOST,PGUSER, PGPORT  , UNDEFINED_VAR23, PGDATABASE\n\
              \CREATE DATABASE codd_db;\n\
              \SELECT \"codd_user\";\n\
              \SELECT ${NOTAVARIABLE};\n\
              \SELECT " -- Declared in the header but not defined is replaced by empty

        -- EnvVarsT over Identity is our way to mock environment variables
        let parsedSql = runIdentity $ flip runEnvVarsT vars $ do
              res <-
                parseAndClassifyMigration $
                  PureStream @(EnvVarsT Identity) $
                    S.yield sqlWithVars
              case res of
                Left err -> error err
                Right (_, _, _, UnparsedSql _) ->
                  error "Got UnparsedSql"
                Right (_, _, _, WellParsedSql pieces) ->
                  mconcat
                    . map sqlPieceText
                    <$> S.toList_ pieces
        parsedSql `shouldBe` templatedSql

    context "Invalid SQL Migrations" $ do
      modifyMaxSuccess (const 10000) $
        it "Sql Migration Parser never fails, even for random text" $
          do
            property $ \RandomSql {unRandomSql, fullContents} -> do
              emig <- parseSqlMigrationIO "any-name.sql" unRandomSql
              case migrationSql <$> emig of
                Left _ -> error "Should not be Left!"
                Right (WellParsedSql pieces) -> do
                  t <- piecesToText <$> S.toList_ pieces
                  t `shouldBe` fullContents
                Right (UnparsedSql t) ->
                  t `shouldBe` fullContents

      it "in-txn and no-txn are mutually exclusive" $
        property $
          \randomSeed -> do
            let plainSql = "SOME SQL"
                sql = "-- codd: no-txn, in-txn\n" <> plainSql
            shouldReturnLeft $
              parseSqlMigrationIO "any-name.sql" $
                mkRandStream randomSeed sql

      modifyMaxSuccess (* 100) $
        it "All valid combinations of codd top-level comment markers, with empty lines and user-added comments in between, are all parsed successfully" $
          property $
            \(randomSeed, linesObj@(TopLevelCoddCommentLinesAndUserComments lines)) -> do
              let sql = renderCoddCommentLines linesObj <> "-- Some SQL here\nSELECT 1;\n -- codd: this-will-be-ignored"
              eMig <-
                parseSqlMigrationIO
                  "any-name.sql"
                  $ mkRandStream
                    randomSeed
                    sql
              case eMig of
                Left e -> error $ "Got Left: " ++ show e
                Right mig -> do
                  let expectedOpts =
                        List.foldl' Set.union mempty $
                          map
                            ( \case
                                Just (Right opts) -> opts
                                _ -> mempty
                            )
                            lines
                      expectedCustomConnInfo =
                        listToMaybe $
                          mapMaybe
                            ( \case
                                Just (Left connGen) -> Just $ connInfoFromConnStringGen connGen
                                _ -> Nothing
                            )
                            lines
                  migrationRequiresCoddSchema mig `shouldBe` (OptRequiresCoddSchema `Set.member` expectedOpts)
                  migrationInTxn mig `shouldBe` (OptInTxn `Set.member` expectedOpts || not (OptNoTxn `Set.member` expectedOpts))
                  migrationCustomConnInfo mig `shouldBe` expectedCustomConnInfo
                  -- TODO: Failing!
                  -- --match "/Parsing/Parsing tests/Invalid SQL Migrations/All valid combinations of codd top-level comment markers, with empty lines and user-added comments in between, are all parsed successfully/" --seed 261994346
                  if OptNoParse `Set.member` expectedOpts
                    then
                      shouldHaveUnparsedSql mig sql
                    else
                      shouldHaveWellParsedSql mig sql

      it "Gibberish after -- codd:" $ property $ \randomSeed -> do
        let sql = "-- codd: complete gibberish\n" <> "ANY SQL HERE"
        mig <-
          parseSqlMigrationIO "any-name.sql" $
            mkRandStream randomSeed sql
        case mig of
          Left err ->
            err
              `shouldSatisfy`
              -- Error message is specific about what is wrong
              List.isInfixOf "complete gibberish"
          Right _ -> expectationFailure "Got Right"

      it "Duplicate options" $ property $ \randomSeed -> do
        let sql = "-- codd: in-txn, in-txn\n" <> "ANY SQL HERE"
        mig <-
          parseSqlMigrationIO "any-name.sql" $
            mkRandStream randomSeed sql
        case mig of
          Right _ -> expectationFailure "Got Right"
          Left err -> err `shouldSatisfy` List.isInfixOf "duplicate"

      it "Unknown / mistyped options" $ property $ \randomSeed -> do
        let sql = "-- codd: unknown-txn\n" <> "ANY SQL HERE"
        shouldReturnLeft $
          parseSqlMigrationIO "any-name.sql" $
            mkRandStream randomSeed sql

      it "Mistyped connection string option" $ property $ \randomSeed ->
        do
          let sql = "-- codd-connection: blah\n" <> "ANY SQL HERE"
          mig <-
            parseSqlMigrationIO "any-name.sql" $
              mkRandStream randomSeed sql
          case mig of
            Left err ->
              -- Nice error message explaining valid format
              err
                `shouldSatisfy` ( "postgresql.org" `List.isInfixOf`
                                )
            Right _ -> expectationFailure "Got Right"

      it "Two connection strings" $ property $ \randomSeed -> do
        let sql =
              "-- codd-connection: postgres://codd_admin@127.0.0.1:5433/codd-experiments\n"
                <> "-- codd-connection: postgres://codd_admin@127.0.0.1:5433/codd-experiments\n"
                <> "ANY SQL HERE"
        shouldReturnLeft $
          parseSqlMigrationIO "any-name.sql" $
            mkRandStream randomSeed sql

      it "Two -- codd comments" $ property $ \randomSeed -> do
        let sql =
              "-- codd: in-txn\n--codd: in-txn\n" <> "MORE SQL HERE"
        shouldReturnLeft $
          parseSqlMigrationIO "any-name.sql" $
            mkRandStream randomSeed sql

      it "--codd: no-parse does not mix with -- codd: no-txn" $
        property $
          \randomSeed -> do
            let sql =
                  "-- codd: no-parse\n-- codd: no-txn\nSome statement"
            mig <-
              parseSqlMigrationIO "no-parse-migration.sql" $
                mkRandStream randomSeed sql
            case mig of
              Left err ->
                err
                  `shouldSatisfy` ( "It is not possible to set both 'no-txn' and 'no-parse'" `List.isInfixOf`
                                  )
              Right _ -> expectationFailure "Got Right"
      it
        "SAVEPOINTs need to be released or rolled back inside SQL migrations"
        $ pendingWith
          "Testing this by selecting txid_current() might be more effective"

    context "Other important behaviours to test" $ do
      it "Countable-runnable statements counted and skipped correctly" $
        property $
          \(ShuffleOfPieces sqlPiecesWithComments, n) -> do
            let sqlPieces =
                  filter
                    ( \case
                        [CommentPiece _] -> False
                        [WhiteSpacePiece _] -> False
                        _ -> True
                    )
                    sqlPiecesWithComments
            -- Nothing to skip returns same stream
            S.toList_
              ( skipCountableRunnableStatements
                  0
                  (S.concat $ S.each sqlPieces)
              )
              `shouldReturn` concat sqlPieces

            -- We never break COPY apart and comments and white space are ignored up until the first statement not to be skipped
            S.toList_
              ( skipCountableRunnableStatements
                  n
                  (S.concat $ S.each sqlPieces)
              )
              `shouldReturn` concat (drop n sqlPieces)

            -- Comments and whitespace in the beginning are ignored
            when (n > 0) $ do
              let clutter =
                    [ [WhiteSpacePiece "         \n"],
                      [CommentPiece "-- some comment\n"],
                      [CommentPiece "/* comment \n \n \n test */"]
                    ]
              S.toList_
                ( skipCountableRunnableStatements
                    n
                    (S.concat $ S.each $ clutter ++ sqlPieces)
                )
                `shouldReturn` concat (drop n sqlPieces)

      it "Empty queries detector works well" $ property $ \randomSeed ->
        do
          let emptyQueries =
                [ "",
                  "           ",
                  "      --Some comment           ",
                  "    /* Just comment */ \n",
                  "      --Some comment    \n-- Some other comment       ",
                  "      --Some comment    \n-- Some other comment\n\n\n       ",
                  "    /* Just comment */ \n -- Other comment \n\n\n\n",
                  "    /* Just comment */ \n -- Other comment \n\n\n\n",
                  "\n\n"
                    <> "-- README:\n"
                    <> "-- This is an example of a Migration that renames a column in a Blue-Green-Safe way. Both Old and New Apps\n"
                    <> "-- need not be concerned of the new/old column names here. We recommend caution and testing when using this.\n\n"
                    <> "-- 1. Add the column and set its values, initially\n"
                    <> "-- ALTER TABLE employee ADD COLUMN employee_name TEXT; -- TODO: Remember to set a good DEFAULT if you need one.\n"
                    <> "/* UPDATE employee SET employee_name=name WHERE name IS DISTINCT FROM employee_name; */ \n"
                ]
              nonEmptyQueries =
                [ "      --Some comment    \n-- Some other comment\n\n\n Some SQL Command      ",
                  "      --Some comment    \n-- Some other comment\n\n\n - - This is not a valid comment and should be considered SQL",
                  "\n\n--Some comment    \n-- Some other comment\n\n\n -- Other comment\n\n\n SQL command",
                  "SOME SQL COMMAND      --Some comment    \n-- Some other comment\n\n\n - - This is not a valid comment and should be considered SQL",
                  "Regular sql COMMANDS -- this is a comment",
                  "Regular sql COMMANDS \n-- this is a comment\n",
                  "Regular sql /* With comment */ COMMANDS",
                  "/* With comment */ SQL COMMANDS",
                  "/* With comment */\n\nSQL COMMANDS\n-- Comment",
                  "\n\n"
                    <> "-- README:\n"
                    <> "-- This is an example of a Migration that renames a column in a Blue-Green-Safe way. Both Old and New Apps\n"
                    <> "-- need not be concerned of the new/old column names here. We recommend caution and testing when using this.\n\n"
                    <> "-- 1. Add the column and set its values, initially\n"
                    <> "ALTER TABLE employee ADD COLUMN employee_name TEXT; -- TODO: Remember to set a good DEFAULT if you need one.\n"
                    <> "UPDATE employee SET employee_name=name WHERE name IS DISTINCT FROM employee_name;\n"
                ]
          forM_ emptyQueries $ \q -> do
            parsed <-
              S.toList_ $
                parseSqlPiecesStreaming $
                  unPureStream $
                    mkRandStream randomSeed q
            forM_ parsed $ \p ->
              unless (isWhiteSpacePiece p || isCommentPiece p) $
                expectationFailure $
                  show p
          forM_
            ( nonEmptyQueries
                -- We want to test with more SQL fragments, but some of the ones in `validSqlStatements` are purely comments or
                -- white space, and we don't want those for this test
                ++ map
                  piecesToText
                  ( filter
                      ( not
                          . all
                            ( \p ->
                                isWhiteSpacePiece p
                                  || isCommentPiece p
                            )
                      )
                      validSqlStatements
                  )
            )
            $ \q -> do
              let parsed =
                    parseSqlPiecesStreaming $
                      unPureStream $
                        mkRandStream (randomSeed + 1) q
              S.any_
                ( \p ->
                    not
                      ( isWhiteSpacePiece p
                          || isCommentPiece p
                      )
                )
                parsed
                `shouldReturn` True

-- | Represents empty lines, user-added comments, comments such as "-- codd: in-txn"
-- and "-- codd-connection" comments, all interleaved in various ways, and always valid.
newtype TopLevelCoddCommentLinesAndUserComments = TopLevelCoddCommentLinesAndUserComments
  { -- | Empty lines are represented by an empty Set, user-added comments by a `Nothing`.
    linesOfCommentsBlanksOptsOrConnString :: [Maybe (Either ConnStringGen (Set SectionOption))]
  }
  deriving stock (Show)

renderCoddCommentLines :: TopLevelCoddCommentLinesAndUserComments -> Text
renderCoddCommentLines
  TopLevelCoddCommentLinesAndUserComments
    { linesOfCommentsBlanksOptsOrConnString
    } =
    Text.concat $
      map
        lineText
        linesOfCommentsBlanksOptsOrConnString
    where
      lineText :: Maybe (Either ConnStringGen (Set SectionOption)) -> Text
      lineText = \case
        Nothing -> "-- User-added comment\n"
        Just (Left connGen) -> "-- codd-connection: " <> renderConnStringGen connGen <> "\n"
        Just (Right opts)
          | null opts -> "\n"
          | otherwise -> "-- codd: " <> Text.intercalate "," (map sectionText $ Set.toList opts) <> "\n"
      sectionText = \case
        OptInTxn -> "in-txn"
        OptNoTxn -> "no-txn"
        OptNoParse -> "no-parse"
        OptRequiresCoddSchema -> "requires-codd-schema"

instance Arbitrary TopLevelCoddCommentLinesAndUserComments where
  arbitrary = do
    compatibleOpts <- arbOpts
    TopLevelCoddCommentLinesAndUserComments <$> QC.sized (lineWithSomething compatibleOpts False)
    where
      lineWithSomething :: Set SectionOption -> Bool -> Int -> Gen [Maybe (Either ConnStringGen (Set SectionOption))]
      lineWithSomething remainingOpts connStringAlreadyAdded size
        | size <= 0 = pure []
        | otherwise = do
            nextLine <- commentOrLineGen $ do
              let lineWithOptsGen = Set.fromList <$> QC.sublistOf (Set.toList remainingOpts)
              if connStringAlreadyAdded
                then Right <$> lineWithOptsGen
                else eitherGen (arbitrary @ConnStringGen) lineWithOptsGen
            let remainingOptsNew =
                  remainingOpts `Set.difference` case nextLine of
                    Just (Right s) -> s
                    _ -> mempty
                connStringAddedNew =
                  connStringAlreadyAdded || case nextLine of
                    Just (Left _) -> True
                    _ -> False
            (nextLine :) <$> lineWithSomething remainingOptsNew connStringAddedNew (size - 1)

      eitherGen :: Gen a -> Gen b -> Gen (Either a b)
      eitherGen l r =
        commentOrLineGen r >>= \case
          Just v -> pure $ Right v
          Nothing -> Left <$> l
      commentOrLineGen :: Gen a -> Gen (Maybe a)
      commentOrLineGen lineGen = do
        coin <- QC.frequency [(1, pure False), (20, pure True)]
        if coin then Just <$> lineGen else pure Nothing
      arbOpts = (Set.fromList <$> QC.listOf (QC.elements [OptInTxn, OptNoTxn, OptNoParse, OptRequiresCoddSchema])) `QC.suchThat` isValid
      isValid opts =
        not
          ( OptNoTxn `Set.member` opts
              && OptNoParse `Set.member` opts
          )
          && not
            (OptInTxn `Set.member` opts && OptNoTxn `Set.member` opts)
  shrink (TopLevelCoddCommentLinesAndUserComments lines) =
    [ TopLevelCoddCommentLinesAndUserComments [lineWithSimplerConnGen] | line <- lines, lineWithSimplerConnGen <- case line of
                                                                                          Just (Left connGen) -> map (Just . Left) $ shrink connGen
                                                                                          _ -> [line]
    ]
