module EnvironmentSpec
  ( ConnStringGen (..),
    spec,
  )
where

import Codd.Environment (retryPolicyParser)
import Codd.Parsing (CoddCommentParseResult (..), coddConnStringCommentParser, connStringParser)
import Codd.Representations.Types (ObjName (..))
import Codd.Types
  ( RetryBackoffPolicy (..),
    RetryPolicy (..),
  )
import Control.Monad (forM_, unless)
import Data.Attoparsec.Text
  ( endOfInput,
    parseOnly,
  )
import qualified Data.Char as Char
import Data.Either (isLeft)
import Data.Hashable (hash)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (secondsToDiffTime)
import Data.Word (Word16)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Network.URI
  ( URI (..),
    URIAuth (..),
    escapeURIString,
    isUnescapedInURIComponent,
    uriToString,
  )
import Test.Hspec
import Test.QuickCheck
import TypesGen (genObjName)

newtype NonEmptyString = NonEmptyString {unString :: String} deriving stock (Show)

instance Arbitrary NonEmptyString where
  arbitrary =
    NonEmptyString
      . getASCIIString
      <$> arbitrary @ASCIIString
        `suchThat` (/= ASCIIString "")

data HostString = Hostname Text | HostUnixSocket Text
  deriving stock (Show)

originalHost :: HostString -> Text
originalHost (Hostname t) = t
originalHost (HostUnixSocket t) = t

instance Arbitrary HostString where
  arbitrary =
    elements
      [ Hostname "127.0.0.1",
        Hostname "200.200.100.100",
        Hostname "hostnodots",
        Hostname "some.host.name",
        Hostname "::1",
        Hostname "2800:3f0:4001:822::200e",
        Hostname "2a03:2880:f105:283:face:b00c:0:25de",
        HostUnixSocket "/var/lib/postgresql"
      ]

data ConnStringType = URIpostgres | Kwvps
  deriving stock (Bounded, Enum, Show)

instance Arbitrary ConnStringType where
  arbitrary = elements [minBound .. maxBound]

getURIConnString ::
  String -> ObjName -> String -> HostString -> Word16 -> ObjName -> Text
getURIConnString uriScheme (Text.unpack . unObjName -> usr) pwd host port (Text.unpack . unObjName -> dbName) =
  Text.pack $ uriToString id uri ""
  where
    uri =
      URI
        { uriScheme,
          uriAuthority =
            Just
              URIAuth
                { uriUserInfo =
                    encodeURIComponent usr
                      <> ( if pwd == ""
                             then ""
                             else ':' : encodeURIComponent pwd
                         )
                      <> "@",
                  uriRegName = Text.unpack $ escapeHost host,
                  uriPort = ':' : show port
                },
          uriPath = '/' : encodeURIComponent dbName,
          uriQuery = "",
          uriFragment = ""
        }

encodeURIComponent :: String -> String
encodeURIComponent = escapeURIString isUnescapedInURIComponent

data KwvpConnGen = KwvpConnGen
  { -- | Shuffles the relative order of keyword/value pairs in the generated connection string.
    shuffleIdx :: Int,
    -- | The boolean dictates quoting when quoting is optional.
    -- The Int represents extra spaces around this kwvp. These will be added one by one
    -- left-to-right everywhere they are acceptable until a limit of 4 (therefore ignoring > 4 spaces).
    user :: (ObjName, Bool, Int),
    password :: Maybe (String, Bool, Int),
    host :: (HostString, Bool, Int),
    port :: Maybe (Word16, Bool, Int),
    dbname :: (ObjName, Bool, Int)
  }

getKeywordValuePairConnString :: KwvpConnGen -> Text
getKeywordValuePairConnString KwvpConnGen {..} =
  Text.intercalate " " $
    map (\(kw, v) -> kw <> "=" <> v) mixedKwvps
  where
    mixedKwvps =
      sortOn ((`mod` max 1 shuffleIdx) . hash) $
        catMaybes
          [ Just $ addExtraSpaces user ("user", quoteIfNeeded unObjName user),
            Just $
              addExtraSpaces
                host
                ("host", quoteIfNeeded originalHost host),
            Just $
              addExtraSpaces dbname ("dbname", quoteIfNeeded unObjName dbname),
            (\p -> addExtraSpaces p ("password", quoteIfNeeded Text.pack p))
              <$> password,
            (\p -> addExtraSpaces p ("port", quoteIfNeeded (Text.pack . show) p))
              <$> port
          ]
    addExtraSpaces (_, _, spaces) (k, v)
      | spaces <= 0 = (k, v)
      | spaces == 1 = (" " <> k, v)
      | spaces == 2 = (" " <> k <> " ", v)
      | spaces == 3 = (" " <> k <> " ", " " <> v)
      | otherwise = (" " <> k <> " ", " " <> v <> " ")
    quoteIfNeeded un (v, force, _) =
      if needsQuoting (un v) || force
        then
          "'"
            <> Text.replace "'" "\\'" (Text.replace "\\" "\\\\" (un v))
            <> "'"
        else un v
    needsQuoting v =
      "'"
        `Text.isInfixOf` v
        || "\\"
          `Text.isInfixOf` v
        || " "
          `Text.isInfixOf` v
        || v
          == ""

escapeHost :: HostString -> Text
escapeHost (Hostname host) =
  -- bracket-escape IPv6 addresses
  if ":" `Text.isInfixOf` host then "[" <> host <> "]" else host
escapeHost (HostUnixSocket socketpath) = Text.pack $ encodeURIComponent $ Text.unpack socketpath

data ConnStringGen = ConnStringGen Text ConnectInfo
  deriving stock (Show)

instance Arbitrary ConnStringGen where
  arbitrary = do
    usr <- genObjName
    pwd <-
      oneof
        [ pure "",
          getPrintableString
            <$> arbitrary @PrintableString
              `suchThat` (\(PrintableString s) -> '\n' `notElem` s)
        ]
    host <- arbitrary @HostString
    port <- oneof [pure 5432, arbitrary @Word16]
    dbName <- genObjName

    let connInfo =
          ConnectInfo
            { connectHost = Text.unpack $ originalHost host,
              connectPort = port,
              connectUser = Text.unpack $ unObjName usr,
              connectPassword = pwd,
              connectDatabase = Text.unpack $ unObjName dbName
            }
    connStringType <- arbitrary @ConnStringType
    case connStringType of
      Kwvps -> do
        (shuffleIdx, b1, s1, b2, s2, b3, s3) <- arbitrary
        (b4, s4, b5, s5) <- arbitrary
        withPwd <-
          if connectPassword connInfo == ""
            then arbitrary @Bool
            else pure True
        withPort <-
          if connectPort connInfo == 5432
            then arbitrary @Bool
            else pure True
        pure $
          ConnStringGen
            ( getKeywordValuePairConnString
                KwvpConnGen
                  { shuffleIdx,
                    user = (usr, b1, s1),
                    password =
                      if withPwd
                        then Just (pwd, b2, s2)
                        else Nothing,
                    host = (host, b3, s3),
                    port =
                      if withPort
                        then Just (port, b4, s4)
                        else Nothing,
                    dbname = (dbName, b5, s5)
                  }
            )
            connInfo
      URIpostgres -> do
        uriScheme <- elements ["postgres:", "postgresql:"]
        pure $
          ConnStringGen
            (getURIConnString uriScheme usr pwd host port dbName)
            connInfo

spec :: Spec
spec = do
  describe "Environment tests" $ do
    context "Connection string parsing" $ do
      it "Some hard coded connection strings" $ do
        -- These are hard coded connection strings that I've tested with psql at some point
        let stringsAndExpectedConnInfos =
              [ ( "postgresql://postgres@localhost/some%20thing%20%2F%3F%3A%40",
                  ConnectInfo
                    { connectHost = "localhost",
                      connectPort = 5432,
                      connectUser = "postgres",
                      connectPassword = "",
                      connectDatabase = "some thing /?:@"
                    }
                ),
                ( "postgresql://some%20thing%20%2F%3F%3A%40:passwdsome%20thing%20%2F%3F%3A%40@localhost:1/some%20thing%20%2F%3F%3A%40",
                  ConnectInfo
                    { connectHost = "localhost",
                      connectPort = 1,
                      connectUser = "some thing /?:@",
                      connectPassword = "passwdsome thing /?:@",
                      connectDatabase = "some thing /?:@"
                    }
                ),
                -- Only mandatory arguments
                ( "postgresql://postgres@[::1]/somedb",
                  ConnectInfo
                    { connectHost = "::1",
                      connectPort = 5432,
                      connectUser = "postgres",
                      connectPassword = "",
                      connectDatabase = "somedb"
                    }
                ),
                ( "dbname='some thing /?:@' user='some thing /?:@'   host = localhost   ",
                  ConnectInfo
                    { connectHost = "localhost",
                      connectPort = 5432,
                      connectUser = "some thing /?:@",
                      connectPassword = "",
                      connectDatabase = "some thing /?:@"
                    }
                ),
                ( "dbname='some thing /?:@'\nuser='some thing /?:@' password='passwdsome thing /?:@' port=1 host = localhost\n",
                  ConnectInfo
                    { connectHost = "localhost",
                      connectPort = 1,
                      connectUser = "some thing /?:@",
                      connectPassword = "passwdsome thing /?:@",
                      connectDatabase = "some thing /?:@"
                    }
                ),
                ( "\ndbname=codd-experiments   \t\n    user=postgres port=5433 host=::1\n",
                  ConnectInfo
                    { connectHost = "::1",
                      connectPort = 5433,
                      connectUser = "postgres",
                      connectPassword = "",
                      connectDatabase = "codd-experiments"
                    }
                ),
                -- Only mandatory arguments
                ( "dbname=codd-experiments\tuser=postgres\rhost=127.0.0.1",
                  ConnectInfo
                    { connectHost = "127.0.0.1",
                      connectPort = 5432,
                      connectUser = "postgres",
                      connectPassword = "",
                      connectDatabase = "codd-experiments"
                    }
                )
              ]
        forM_ stringsAndExpectedConnInfos $ \(connString, expectedConnInfo) -> do
          parseOnly
            (connStringParser <* endOfInput)
            connString
            `shouldBe` Right expectedConnInfo
          -- It makes no sense to try to parse --codd-connection comments with newlines not at the end because our SQL parser would never generate them, so we don't even try in those cases
          unless (Text.any (\c -> c /= ' ' && Char.isSpace c) connString) $ do
            parseOnly (coddConnStringCommentParser <* endOfInput) ("-- codd-connection: " <> connString <> "\n") `shouldBe` Right (CoddCommentSuccess expectedConnInfo)
            -- In the line below, a --codd-connection comment without a newline most likely means it's an empty migration, but it's better if we parse it successfully so that we tell the user their migration is empty instead of saying it's a connection string problem.
            parseOnly (coddConnStringCommentParser <* endOfInput) ("-- codd-connection: " <> connString) `shouldBe` Right (CoddCommentSuccess expectedConnInfo)

      it "Randomized valid connection strings" $ do
        property $ \(ConnStringGen connStr connInfo) ->
          parseOnly (connStringParser <* endOfInput) connStr
            `shouldBe` Right connInfo
    context "Retry policy parsing" $ do
      it "Invalid policy strings" $ do
        parseOnly
          (retryPolicyParser <* endOfInput)
          "max 1 backoff exponential 2"
          `shouldSatisfy` isLeft

        parseOnly
          (retryPolicyParser <* endOfInput)
          "max -1 backoff exponential 2s"
          `shouldSatisfy` isLeft
      it "Valid policy strings" $ do
        parseOnly
          (retryPolicyParser <* endOfInput)
          "max 0 backoff exponential 2s"
          `shouldBe` Right
            ( RetryPolicy
                0
                ( ExponentialBackoff (secondsToDiffTime 2)
                )
            )

        parseOnly
          (retryPolicyParser <* endOfInput)
          "max 2 backoff exponential 2.5s"
          `shouldBe` Right
            ( RetryPolicy
                2
                ( ExponentialBackoff
                    (realToFrac @Double 2.5)
                )
            )

        parseOnly
          (retryPolicyParser <* endOfInput)
          "max 7 backoff constant 1250ms"
          `shouldBe` Right
            ( RetryPolicy
                7
                ( ConstantBackoff
                    (realToFrac @Double 1.25)
                )
            )
