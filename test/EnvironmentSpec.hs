module EnvironmentSpec
  ( ConnStringGen (..),
    renderConnStringGen,
    connInfoFromConnStringGen,
    spec,
  )
where

import Codd.Environment (retryPolicyParser)
import Codd.Parsing (CoddCommentParseResult (..), coddConnStringCommentParser, connStringParser)
import Codd.Representations.Types (ObjName (..))
import Codd.Types (ConnectionString (..), RetryBackoffPolicy (..), RetryPolicy (..))
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
    kwvp_password :: Maybe (String, Bool, Int),
    host :: (HostString, Bool, Int),
    kwvp_port :: Maybe (Word16, Bool, Int),
    dbname :: (ObjName, Bool, Int)
  }
  deriving stock (Show)

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
              <$> kwvp_password,
            (\p -> addExtraSpaces p ("port", quoteIfNeeded (Text.pack . show) p))
              <$> kwvp_port
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
        || Text.any Char.isSpace v
        || v
          == ""

escapeHost :: HostString -> Text
escapeHost (Hostname host) =
  -- bracket-escape IPv6 addresses
  if ":" `Text.isInfixOf` host then "[" <> host <> "]" else host
escapeHost (HostUnixSocket socketpath) = Text.pack $ encodeURIComponent $ Text.unpack socketpath

newtype ConnStringGen = ConnStringGen (Either KwvpConnGen (String, ObjName, String, HostString, Word16, ObjName))

instance Show ConnStringGen where
  show (ConnStringGen kwvpOrUriScheme) = Text.unpack $
    case kwvpOrUriScheme of
      Left kwvpConnGen -> getKeywordValuePairConnString kwvpConnGen
      Right (uriScheme, usr, pwd, host, port, dbName) ->
        getURIConnString uriScheme usr pwd host port dbName

renderConnStringGen :: ConnStringGen -> Text
renderConnStringGen (ConnStringGen kwvpOrUriScheme) =
  case kwvpOrUriScheme of
    Left kwvpConnGen -> getKeywordValuePairConnString kwvpConnGen
    Right (uriScheme, usr, pwd, host, port, dbName) ->
      getURIConnString uriScheme usr pwd host port dbName

connInfoFromConnStringGen :: ConnStringGen -> ConnectionString
connInfoFromConnStringGen (ConnStringGen kwvpOrUriScheme) =
  case kwvpOrUriScheme of
    Left KwvpConnGen {user = (user, _, _), host = (host, _, _), dbname = (dbname, _, _), kwvp_password, kwvp_port} ->
      ConnectionString
        { hostname = Text.unpack $ originalHost host,
          port = case kwvp_port of
            Nothing -> 5432
            Just (p, _, _) -> p,
          user = Text.unpack $ unObjName user,
          password = case kwvp_password of
            Nothing -> ""
            Just (p, _, _) -> p,
          database = Text.unpack $ unObjName dbname,
          options = Nothing
        }
    Right (uriScheme, user, password, host, port, dbname) ->
      ConnectionString
        { hostname = Text.unpack $ originalHost host,
          port = port,
          user = Text.unpack $ unObjName user,
          password = password,
          database = Text.unpack $ unObjName dbname,
          options = Nothing
        }

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
          ConnectionString
            { hostname = Text.unpack $ originalHost host,
              port,
              user = Text.unpack $ unObjName usr,
              password = pwd,
              database = Text.unpack $ unObjName dbName,
              options = Nothing
            }
    connStringType <- arbitrary @ConnStringType
    case connStringType of
      Kwvps -> do
        (shuffleIdx, b1, s1, b2, s2, b3, s3) <- arbitrary
        (b4, s4, b5, s5) <- arbitrary
        withPwd <-
          if (password :: ConnectionString -> String) connInfo == ""
            then arbitrary @Bool
            else pure True
        withPort <-
          if port == 5432
            then arbitrary @Bool
            else pure True
        pure $
          ConnStringGen $
            Left $
              KwvpConnGen
                { shuffleIdx,
                  user = (usr, b1, s1),
                  kwvp_password =
                    if withPwd
                      then Just (pwd, b2, s2)
                      else Nothing,
                  host = (host, b3, s3),
                  kwvp_port =
                    if withPort
                      then Just (port, b4, s4)
                      else Nothing,
                  dbname = (dbName, b5, s5)
                }
      URIpostgres -> do
        uriScheme <- elements ["postgres:", "postgresql:"]
        pure $
          ConnStringGen $
            Right (uriScheme, usr, pwd, host, port, dbName)
  shrink (ConnStringGen genInfo) =
    case genInfo of
      Left kwvpGen@KwvpConnGen {user = (user, _, _), host = (host, _, _), dbname = (dbname, _, _)} ->
        [ ConnStringGen
            ( Left $
                KwvpConnGen
                  { shuffleIdx = 0,
                    user = (user, False, 0),
                    kwvp_password = Nothing, -- Remove password
                    host = (host, False, 0),
                    kwvp_port = Nothing, -- Remove port
                    dbname = (ObjName "simpledb", False, 0)
                  }
            ),
          ConnStringGen
            ( Left $
                KwvpConnGen
                  { shuffleIdx = 0,
                    user = (ObjName "", False, 0), -- Remove user
                    kwvp_password = Nothing, -- Remove password
                    host = (host, False, 0),
                    kwvp_port = Nothing, -- Remove port
                    dbname = (ObjName "simpledb", False, 0)
                  }
            )
        ]

spec :: Spec
spec = do
  describe "Environment tests" $ do
    context "Connection string parsing" $ do
      it "Some hard coded connection strings" $ do
        -- These are hard coded connection strings that I've tested with psql at some point
        let stringsAndExpectedConnInfos =
              [ ( "postgresql://postgres@localhost/some%20thing%20%2F%3F%3A%40",
                  ConnectionString
                    { hostname = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = "",
                      database = "some thing /?:@",
                      options = Nothing
                    }
                ),
                ( "postgresql://some%20thing%20%2F%3F%3A%40:passwdsome%20thing%20%2F%3F%3A%40@localhost:1/some%20thing%20%2F%3F%3A%40",
                  ConnectionString
                    { hostname = "localhost",
                      port = 1,
                      user = "some thing /?:@",
                      password = "passwdsome thing /?:@",
                      database = "some thing /?:@",
                      options = Nothing
                    }
                ),
                -- Only mandatory arguments
                ( "postgresql://postgres@[::1]/somedb",
                  ConnectionString
                    { hostname = "::1",
                      port = 5432,
                      user = "postgres",
                      password = "",
                      database = "somedb",
                      options = Nothing
                    }
                ),
                ( "dbname='some thing /?:@' user='some thing /?:@'   host = localhost   ",
                  ConnectionString
                    { hostname = "localhost",
                      port = 5432,
                      user = "some thing /?:@",
                      password = "",
                      database = "some thing /?:@",
                      options = Nothing
                    }
                ),
                ( "dbname='some thing /?:@'\nuser='some thing /?:@' password='passwdsome thing /?:@' port=1 host = localhost\n",
                  ConnectionString
                    { hostname = "localhost",
                      port = 1,
                      user = "some thing /?:@",
                      password = "passwdsome thing /?:@",
                      database = "some thing /?:@",
                      options = Nothing
                    }
                ),
                ( "\ndbname=codd-experiments   \t\n    user=postgres port=5433 host=::1\n",
                  ConnectionString
                    { hostname = "::1",
                      port = 5433,
                      user = "postgres",
                      password = "",
                      database = "codd-experiments",
                      options = Nothing
                    }
                ),
                -- Only mandatory arguments
                ( "dbname=codd-experiments\tuser=postgres\rhost=127.0.0.1",
                  ConnectionString
                    { hostname = "127.0.0.1",
                      port = 5432,
                      user = "postgres",
                      password = "",
                      database = "codd-experiments",
                      options = Nothing
                    }
                ),
                -- I didn't really test this with psql, but IIUC = signs should be accepted as parts of values
                ( "dbname=codd-experiments\t  user=  post=gres password='abc = def'  host=127.0.0.1",
                  ConnectionString
                    { hostname = "127.0.0.1",
                      port = 5432,
                      user = "post=gres",
                      password = "abc = def",
                      database = "codd-experiments",
                      options = Nothing
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
        property $ \(connGen :: ConnStringGen) ->
          parseOnly (connStringParser <* endOfInput) (renderConnStringGen connGen)
            `shouldBe` Right (connInfoFromConnStringGen connGen)
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
