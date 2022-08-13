module EnvironmentSpec
    ( ConnStringGen(..)
    , spec
    ) where

import           Codd.Environment               ( retryPolicyParser )
import           Codd.Hashing.Types             ( ObjName(..) )
import           Codd.Parsing                   ( connStringParser )
import           Codd.Types                     ( RetryBackoffPolicy(..)
                                                , RetryPolicy(..)
                                                )
import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Data.Align                     ( alignWith )
import           Data.Attoparsec.Text           ( endOfInput
                                                , parseOnly
                                                )
import           Data.Either                    ( isLeft )
import           Data.Functor                   ( (<&>) )
import           Data.Hashable                  ( hash )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Time                      ( secondsToDiffTime )
import           Data.Word                      ( Word16 )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           GHC.Generics                   ( Generic )
import           Network.URI                    ( URI(..)
                                                , URIAuth(..)
                                                , escapeURIString
                                                , isUnescapedInURIComponent
                                                , uriToString
                                                )
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSuccess )
import           Test.QuickCheck
import           TypesGen                       ( genObjName )

newtype NonEmptyString = NonEmptyString { unString :: String } deriving stock Show
instance Arbitrary NonEmptyString where
    arbitrary =
        NonEmptyString
            .          getASCIIString
            <$>        arbitrary @ASCIIString
            `suchThat` (/= ASCIIString "")

newtype HostString = HostString { unHostString :: String }
    deriving stock Show
instance Arbitrary HostString where
    arbitrary = HostString . Text.unpack <$> oneof
        [ pure "127.0.0.1"
        , pure "200.200.100.100"
        , pure "hostnodots"
        , pure "some.host.name"
        , pure "::1"
        , pure "2800:3f0:4001:822::200e"
        , pure "2a03:2880:f105:283:face:b00c:0:25de"
        ]

data ConnStringType = URIpostgres | Kwvps
    deriving stock (Bounded, Enum, Show)

instance Arbitrary ConnStringType where
    arbitrary = elements [minBound .. maxBound]

getURIConnString
    :: String -> ObjName -> String -> HostString -> Word16 -> ObjName -> Text
getURIConnString uriScheme (Text.unpack . unObjName -> usr) pwd (unHostString -> host) port (Text.unpack . unObjName -> dbName)
    = Text.pack $ uriToString id uri ""
  where
    uri = URI
        { uriScheme
        , uriAuthority = Just URIAuth
            { uriUserInfo = encodeURIComponent usr
                            <> (if pwd == ""
                                   then ""
                                   else ':' : encodeURIComponent pwd
                               )
                            <> "@"
            , uriRegName  = Text.unpack $ escapeHost $ Text.pack host
            , uriPort     = ':' : show port
            }
        , uriPath      = '/' : encodeURIComponent dbName
        , uriQuery     = ""
        , uriFragment  = ""
        }
    encodeURIComponent :: String -> String
    encodeURIComponent = escapeURIString isUnescapedInURIComponent

data KwvpConnGen = KwvpConnGen
    { shuffleIdx :: Int
    -- ^ Shuffles the relative order of keyword/value pairs in the generated connection string.
    , user       :: (ObjName, Bool, Int)
    -- ^ The boolean dictates quoting when quoting is optional.
    -- The Int represents extra spaces around this kwvp. These will be added one by one
    -- left-to-right everywhere they are acceptable until a limit of 4 (therefore ignoring > 4 spaces).
    , password   :: Maybe (String, Bool, Int)
    , host       :: (HostString, Bool, Int)
    , port       :: Maybe (Word16, Bool, Int)
    , dbname     :: (ObjName, Bool, Int)
    }

getKeywordValuePairConnString :: KwvpConnGen -> Text
getKeywordValuePairConnString KwvpConnGen {..} = Text.intercalate " "
    $ map (\(kw, v) -> kw <> "=" <> v) mixedKwvps
  where
    mixedKwvps = sortOn ((`mod` max 1 shuffleIdx) . hash) $ catMaybes
        [ Just $ addExtraSpaces user ("user", quoteIfNeeded unObjName user)
        , Just $ addExtraSpaces
            host
            ("host", quoteIfNeeded (Text.pack . unHostString) host)
        , Just
            $ addExtraSpaces dbname ("dbname", quoteIfNeeded unObjName dbname)
        , (\p -> addExtraSpaces p ("password", quoteIfNeeded Text.pack p))
            <$> password
        , (\p -> addExtraSpaces p ("port", quoteIfNeeded (Text.pack . show) p))
            <$> port
        ]
    addExtraSpaces (_, _, spaces) (k, v)
        | spaces <= 0 = (k, v)
        | spaces == 1 = (" " <> k, v)
        | spaces == 2 = (" " <> k <> " ", v)
        | spaces == 3 = (" " <> k <> " ", " " <> v)
        | otherwise   = (" " <> k <> " ", " " <> v <> " ")
    quoteIfNeeded un (v, force, _) = if needsQuoting (un v) || force
        then
            "'"
            <> Text.replace "'" "\\'" (Text.replace "\\" "\\\\" (un v))
            <> "'"
        else un v
    needsQuoting v =
        "'"
            `Text.isInfixOf` v
            ||               "\\"
            `Text.isInfixOf` v
            ||               " "
            `Text.isInfixOf` v
            ||               v
            ==               ""

escapeHost :: Text -> Text
escapeHost host =
    -- bracket-escape IPv6 addresses
    if ":" `Text.isInfixOf` host then "[" <> host <> "]" else host

data ConnStringGen = ConnStringGen Text ConnectInfo
    deriving stock Show
instance Arbitrary ConnStringGen where
    arbitrary = do
        usr <- genObjName
        pwd <- oneof
            [ pure ""
            , getPrintableString
            <$>        arbitrary @PrintableString
            `suchThat` (\(PrintableString s) -> '\n' `notElem` s)
            ]
        host   <- arbitrary @HostString
        port   <- oneof [pure 5432, arbitrary @Word16]
        dbName <- genObjName

        let connInfo = ConnectInfo
                { connectHost     = unHostString host
                , connectPort     = port
                , connectUser     = Text.unpack $ unObjName usr
                , connectPassword = pwd
                , connectDatabase = Text.unpack $ unObjName dbName
                }
        connStringType <- arbitrary @ConnStringType
        case connStringType of
            Kwvps -> do
                (shuffleIdx, b1, s1, b2, s2, b3, s3) <- arbitrary
                (b4, s4, b5, s5) <- arbitrary
                withPwd <- if connectPassword connInfo == ""
                    then arbitrary @Bool
                    else pure True
                withPort <- if connectPort connInfo == 5432
                    then arbitrary @Bool
                    else pure True
                pure $ ConnStringGen
                    (getKeywordValuePairConnString KwvpConnGen
                        { shuffleIdx
                        , user       = (usr, b1, s1)
                        , password   = if withPwd
                                           then Just (pwd, b2, s2)
                                           else Nothing
                        , host       = (host, b3, s3)
                        , port       = if withPort
                                           then Just (port, b4, s4)
                                           else Nothing
                        , dbname     = (dbName, b5, s5)
                        }
                    )
                    connInfo
            URIpostgres -> do
                uriScheme <- elements ["postgres:", "postgresql:"]
                pure $ ConnStringGen
                    (getURIConnString uriScheme usr pwd host port dbName)
                    connInfo

spec :: Spec
spec = do
    describe "Environment tests" $ do
        context "Connection string parsing" $ do
            it "Some hard coded URI Encoded connection strings" $ do
                -- These are hard coded connection strings that I've tested with psql at some point
                parseOnly
                        (connStringParser <* endOfInput)
                        "postgresql://postgres@localhost/some%20thing%20%2F%3F%3A%40"
                    `shouldBe` Right ConnectInfo
                                   { connectHost     = "localhost"
                                   , connectPort     = 5432
                                   , connectUser     = "postgres"
                                   , connectPassword = ""
                                   , connectDatabase = "some thing /?:@"
                                   }
                parseOnly
                        (connStringParser <* endOfInput)
                        "postgresql://some%20thing%20%2F%3F%3A%40:passwdsome%20thing%20%2F%3F%3A%40@localhost:1/some%20thing%20%2F%3F%3A%40"
                    `shouldBe` Right ConnectInfo
                                   { connectHost     = "localhost"
                                   , connectPort     = 1
                                   , connectUser     = "some thing /?:@"
                                   , connectPassword = "passwdsome thing /?:@"
                                   , connectDatabase = "some thing /?:@"
                                   }

                -- Only mandatory arguments
                parseOnly (connStringParser <* endOfInput)
                          "postgresql://postgres@[::1]/somedb"
                    `shouldBe` Right ConnectInfo { connectHost     = "::1"
                                                 , connectPort     = 5432
                                                 , connectUser     = "postgres"
                                                 , connectPassword = ""
                                                 , connectDatabase = "somedb"
                                                 }

            it "Some hard coded keyword/value pair connection strings" $ do
                -- These are hard coded connection strings that I've tested with psql at some point
                parseOnly
                        (connStringParser <* endOfInput)
                        "dbname='some thing /?:@' user='some thing /?:@'   host = localhost   "
                    `shouldBe` Right ConnectInfo
                                   { connectHost     = "localhost"
                                   , connectPort     = 5432
                                   , connectUser     = "some thing /?:@"
                                   , connectPassword = ""
                                   , connectDatabase = "some thing /?:@"
                                   }
                parseOnly
                        (connStringParser <* endOfInput)
                        "dbname='some thing /?:@' user='some thing /?:@' password='passwdsome thing /?:@' port=1 host = localhost"
                    `shouldBe` Right ConnectInfo
                                   { connectHost     = "localhost"
                                   , connectPort     = 1
                                   , connectUser     = "some thing /?:@"
                                   , connectPassword = "passwdsome thing /?:@"
                                   , connectDatabase = "some thing /?:@"
                                   }

                parseOnly
                        (connStringParser <* endOfInput)
                        "dbname=codd-experiments user=postgres port=5433 host=::1"
                    `shouldBe` Right ConnectInfo
                                   { connectHost     = "::1"
                                   , connectPort     = 5433
                                   , connectUser     = "postgres"
                                   , connectPassword = ""
                                   , connectDatabase = "codd-experiments"
                                   }

                -- Only mandatory arguments
                parseOnly
                        (connStringParser <* endOfInput)
                        "dbname=codd-experiments user=postgres host=127.0.0.1"
                    `shouldBe` Right ConnectInfo
                                   { connectHost     = "127.0.0.1"
                                   , connectPort     = 5432
                                   , connectUser     = "postgres"
                                   , connectPassword = ""
                                   , connectDatabase = "codd-experiments"
                                   }

            it "Randomized valid connection strings" $ do
                property $ \(ConnStringGen connStr connInfo) ->
                    parseOnly (connStringParser <* endOfInput) connStr
                        `shouldBe` Right connInfo
        context "Retry policy parsing" $ do
            it "Invalid policy strings" $ do
                parseOnly (retryPolicyParser <* endOfInput)
                          "max 1 backoff exponential 2"
                    `shouldSatisfy` isLeft

                parseOnly (retryPolicyParser <* endOfInput)
                          "max -1 backoff exponential 2s"
                    `shouldSatisfy` isLeft
            it "Valid policy strings" $ do
                parseOnly (retryPolicyParser <* endOfInput)
                          "max 0 backoff exponential 2s"
                    `shouldBe` Right
                                   (RetryPolicy
                                       0
                                       (ExponentialBackoff (secondsToDiffTime 2)
                                       )
                                   )

                parseOnly (retryPolicyParser <* endOfInput)
                          "max 2 backoff exponential 2.5s"
                    `shouldBe` Right
                                   (RetryPolicy
                                       2
                                       (ExponentialBackoff (realToFrac 2.5))
                                   )

                parseOnly (retryPolicyParser <* endOfInput)
                          "max 7 backoff constant 1250ms"
                    `shouldBe` Right
                                   (RetryPolicy
                                       7
                                       (ConstantBackoff (realToFrac 1.25))
                                   )
