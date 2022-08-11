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
import           Data.Attoparsec.Text           ( endOfInput
                                                , parseOnly
                                                )
import           Data.Either                    ( isLeft )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Time                      ( secondsToDiffTime )
import           Data.Word                      ( Word16 )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import GHC.Generics (Generic)
import Network.URI (URI(..), URIAuth(..), escapeURIString, isUnescapedInURIComponent, uriToString)
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

data ConnStringType = URIpostgres | URIpostgresql
    deriving stock (Bounded, Enum, Show)

instance Arbitrary ConnStringType where
    arbitrary = elements [minBound .. maxBound]

getConnString
    :: ConnStringType -> ObjName -> PrintableString -> HostString -> Word16 -> ObjName -> Text
getConnString connStrType (Text.unpack . unObjName -> usr) (getPrintableString -> pwd) (unHostString -> host) port (Text.unpack . unObjName -> dbName)
    = Text.pack $ uriToString id uri ""
  where
    uri = URI {
        uriScheme = case connStrType of
            URIpostgres -> "postgres:"
            URIpostgresql -> "postgresql:"
        , uriAuthority = Just URIAuth {
            uriUserInfo = encodeURIComponent usr <> (if pwd == "" then "" else (':' : encodeURIComponent pwd)) <> "@"
            , uriRegName = Text.unpack $ escapeHost $ Text.pack host
            , uriPort = ':' : show port
        }
        , uriPath = '/' : encodeURIComponent dbName
        , uriQuery = ""
        , uriFragment = ""
    }
    encodeURIComponent :: String -> String
    encodeURIComponent = escapeURIString isUnescapedInURIComponent
    escapeHost :: Text -> Text
    escapeHost host =
        -- bracket-escape IPv6 addresses
        if ":" `Text.isInfixOf` host then "[" <> host <> "]" else host

data ConnStringGen = ConnStringGen Text ConnectInfo
    deriving stock Show
instance Arbitrary ConnStringGen where
    arbitrary = do
        usr <- genObjName
        pwd <-
            arbitrary @PrintableString
                `suchThat` (\(PrintableString s) -> '\n' `notElem` s)
        host   <- arbitrary @HostString
        port   <- arbitrary @Word16
        dbName <- genObjName
        connStringType <- arbitrary @ConnStringType
        pure $ ConnStringGen
            (getConnString connStringType usr pwd host port dbName)
            ConnectInfo { connectHost     = unHostString host
                        , connectPort     = port
                        , connectUser     = Text.unpack $ unObjName usr
                        , connectPassword = getPrintableString pwd
                        , connectDatabase = Text.unpack $ unObjName dbName
                        }



spec :: Spec
spec = do
    describe "Environment tests" $ do
        context "Connection string parsing" $ do
            it "Some hard coded URI Encoded connection strings" $ do
                -- These are hard coded connection strings that I've tested with psql at some point
                parseOnly (connStringParser <* endOfInput) "postgresql://postgres@localhost/some%20thing%20%2F%3F%3A%40"
                        `shouldBe` Right ConnectInfo {
                            connectHost = "localhost"
                            , connectPort = 5432
                            , connectUser = "postgres"
                            , connectPassword = ""
                            , connectDatabase = "some thing /?:@"
                        }
                parseOnly (connStringParser <* endOfInput) "postgresql://some%20thing%20%2F%3F%3A%40:passwdsome%20thing%20%2F%3F%3A%40@localhost:1/some%20thing%20%2F%3F%3A%40"
                        `shouldBe` Right ConnectInfo {
                            connectHost = "localhost"
                            , connectPort = 1
                            , connectUser = "some thing /?:@"
                            , connectPassword = "passwdsome thing /?:@"
                            , connectDatabase = "some thing /?:@"
                        }
            
            it "Full connection string with password" $ do
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
