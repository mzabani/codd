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
getConnString connStrType (unObjName -> usr) (Text.pack . getPrintableString -> pwd) (Text.pack . unHostString -> host) (Text.pack . show -> port) (unObjName -> dbName)
    = (case connStrType of
            URIpostgres -> "postgres://"
            URIpostgresql -> "postgresql://"
        )
        <> escapeConnStringPiece usr
        <> (if pwd == "" then "" else ":" <> escapeConnStringPiece pwd)
        <> "@"
        <> escapeHost host
        <> ":"
        <> port
        <> "/"
        <> escapeConnStringPiece dbName
  where
    escapeHost :: Text -> Text
    escapeHost host =
        -- bracket-escape IPv6 addresses
        if ":" `Text.isInfixOf` host then "[" <> host <> "]" else host
    escapeConnStringPiece :: Text -> Text
    escapeConnStringPiece =
        Text.replace "@" "\\@" . Text.replace ":" "\\:" . Text.replace
            "\\"
            "\\\\"

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
            -- protocol://username[:password]@host:port/database_name
            -- Escape chars such as :, @ and even \ with a backslash. E.g. \:, \@ and \\
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
