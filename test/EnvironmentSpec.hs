module EnvironmentSpec where

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
import           Data.Time                      ( secondsToNominalDiffTime )
import           Data.Word                      ( Word16 )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
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

-- TODO: HostString ipv6
newtype HostString = HostString { unHostString :: String }
    deriving stock Show
instance Arbitrary HostString where
    arbitrary =
        HostString
            .          getASCIIString
            <$>        arbitrary @ASCIIString
            `suchThat` (/= ASCIIString "")

getConnString
    :: ObjName -> PrintableString -> HostString -> Word16 -> ObjName -> Text
getConnString (unObjName -> usr) (Text.pack . getPrintableString -> pwd) (Text.pack . unHostString -> host) (Text.pack . show -> port) (unObjName -> dbName)
    = "postgres://"
        <> escapeConnStringPiece usr
        <> (if pwd == "" then "" else ":" <> escapeConnStringPiece pwd)
        <> "@"
        <> escapeConnStringPiece host
        <> ":"
        <> port
        <> "/"
        <> escapeConnStringPiece dbName

escapeConnStringPiece :: Text -> Text
escapeConnStringPiece =
    Text.replace "@" "\\@" . Text.replace ":" "\\:" . Text.replace "\\" "\\\\"

data ConnStringGen = ConnStringGen Text ConnectInfo
    deriving stock Show
instance Arbitrary ConnStringGen where
    arbitrary = do
        usr    <- genObjName
        pwd    <- arbitrary @PrintableString
        host   <- arbitrary @HostString
        port   <- arbitrary @Word16
        dbName <- genObjName
        pure $ ConnStringGen
            (getConnString usr pwd host port dbName)
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
                                       (ExponentialBackoff
                                           (secondsToNominalDiffTime 2)
                                       )
                                   )

                parseOnly (retryPolicyParser <* endOfInput)
                          "max 2 backoff exponential 2.5s"
                    `shouldBe` Right
                                   (RetryPolicy
                                       2
                                       (ExponentialBackoff
                                           (secondsToNominalDiffTime 2.5)
                                       )
                                   )

                parseOnly (retryPolicyParser <* endOfInput)
                          "max 7 backoff constant 1250ms"
                    `shouldBe` Right
                                   (RetryPolicy
                                       7
                                       (ConstantBackoff
                                           (secondsToNominalDiffTime 1.25)
                                       )
                                   )
