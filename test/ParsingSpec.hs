module ParsingSpec where

import Codd.Parsing (parseSqlMigration)
import Codd.Types (SqlMigration(..))
import Control.Monad (when)
import Data.Either (isLeft)
import qualified Data.Text as Text
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

newtype RandomSql = RandomSql { unRandomSql :: Text } deriving newtype Show
instance Arbitrary RandomSql where
    arbitrary = fmap RandomSql $ frequency [
            (1, pure "")
            , (50, randomSqlGen)
            ]

        where
            semiCommaGen = frequency [(5, pure ";"), (1, pure "")]
            lineGen = Text.pack . getUnicodeString <$> arbitrary
            -- Note: the likelihood that QuickCheck will randomly generate text that has a line starting with "-- codd:"
            -- is so low that we can just ignore it
            commentGen = ("-- " <>) <$> lineGen
            lineOrCommentGen = frequency [ (5, lineGen), (1, commentGen) ]
            randomSqlGen = Text.concat <$> listOf1 lineOrCommentGen

spec :: Spec
spec = do
    describe "Parsing tests" $ do
        context "Valid SQL Migrations" $ do
            it "Sql Migration with two sections, missing optional options" $ do
                property $ \(unRandomSql -> nonDestSql, unRandomSql -> destSql) ->
                    let
                        sql = "-- codd: non-destructive\n"
                            <> nonDestSql
                            <> "\n-- codd: destructive\n"
                            <> destSql
                    in
                        parseSqlMigration sql `shouldBe` Right SqlMigration {
                            nonDestructiveSql = nonDestSql
                            , nonDestructiveForce = False
                            , nonDestructiveInTxn = True
                            , destructiveSql = destSql
                            , destructiveInTxn = True
                        }
            it "Sql Migration with one section, missing optional options" $ do
                property $ \(unRandomSql -> sectionSql, nonDest :: Bool) ->
                    let
                        sql = (if nonDest then "-- codd: non-destructive\n" else "-- codd: destructive\n")
                                <> sectionSql
                        (nonDestSql, destSql) = if nonDest then (sectionSql, "") else ("", sectionSql)
                    in
                        parseSqlMigration sql `shouldBe` Right SqlMigration {
                            nonDestructiveSql = nonDestSql
                            , nonDestructiveForce = False
                            , nonDestructiveInTxn = True
                            , destructiveSql = destSql
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
                    parseSqlMigration sql `shouldBe` Right SqlMigration {
                            nonDestructiveSql = nonDestSql
                            , nonDestructiveForce = True
                            , nonDestructiveInTxn = True
                            , destructiveSql = destSql
                            , destructiveInTxn = False
                        }
        
        context "Invalid SQL Migrations" $ do
            it "Sql Migration Parser never blocks for random text" $ do
                property $ \(unRandomSql -> anyText) ->
                    parseSqlMigration anyText `shouldSatisfy` const True

            it "Gibberish after -- codd:" $
                let
                    sql = "-- codd: complete gibberish\n"
                        <> "ANY SQL HERE"
                in
                    parseSqlMigration sql `shouldSatisfy` isLeft
            
            it "Duplicate options" $
                let
                    sql = "-- codd: force, non-destructive, in-txn, non-destructive\n"
                        <> "ANY SQL HERE"
                in
                    parseSqlMigration sql `shouldSatisfy` \case
                                                                Right _ -> False
                                                                Left err -> "duplicate" `Text.isInfixOf` err
            it "Unknown / mistyped options" $
                let
                    sql = "-- codd: force, NON-Destructive, in-txn\n"
                        <> "ANY SQL HERE"
                in
                    parseSqlMigration sql `shouldSatisfy` isLeft

            it "Missing non-destructive and destructive, otherwise valid options" $
                let
                    sql = "-- codd: in-txn\n"
                        <> "ANY SQL HERE"
                in
                    parseSqlMigration sql `shouldSatisfy` isLeft

            it "Two non-destructive sections" $
                let
                    sql = "-- codd: non-destructive\n"
                        <> "ANY SQL HERE"
                        <> "\n--codd: non-destructive\n"
                        <> "MORE SQL HERE"
                in
                    parseSqlMigration sql `shouldSatisfy` isLeft

            it "Two destructive sections" $
                let
                    sql = "-- codd: destructive\n"
                        <> "ANY SQL HERE"
                        <> "\n--codd: destructive\n"
                        <> "MORE SQL HERE"
                in
                    parseSqlMigration sql `shouldSatisfy` isLeft