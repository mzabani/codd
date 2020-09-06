module TypesGen where

import Codd.Hashing (readHashesFromDisk, persistHashesToDisk, DbHashes(..), SchemaHash(..), SchemaObjectHash(..), TableColumn(..), TableConstraint(..), ObjHash(..), ObjName(..), objName)
import Data.Function (on)
import Data.List (nubBy)
import qualified Data.Text as Text
import Test.QuickCheck

newtype DbHashesGen = DbHashesGen { unDbHashesGen :: DbHashes } deriving stock Show

instance Arbitrary DbHashesGen where
    arbitrary = DbHashesGen . DbHashes <$> uniqueListOf 3 schemaHashGen objName
        where
            schemaHashGen = SchemaHash <$> genObjName <*> genObjHash <*> uniqueListOf 100 schemaObjGen objName
            schemaObjGen =
                oneof [
                    TableHash <$> genObjName <*> genObjHash <*> uniqueListOf 20 colGen objName <*> uniqueListOf 5 constraintGen objName
                    , ViewHash <$> genObjName <*> genObjHash
                    , RoutineHash <$> genObjName <*> genObjHash
                    , SequenceHash <$> genObjName <*> genObjHash
                ]
            colGen = TableColumn <$> genObjName <*> genObjHash
            constraintGen = TableConstraint <$> genObjName <*> genObjHash

uniqueListOf :: Eq b => Int -> Gen a -> (a -> b) -> Gen [a]
uniqueListOf size gen uniqBy = nubBy ((==) `on` uniqBy) <$> resize size (listOf gen)

genObjName :: Gen ObjName
-- TODO: freq > 0 genNasty
genObjName = ObjName . Text.pack <$> frequency [(100, genLower), (5, genMixed), (0, genNastyIdentifier)]
    where
        -- Docs: https://www.postgresql.org/docs/12/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
        validLowerFirstChars = "abcdefghijklmnopqrstuvxwyzçáéíóúñ_"
        validUpperFirstChars = "ABCDEFGHIJKLMNOPQRSTUVXWYZÇÁÉÍÓÚñ_"
        validLowerOtherChars = validLowerFirstChars ++ "0123456789$"
        validUpperOtherChars = validUpperFirstChars ++ "0123456789$"
        genLower = do
            c <- elements validLowerFirstChars
            -- Max Length 63 bytes of UTF8-Encoded name
            r <- resize 62 $ listOf $ elements validLowerOtherChars
            pure $ c : r
        genMixed = do
            c <- elements $ validLowerFirstChars ++ validUpperFirstChars
            -- Max Length 63 bytes of UTF8-Encoded name
            r <- resize 62 $ listOf $ elements $ validLowerOtherChars ++ validUpperOtherChars
            pure $ c : r
        genNastyIdentifier = getPrintableString <$> arbitrary @PrintableString

genObjHash :: Gen ObjHash
genObjHash = ObjHash . Text.pack . getASCIIString <$> arbitrary @ASCIIString