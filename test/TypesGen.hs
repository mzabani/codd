module TypesGen where

import Codd.Representations
import Codd.Types (PgMajorVersion (..))
import Data.Function (on)
import Data.List (nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Test.QuickCheck

data DbRepsGen = DbRepsGen {unDbRepsGen :: DbRep, pgVersion :: PgMajorVersion} deriving stock (Show)

instance Arbitrary DbRepsGen where
  arbitrary =
    let repsGen =
          DbRep
            <$> arbitrary
            <*> uniqueMapOf 3 schemaHashGen objName
            <*> uniqueMapOf 2 roleHashGen objName
        versionGen = PgMajorVersion <$> arbitrary
     in DbRepsGen <$> repsGen <*> versionGen
    where
      schemaHashGen =
        SchemaRep
          <$> genObjName
          <*> arbitrary
          <*> uniqueMapOf 20 tableGen objName
          <*> uniqueMapOf 5 viewGen objName
          <*> uniqueMapOf 10 routineGen objName
          <*> uniqueMapOf 15 sequenceGen objName
          <*> uniqueMapOf 2 collationGen objName
          <*> uniqueMapOf 5 typeGen objName
      roleHashGen = RoleRep <$> genObjName <*> arbitrary

      -- Per-schema object generators
      tableGen =
        TableRep
          <$> genObjName
          <*> arbitrary
          <*> uniqueMapOf 20 colGen objName
          <*> uniqueMapOf 5 constraintGen objName
          <*> uniqueMapOf 1 triggerGen objName
          <*> uniqueMapOf 2 policyGen objName
          <*> uniqueMapOf 3 indexGen objName
          <*> uniqueMapOf 1 stxGen objName
      viewGen = ViewRep <$> genObjName <*> arbitrary
      routineGen = RoutineRep <$> genObjName <*> arbitrary
      sequenceGen = SequenceRep <$> genObjName <*> arbitrary
      collationGen = CollationRep <$> genObjName <*> arbitrary
      typeGen = TypeRep <$> genObjName <*> arbitrary

      -- Per-table object generators
      colGen = TableColumnRep <$> genObjName <*> arbitrary
      constraintGen = TableConstraintRep <$> genObjName <*> arbitrary
      triggerGen = TableTriggerRep <$> genObjName <*> arbitrary
      policyGen = TablePolicyRep <$> genObjName <*> arbitrary
      indexGen = TableIndexRep <$> genObjName <*> arbitrary
      stxGen = TableStatisticsRep <$> genObjName <*> arbitrary

uniqueListOf :: (Eq b) => Int -> Gen a -> (a -> b) -> Gen [a]
uniqueListOf size gen uniqBy =
  nubBy ((==) `on` uniqBy) <$> resize size (listOf gen)

uniqueMapOf :: (Ord k) => Int -> Gen a -> (a -> k) -> Gen (Map k a)
uniqueMapOf size gen uniqBy =
  Map.fromList . map (\v -> (uniqBy v, v)) <$> resize size (listOf gen)

genObjName :: Gen ObjName
genObjName =
  ObjName . Text.pack
    <$> frequency
      -- TODO: freq > 0 genNasty
      [(100, genLower), (5, genMixed), (0, genNastyIdentifier)]
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
      r <-
        resize 62 $
          listOf $
            elements $
              validLowerOtherChars
                ++ validUpperOtherChars
      pure $ c : r
    genNastyIdentifier = getPrintableString <$> arbitrary @PrintableString
