module WritingReadingRepresentationsSpec where

import Codd.Representations
  ( persistRepsToDisk,
    readRepsFromDisk,
    schemaDifferences,
  )
import qualified Data.Map as Map
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import TypesGen (DbRepsGen (..))

spec :: Spec
spec = do
  describe "Writing and reading representations" $ do
    it "persistRepsToDisk is inverse of readRepsFromDisk" $ do
      property $ \(DbRepsGen dbHashes) -> do
        persistRepsToDisk dbHashes "/dev/shm/inverse-test-sql-folder"
        readDbHashes <-
          readRepsFromDisk
            "/dev/shm/inverse-test-sql-folder"
        let diffs = schemaDifferences dbHashes readDbHashes
        diffs `shouldBe` Map.empty
        readDbHashes `shouldBe` dbHashes
