module WritingReadingRepresentationsSpec where

import Codd.Representations
  ( persistRepsToDisk,
    readRepsFromDisk,
    schemaDifferences,
  )
import qualified Data.Map as Map
import System.FilePath
  ( (</>),
  )
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import TypesGen (DbRepsGen (..))
import UnliftIO.Directory (doesDirectoryExist)

spec :: Spec
spec = do
  describe "Writing and reading representations" $ do
    it "persistRepsToDisk is inverse of readRepsFromDisk" $ do
      property $ \(DbRepsGen dbHashes) -> do
        shmExists <- doesDirectoryExist "/dev/shm"
        let baseFolder = if shmExists then "/dev/shm" else "/tmp"
        persistRepsToDisk dbHashes (baseFolder </> "inverse-test-sql-folder")
        readDbHashes <-
          readRepsFromDisk
            (baseFolder </> "inverse-test-sql-folder")
        let diffs = schemaDifferences dbHashes readDbHashes
        diffs `shouldBe` Map.empty
        readDbHashes `shouldBe` dbHashes
