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
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, emptyPermissions, getTemporaryDirectory, setOwnerExecutable, setOwnerReadable, setPermissions)

spec :: Spec
spec = do
  describe "Writing and reading representations" $ do
    it "persistRepsToDisk is inverse of readRepsFromDisk" $ do
      property $ \(DbRepsGen dbHashes) -> do
        -- /dev/shm is shared memory so should be faster, if it exists (MacOS doesn't have it)
        shmExists <- doesDirectoryExist "/dev/shm"
        let baseFolder = if shmExists then "/dev/shm" else "/tmp"
        persistRepsToDisk dbHashes (baseFolder </> "inverse-test-sql-folder")
        readDbHashes <-
          readRepsFromDisk
            (baseFolder </> "inverse-test-sql-folder")
        let diffs = schemaDifferences dbHashes readDbHashes
        diffs `shouldBe` Map.empty
        readDbHashes `shouldBe` dbHashes
    modifyMaxSuccess (const 1)
      $ it
        "persistRepsToDisk works even when expected schema dir parent has difficult permissions"
      $ do
        property $ \(DbRepsGen dbHashes) -> do
          tempDir <- getTemporaryDirectory
          let expectedSchemaDir = tempDir </> "badfolder/badsubfolder/expected-schema"
          createDirectoryIfMissing True expectedSchemaDir
          let permsNoWrite = setOwnerReadable True $ setOwnerExecutable True emptyPermissions
          setPermissions (tempDir </> "badfolder/badsubfolder") permsNoWrite
          setPermissions (tempDir </> "badfolder") permsNoWrite
          persistRepsToDisk @IO
            dbHashes
            expectedSchemaDir
