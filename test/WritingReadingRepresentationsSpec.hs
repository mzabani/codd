module WritingReadingRepresentationsSpec where

import Codd.Representations
  ( DbRep,
    persistRepsToDisk,
    readRepsFromDisk,
    schemaDifferences,
  )
import qualified Data.Map as Map
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import System.FilePath
  ( (</>),
  )
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import TypesGen (DbRepsGen (..))
import UnliftIO (MonadIO (..))
import UnliftIO.Directory (createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, emptyPermissions, getSymbolicLinkTarget, getTemporaryDirectory, pathIsSymbolicLink, setOwnerExecutable, setOwnerReadable, setPermissions)

getEmptyTempDir :: (MonadIO m) => m FilePath
getEmptyTempDir = do
  tmp <- getTemporaryDirectory
  complement :: UUID <- liftIO UUIDv4.nextRandom
  let emptyDir = tmp </> UUID.toString complement
  createDirectoryIfMissing True emptyDir
  pure emptyDir

spec :: Spec
spec = do
  describe "Writing and reading representations" $ do
    it "persistRepsToDisk is inverse of readRepsFromDisk" $ do
      property $ \(DbRepsGen dbHashes) -> do
        -- /dev/shm is shared memory so should be faster, if it exists (MacOS doesn't have it)
        shmExists <- doesDirectoryExist "/dev/shm"
        let baseFolder = if shmExists then "/dev/shm" else "/tmp"
        writeSchemaAndReadSchemaRoundtrip dbHashes (baseFolder </> "inverse-test-sql-folder")
    modifyMaxSuccess (const 1)
      $ it
        "persistRepsToDisk works when expected schema dir does not exist"
      $ do
        property $ \(DbRepsGen dbHashes) -> do
          tempDir <- getEmptyTempDir
          let expectedSchemaDir = tempDir </> "parentfolder/expected-schema"
          writeSchemaAndReadSchemaRoundtrip dbHashes expectedSchemaDir
    modifyMaxSuccess (const 1)
      $ it
        "persistRepsToDisk works even when expected schema dir parent has difficult permissions"
      $ do
        property $ \(DbRepsGen dbHashes) -> do
          tempDir <- getEmptyTempDir
          let expectedSchemaDir = tempDir </> "badfolder/badsubfolder/expected-schema"
          createDirectoryIfMissing True expectedSchemaDir
          let permsNoWrite = setOwnerReadable True $ setOwnerExecutable True emptyPermissions
          setPermissions (tempDir </> "badfolder/badsubfolder") permsNoWrite
          setPermissions (tempDir </> "badfolder") permsNoWrite
          writeSchemaAndReadSchemaRoundtrip
            dbHashes
            expectedSchemaDir
    modifyMaxSuccess (const 1)
      $ it
        "persistRepsToDisk preserves symlinks"
      $ do
        property $ \(DbRepsGen dbHashes) -> do
          tempDir <- getEmptyTempDir
          let realExpectedSchemaDir = tempDir </> "real-expected-schema"
              linkToSchemaDir = tempDir </> "link-to-schema-dir"
          createDirectoryIfMissing True realExpectedSchemaDir
          createDirectoryLink realExpectedSchemaDir linkToSchemaDir
          writeSchemaAndReadSchemaRoundtrip
            dbHashes
            linkToSchemaDir
          pathIsSymbolicLink linkToSchemaDir `shouldReturn` True
          getSymbolicLinkTarget linkToSchemaDir `shouldReturn` realExpectedSchemaDir

writeSchemaAndReadSchemaRoundtrip :: DbRep -> FilePath -> IO ()
writeSchemaAndReadSchemaRoundtrip dbReps expectedSchemaDir = do
  persistRepsToDisk dbReps expectedSchemaDir
  readDbSchema <-
    readRepsFromDisk
      expectedSchemaDir
  let diffs = schemaDifferences dbReps readDbSchema
  diffs `shouldBe` Map.empty
  readDbSchema `shouldBe` dbReps
