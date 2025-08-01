module WritingReadingRepresentationsSpec where

import Codd.Representations
  ( DbRep,
    persistRepsToDisk,
    readRepsFromDisk,
    schemaDifferences,
  )
import Codd.Types (PgMajorVersion)
import qualified Data.Map as Map
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import DbUtils (getEmptyTempDir)
import System.FilePath
  ( (</>),
  )
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import TypesGen (DbRepsGen (..))
import UnliftIO (MonadIO (..))
import UnliftIO.Directory (createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, emptyPermissions, getSymbolicLinkTarget, getTemporaryDirectory, pathIsSymbolicLink, setOwnerExecutable, setOwnerReadable, setPermissions)

spec :: Spec
spec = do
  describe "Writing and reading representations" $ do
    it "persistRepsToDisk is inverse of readRepsFromDisk" $ do
      property $ \(DbRepsGen dbHashes pgVersion) -> do
        -- /dev/shm is shared memory so should be faster, if it exists (MacOS doesn't have it)
        shmExists <- doesDirectoryExist "/dev/shm"
        baseFolder :: FilePath <- if shmExists then pure "/dev/shm" else getEmptyTempDir
        writeSchemaAndReadSchemaRoundtrip pgVersion dbHashes (baseFolder </> "inverse-test-sql-folder")
    modifyMaxSuccess (const 1)
      $ it
        "persistRepsToDisk works when expected schema dir does not exist"
      $ do
        property $ \(DbRepsGen dbHashes pgVersion) -> do
          tempDir <- getEmptyTempDir
          let expectedSchemaDir = tempDir </> "parentfolder/expected-schema"
          writeSchemaAndReadSchemaRoundtrip pgVersion dbHashes expectedSchemaDir
    modifyMaxSuccess (const 1)
      $ it
        "persistRepsToDisk works even when expected schema dir parent has difficult permissions"
      $ do
        property $ \(DbRepsGen dbHashes pgVersion) -> do
          tempDir <- getEmptyTempDir
          let expectedSchemaDir = tempDir </> "badfolder/badsubfolder/expected-schema"
          createDirectoryIfMissing True expectedSchemaDir
          let permsNoWrite = setOwnerReadable True $ setOwnerExecutable True emptyPermissions
          setPermissions (tempDir </> "badfolder/badsubfolder") permsNoWrite
          setPermissions (tempDir </> "badfolder") permsNoWrite
          writeSchemaAndReadSchemaRoundtrip
            pgVersion
            dbHashes
            expectedSchemaDir
    modifyMaxSuccess (const 1)
      $ it
        "persistRepsToDisk preserves symlinks"
      $ do
        property $ \(DbRepsGen dbHashes pgVersion) -> do
          tempDir <- getEmptyTempDir
          let realExpectedSchemaDir = tempDir </> "real-expected-schema"
              linkToSchemaDir = tempDir </> "link-to-schema-dir"
          createDirectoryIfMissing True realExpectedSchemaDir
          createDirectoryLink realExpectedSchemaDir linkToSchemaDir
          writeSchemaAndReadSchemaRoundtrip
            pgVersion
            dbHashes
            linkToSchemaDir
          pathIsSymbolicLink linkToSchemaDir `shouldReturn` True
          getSymbolicLinkTarget linkToSchemaDir `shouldReturn` realExpectedSchemaDir

writeSchemaAndReadSchemaRoundtrip :: PgMajorVersion -> DbRep -> FilePath -> IO ()
writeSchemaAndReadSchemaRoundtrip pgVersion dbReps expectedSchemaDir = do
  persistRepsToDisk pgVersion dbReps expectedSchemaDir
  readDbSchema <-
    readRepsFromDisk
      pgVersion
      expectedSchemaDir
  let diffs = schemaDifferences dbReps readDbSchema
  diffs `shouldBe` Map.empty
  readDbSchema `shouldBe` dbReps
