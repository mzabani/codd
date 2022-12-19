module WritingReadingRepresentationsSpec where

import           Codd.Representations           ( persistRepsToDisk
                                                , readRepsFromDisk
                                                , schemaDifferences
                                                )
import           Control.Monad                  ( when )
import qualified Data.Map                      as Map
import           Data.Text                      ( unpack )
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSuccess )
import           Test.QuickCheck
import           TypesGen                       ( DbRepsGen(..) )

spec :: Spec
spec = do
    describe "Writing and reading representations" $ do
        it "persistRepsToDisk is inverse of readRepsFromDisk" $ do
            property $ \(DbRepsGen dbHashes) -> do
                persistRepsToDisk dbHashes "/dev/shm/inverse-test-sql-folder"
                readDbHashes <- readRepsFromDisk
                    "/dev/shm/inverse-test-sql-folder"
                let diffs = schemaDifferences dbHashes readDbHashes
                diffs `shouldBe` Map.empty
                readDbHashes `shouldBe` dbHashes
        modifyMaxSuccess (const 1)
            $ it
                  "persistRepsToDisk works even when temporary folder and final folder are in separate filesystems"
            $ do
                -- We don't know where the temporary folder will be, but it'll either be in /dev/shm or it won't,
                -- and /dev/shm is (hopefully) always a separately mounted tmpfs
                  property $ \(DbRepsGen dbHashes) -> persistRepsToDisk @IO
                      dbHashes
                      "/tmp/some-or-any-codd-folder"
