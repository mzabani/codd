module WritingReadingChecksumsSpec where

import           Codd.Hashing                   ( hashDifferences
                                                , persistHashesToDisk
                                                , readHashesFromDisk
                                                )
import           Control.Monad                  ( when )
import qualified Data.Map                      as Map
import           Data.Text                      ( unpack )
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck     ( modifyMaxSuccess )
import           Test.QuickCheck
import           TypesGen                       ( DbHashesGen(..) )

spec :: Spec
spec = do
    describe "Writing and reading checksums" $ do
        it "persistHashesToDisk is inverse of readHashesFromDisk" $ do
            property $ \(DbHashesGen dbHashes) -> do
                persistHashesToDisk dbHashes "/dev/shm/inverse-test-sql-folder"
                readDbHashes <- readHashesFromDisk
                    "/dev/shm/inverse-test-sql-folder"
                let diffs = hashDifferences dbHashes readDbHashes
                diffs `shouldBe` Map.empty
                readDbHashes `shouldBe` dbHashes
        modifyMaxSuccess (const 1)
            $ it
                  "persistHashesToDisk works even when temporary folder and final folder are in separate filesystems"
            $ do
                -- We don't know where the temporary folder will be, but it'll either be in /dev/shm or it won't,
                -- and /dev/shm is (hopefully) always a separately mounted tmpfs
                  property $ \(DbHashesGen dbHashes) -> persistHashesToDisk @IO
                      dbHashes
                      "/tmp/some-or-any-codd-folder"
