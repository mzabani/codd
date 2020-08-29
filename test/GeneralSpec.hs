module GeneralSpec where

import Codd.Hashing (readHashesFromDisk, persistHashesToDisk)
import Control.Monad (when)
import Data.Text (unpack)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import TypesGen (DbHashesGen(..))

spec :: Spec
spec = do
    describe "General tests" $ do
        it "persistHashesToDisk is inverse of readHashesFromDisk" $ do
            property $ \(DbHashesGen dbHashes) -> do
                persistHashesToDisk dbHashes "/dev/shm/inverse-test-sql-folder"
                readDbHashes <- readHashesFromDisk "/dev/shm/inverse-test-sql-folder"
                case readDbHashes of
                    Left err -> error $ unpack err
                    Right x ->
                        when (x /= dbHashes) $ do
                            putStrLn "Hashes differ!"
                            putStrLn "Randomly generated hashes (expected):"
                            print dbHashes
                            putStrLn "Hashes read from disk"
                            print x
                            error "Failed!"
        modifyMaxSuccess (const 1) $
            it "persistHashesToDisk works even when temporary folder and final folder are in separate filesystems" $ do
                -- We don't know where the temporary folder will be, but it'll either be in /dev/shm or it won't,
                -- and /dev/shm is (hopefully) always a separately mounted tmpfs
                property $ \(DbHashesGen dbHashes) -> persistHashesToDisk @IO dbHashes "/tmp/some-or-any-codd-folder"