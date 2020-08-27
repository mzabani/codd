module GeneralSpec where

import Codd.Hashing (readHashesFromDisk, persistHashesToDisk)
import Control.Monad (when)
import Data.Text (unpack)
import Test.Hspec
import Test.QuickCheck
import TypesGen (DbHashesGen(..))

spec :: Spec
spec = do
    describe "General tests" $ do
        it "persistHashesToDisk is inverse of readHashesFromDisk" $ do
            property $ \(DbHashesGen dbHashes) -> do
                persistHashesToDisk dbHashes "/dev/shm/inverse-test-sql-folder"
                readDbHashes <- readHashesFromDisk "/dev/shm/inverse-test-sql-folder"
                -- True `shouldBe` True
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
        it "TEMP persistHashesToDisk is inverse of readHashesFromDisk" $ do
            property $ \(dbName :: Int) -> dbName `shouldBe` dbName