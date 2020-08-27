module Main where

import qualified System.IO as IO
import qualified AllSpecs
import Test.Hspec

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering
    
    hspec AllSpecs.spec