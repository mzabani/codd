module Main where

import qualified AllSpecs
import qualified System.IO                     as IO
import           Test.Hspec

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering

    hspec AllSpecs.spec
