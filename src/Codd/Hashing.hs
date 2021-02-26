module Codd.Hashing
    ( module Codd.Hashing.Database
    , module Codd.Hashing.Types
    , module Codd.Hashing.Disk
    , hashDifferences
    , matchOrd
    ) where

import           Codd.Hashing.Database
import           Codd.Hashing.Disk
import           Codd.Hashing.Types

import           Data.List                      ( sortOn )
import           Data.Maybe                     ( mapMaybe )

hashDifferences :: DbHashes -> DbHashes -> [HashDiff]
hashDifferences l r =
    let matches = matchOrd fst (toFiles l) (toFiles r)
    in  mapMaybe
            (\case
                (Nothing, Nothing) -> Nothing
                (Just (name, _), Nothing) -> Just $ HashDiff name OnlyLeft
                (Nothing, Just (name, _)) -> Just $ HashDiff name OnlyRight
                (Just (name, h1), Just (_, h2))
                    | h1 == h2  -> Nothing
                    | otherwise -> Just $ HashDiff name BothButDifferent
            )
            matches



matchOrd :: Ord b => (a -> b) -> [a] -> [a] -> [(Maybe a, Maybe a)]
matchOrd f (sortOn f -> lsub) (sortOn f -> lsup) = go lsub lsup
  where
    go []       []       = []
    go (x : xs) []       = (Just x, Nothing) : go xs []
    go []       (y : ys) = (Nothing, Just y) : go [] ys
    go (x : xs) (y : ys) =
        let xf = f x
            yf = f y
        in  if xf < yf
                then (Just x, Nothing) : go xs (y : ys)
                else if xf == yf
                    then (Just x, Just y) : go xs ys
                    else (Nothing, Just y) : go (x : xs) ys
