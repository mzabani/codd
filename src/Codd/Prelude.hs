module Codd.Prelude where

import qualified Data.List.NonEmpty as NE

nubOrd :: Ord a => [a] -> [a]
nubOrd = map NE.head . NE.groupAllWith id