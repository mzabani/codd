module Codd.Representations
    ( module Codd.Representations.Database
    , module Codd.Representations.Types
    , module Codd.Representations.Disk
    , logSchemasComparison
    , schemaDifferences
    ) where

import           Codd.Representations.Database  ( readRepresentationsFromDbWithSettings
                                                , readSchemaFromDatabase
                                                )
import           Codd.Representations.Disk
import           Codd.Representations.Types

import           Control.Monad.Logger           ( MonadLogger
                                                , logErrorN
                                                , logInfoN
                                                )
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( mapMaybe )

-- | Takes the DB and the expected schemas and logErrorN's the differences, if any,
-- or logInfoN that they match otherwise.
logSchemasComparison
    :: MonadLogger m
    => DbRep
    -- ^ Database schema
    -> DbRep
    -- ^ Expected schema
    -> m ()
logSchemasComparison dbSchema expectedSchemas = if dbSchema /= expectedSchemas
    then
        logErrorN
        $ "DB and expected schemas do not match. Differing objects and their current DB schemas are: "
        <> detEncodeSingleLineJSON (schemaDifferences dbSchema expectedSchemas)
    else logInfoN
        "Comparing actual and expected schemas... [<GREEN>match</GREEN>]"

schemaDifferences :: DbRep -> DbRep -> Map FilePath DiffType
schemaDifferences l r =
    let matches = matchOrd fst (toFiles l) (toFiles r)
    in  Map.fromList $ mapMaybe
            (\case
                (Nothing, Nothing) -> Nothing
                (Just (name, foundInDB), Nothing) ->
                    Just (name, NotExpectedButFound foundInDB)
                (Nothing, Just (name, _)) -> Just (name, ExpectedButNotFound)
                (Just (name, foundInDB), Just (_, expected))
                    | foundInDB == expected -> Nothing
                    | otherwise -> Just (name, BothButDifferent foundInDB)
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

