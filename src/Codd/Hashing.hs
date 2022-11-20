module Codd.Hashing
    ( module Codd.Hashing.Database
    , module Codd.Hashing.Types
    , module Codd.Hashing.Disk
    , logChecksumsComparison
    , hashDifferences
    ) where

import           Codd.Hashing.Database          ( readHashesFromDatabase
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Hashing.Disk
import           Codd.Hashing.Types

import           Control.Monad.Logger           ( MonadLogger
                                                , logErrorN
                                                , logInfoN
                                                )
import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( mapMaybe )
import           Data.Text.Encoding             ( decodeUtf8 )

-- | Takes the DB and the expected hashes and logErrorN's the differences, if any,
-- or logInfoN that they match otherwise.
logChecksumsComparison
    :: MonadLogger m
    => DbHashes
    -- ^ Database hashes
    -> DbHashes
 -- ^ Expected hashes
    -> m ()
logChecksumsComparison dbHashes expectedChecksums =
    if dbHashes /= expectedChecksums
        then
        -- Urgh.. UTF-8 Text as output from Aeson would be perfect here..
        -- Also, we need to make sure we output as much as possible in a single
        -- line to be amenable to logging infrastructure
            logErrorN
            $  "DB and expected checksums do not match. Differences are: "
            <> decodeUtf8
                   (toStrict $ encode $ hashDifferences dbHashes
                                                        expectedChecksums
                   )
        else logInfoN "Database and expected schemas match."

hashDifferences :: DbHashes -> DbHashes -> Map FilePath DiffType
hashDifferences l r =
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

