module Codd.Hashing
    ( module Codd.Hashing.Database
    , module Codd.Hashing.Types
    , module Codd.Hashing.Disk
    , logHashDifferences
    , hashDifferences
    , matchOrd
    ) where

import           Codd.Hashing.Database          ( HashReq(..)
                                                , Haxl
                                                , HaxlEnv
                                                , PgVersion
                                                , QueryInPieces(..)
                                                , SameQueryFormatFetch2(..)
                                                , State(..)
                                                , getSchemaHash
                                                , getTablesHashes
                                                , queryInPiecesToQueryFrag
                                                , readHashesFromDatabase
                                                , readHashesFromDatabaseWithSettings
                                                )
import           Codd.Hashing.Disk
import           Codd.Hashing.Types

import           Control.Monad                  ( when )
import           Control.Monad.Logger           ( MonadLogger
                                                , logErrorN
                                                )
import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( mapMaybe )
import           Data.Text.Encoding             ( decodeUtf8 )

-- | Takes the DB and the expected hashes and logErrorN's the differences, if any.
logHashDifferences
    :: MonadLogger m
    => DbHashes
    -- ^ Database hashes
    -> DbHashes
 -- ^ Expected hashes
    -> m ()
logHashDifferences dbHashes expectedChecksums =
    when (dbHashes /= expectedChecksums) $ do
    -- Urgh.. UTF-8 Text as output from Aeson would be perfect here..
        logErrorN $ decodeUtf8 $ toStrict $ encode $ hashDifferences
            dbHashes
            expectedChecksums
        logErrorN
            "DB and expected checksums do not match. Differences right above this message. Left is Database, Right is expected."
        logErrorN
            "Do note some names will not be strictly the names of Database objects, because codd may add extra characters and strings due to DB name overloading."

hashDifferences :: DbHashes -> DbHashes -> Map FilePath DiffType
hashDifferences l r =
    let matches = matchOrd fst (toFiles l) (toFiles r)
    in
        Map.fromList $ mapMaybe
            (\case
                (Nothing       , Nothing       ) -> Nothing
                (Just (name, _), Nothing       ) -> Just (name, OnlyLeft)
                (Nothing       , Just (name, _)) -> Just (name, OnlyRight)
                (Just (name, h1), Just (_, h2))
                    | h1 == h2  -> Nothing
                    | otherwise -> Just (name, BothButDifferent)
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

