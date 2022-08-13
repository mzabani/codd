module Codd.Hashing.Database.Pg13
    ( hashQueryFor
    ) where

import           Codd.Hashing.Database.Model    ( HashQuery(..) )
import qualified Codd.Hashing.Database.Pg12    as Pg12
import           Codd.Hashing.Types             ( HashableObject(..)
                                                , ObjName
                                                )
import           Codd.Types                     ( ChecksumAlgo
                                                , SqlRole
                                                , SqlSchema
                                                )

-- Postgres 13 and 14 don't seem to have any hashable new features compared to 12.
hashQueryFor
    :: [SqlRole]
    -> [SqlSchema]
    -> ChecksumAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> HashableObject
    -> HashQuery
hashQueryFor = Pg12.hashQueryFor
