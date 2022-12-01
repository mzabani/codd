module Codd.Representations.Database.Pg13
    ( hashQueryFor
    ) where

import           Codd.Representations.Database.Model
                                                ( HashQuery(..) )
import qualified Codd.Representations.Database.Pg12
                                               as Pg12
import           Codd.Representations.Types     ( ObjName
                                                , ObjectRep(..)
                                                )
import           Codd.Types                     ( ChecksumAlgo
                                                , SchemaSelection
                                                , SqlRole
                                                )

-- Postgres 13 and 14 don't seem to have any hashable new features compared to 12.
hashQueryFor
    :: [SqlRole]
    -> SchemaSelection
    -> ChecksumAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> ObjectRep
    -> HashQuery
hashQueryFor = Pg12.hashQueryFor
