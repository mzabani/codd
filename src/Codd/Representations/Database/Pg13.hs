module Codd.Representations.Database.Pg13
    ( objRepQueryFor
    ) where

import           Codd.Representations.Database.Model
                                                ( DbObjRepresentationQuery(..) )
import qualified Codd.Representations.Database.Pg12
                                               as Pg12
import           Codd.Representations.Types     ( ObjName
                                                , ObjectRep(..)
                                                )
import           Codd.Types                     ( SchemaAlgo
                                                , SchemaSelection
                                                , SqlRole
                                                )

-- Postgres 13 and 14 don't seem to have any verifiable new features compared to 12.
objRepQueryFor
    :: [SqlRole]
    -> SchemaSelection
    -> SchemaAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> ObjectRep
    -> DbObjRepresentationQuery
objRepQueryFor = Pg12.objRepQueryFor
