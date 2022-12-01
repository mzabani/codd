module Codd.Representations.Database.Pg12
    ( objRepQueryFor
    ) where

import           Codd.Representations.Database.Model
                                                ( DbObjRepresentationQuery(..) )
import qualified Codd.Representations.Database.Pg11
                                               as Pg11
import           Codd.Representations.Types     ( ObjName
                                                , ObjectRep(..)
                                                )
import           Codd.Types                     ( SchemaAlgo
                                                , SchemaSelection
                                                , SqlRole
                                                )

objRepQueryFor
    :: [SqlRole]
    -> SchemaSelection
    -> SchemaAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> ObjectRep
    -> DbObjRepresentationQuery
objRepQueryFor allRoles schemaSel schemaAlgoOpts schemaName tableName hobj =
    let hq = Pg11.objRepQueryFor allRoles
                                 schemaSel
                                 schemaAlgoOpts
                                 schemaName
                                 tableName
                                 hobj
    in  case hobj of
            HColumn ->
                hq { repCols = repCols hq ++ [("generated", "attgenerated")] }
            HCollation -> hq
                { repCols = repCols hq
                                ++ [("deterministic", "collisdeterministic")]
                }
            _ -> hq
