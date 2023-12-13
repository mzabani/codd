module Codd.Representations.Database.Pg16
    ( objRepQueryFor
    ) where

import           Codd.Representations.Database.Model
                                                ( DbObjRepresentationQuery(..) )
import qualified Codd.Representations.Database.Pg15
                                               as Pg15
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
objRepQueryFor allRoles allSchemas schemaAlgoOpts schemaName tableName hobj =
    let hq = Pg15.objRepQueryFor allRoles
                                 allSchemas
                                 schemaAlgoOpts
                                 schemaName
                                 tableName
                                 hobj
    in  case hobj of
            HCollation ->
                hq { repCols = repCols hq ++ [("icurules", "collicurules")] }
            _ -> hq

