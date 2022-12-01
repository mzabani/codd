module Codd.Representations.Database.Pg11
    ( objRepQueryFor
    ) where

import           Codd.Representations.Database.Model
                                                ( DbObjRepresentationQuery(..) )
import qualified Codd.Representations.Database.Pg10
                                               as Pg10
import           Codd.Representations.Types     ( ObjName
                                                , ObjectRep(..)
                                                )
import           Codd.Types                     ( SchemaAlgo(..)
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
    let hq = Pg10.objRepQueryFor allRoles
                                 schemaSel
                                 schemaAlgoOpts
                                 schemaName
                                 tableName
                                 hobj
    in
        case hobj of
            HTableConstraint -> hq
                { repCols = repCols hq
                                ++ [ ( "parent_constraint"
                                     , "pg_parent_constraint.conname"
                                     )
                                   ] -- TODO: Full constraint name
                , joins   =
                    joins hq
                        <> "\n LEFT JOIN pg_constraint pg_parent_constraint ON pg_parent_constraint.oid=pg_constraint.conparentid"
                }
            HRoutine -> hq { repCols     = repCols hq ++ [("kind", "prokind")]
                           , groupByCols = groupByCols hq ++ ["prokind"]
                           }
            HColumn ->
                -- Careful! Do not include atthasmissing or attmissingval here.
                -- They seem to hold values that change depending on whether
                -- a DEFAULT was added in ALTER TABLE or whether it was added from the start
                -- with CREATE TABLE.
                -- Read their descriptions at https://www.postgresql.org/docs/current/catalog-pg-attribute.html
                hq
            _ -> hq
