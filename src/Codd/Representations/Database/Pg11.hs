module Codd.Representations.Database.Pg11
    ( hashQueryFor
    ) where

import           Codd.Representations.Database.Model
                                                ( HashQuery(..) )
import qualified Codd.Representations.Database.Pg10
                                               as Pg10
import           Codd.Representations.Types     ( ObjName
                                                , ObjectRep(..)
                                                )
import           Codd.Types                     ( ChecksumAlgo(..)
                                                , SchemaSelection
                                                , SqlRole
                                                )

hashQueryFor
    :: [SqlRole]
    -> SchemaSelection
    -> ChecksumAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> ObjectRep
    -> HashQuery
hashQueryFor allRoles schemaSel checksumAlgo schemaName tableName hobj =
    let hq = Pg10.hashQueryFor allRoles
                               schemaSel
                               checksumAlgo
                               schemaName
                               tableName
                               hobj
    in
        case hobj of
            HTableConstraint -> hq
                { checksumCols = checksumCols hq
                                     ++ [ ( "parent_constraint"
                                          , "pg_parent_constraint.conname"
                                          )
                                        ] -- TODO: Full constraint name
                , joins        =
                    joins hq
                        <> "\n LEFT JOIN pg_constraint pg_parent_constraint ON pg_parent_constraint.oid=pg_constraint.conparentid"
                }
            HRoutine -> hq
                { checksumCols = checksumCols hq ++ [("kind", "prokind")]
                , groupByCols  = groupByCols hq ++ ["prokind"]
                }
            HColumn ->
                -- Careful! Do not include atthasmissing or attmissingval here.
                -- They seem to hold values that change depending on whether
                -- a DEFAULT was added in ALTER TABLE or whether it was added from the start
                -- with CREATE TABLE.
                -- Read their descriptions at https://www.postgresql.org/docs/current/catalog-pg-attribute.html
                hq
            _ -> hq
