module Codd.Hashing.Database.Pg11
    ( hashQueryFor
    ) where

import           Codd.Hashing.Database.Model    ( HashQuery(..) )
import qualified Codd.Hashing.Database.Pg10    as Pg10
import           Codd.Hashing.Types             ( HashableObject(..)
                                                , ObjName
                                                )
import           Codd.Types                     ( ChecksumAlgo(..)
                                                , SqlRole
                                                , SqlSchema
                                                )

hashQueryFor
    :: [SqlRole]
    -> [SqlSchema]
    -> ChecksumAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> HashableObject
    -> HashQuery
hashQueryFor allRoles allSchemas checksumAlgo schemaName tableName hobj =
    let hq = Pg10.hashQueryFor allRoles
                               allSchemas
                               checksumAlgo
                               schemaName
                               tableName
                               hobj
    in
        case hobj of
            HTableConstraint -> hq
                { checksumCols = checksumCols hq
                                     ++ ["pg_parent_constraint.conname"] -- TODO: Full constraint name
                , joins        =
                    joins hq
                        <> "\n LEFT JOIN pg_constraint pg_parent_constraint ON pg_parent_constraint.oid=pg_constraint.conparentid"
                }
            HRoutine -> hq { checksumCols = checksumCols hq ++ ["prokind"]
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
