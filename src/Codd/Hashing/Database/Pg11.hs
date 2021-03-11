module Codd.Hashing.Database.Pg11
    ( hashQueryFor
    ) where

import           Codd.Hashing.Database.Model    ( HashQuery(..) )
import qualified Codd.Hashing.Database.Pg10    as Pg10
import           Codd.Hashing.Types             ( HashableObject(..)
                                                , ObjName
                                                )
import           Codd.Types                     ( Include
                                                , SqlRole
                                                , SqlSchema
                                                )

hashQueryFor
    :: Include SqlRole
    -> Include SqlSchema
    -> Maybe ObjName
    -> Maybe ObjName
    -> HashableObject
    -> HashQuery
hashQueryFor allRoles allSchemas schemaName tableName hobj =
    let hq = Pg10.hashQueryFor allRoles allSchemas schemaName tableName hobj
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
            HColumn -> hq
                { checksumCols = checksumCols hq
                                     ++ ["atthasmissing", "attmissingval"]
                }
            _ -> hq
