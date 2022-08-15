module Codd.Hashing.Database.Pg12
    ( hashQueryFor
    ) where

import           Codd.Hashing.Database.Model    ( HashQuery(..) )
import qualified Codd.Hashing.Database.Pg11    as Pg11
import           Codd.Hashing.Types             ( HashableObject(..)
                                                , ObjName
                                                )
import           Codd.Types                     ( ChecksumAlgo
                                                , SchemaSelection
                                                , SqlRole
                                                )

hashQueryFor
    :: [SqlRole]
    -> SchemaSelection
    -> ChecksumAlgo
    -> Maybe ObjName
    -> Maybe ObjName
    -> HashableObject
    -> HashQuery
hashQueryFor allRoles schemaSel checksumAlgo schemaName tableName hobj =
    let hq = Pg11.hashQueryFor allRoles
                               schemaSel
                               checksumAlgo
                               schemaName
                               tableName
                               hobj
    in  case hobj of
            HColumn ->
                hq { checksumCols = checksumCols hq ++ ["attgenerated"] }
            HCollation ->
                hq { checksumCols = checksumCols hq ++ ["collisdeterministic"] }
            _ -> hq
