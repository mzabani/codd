module Codd.Representations.Database.Pg12
    ( hashQueryFor
    ) where

import           Codd.Representations.Database.Model
                                                ( HashQuery(..) )
import qualified Codd.Representations.Database.Pg11
                                               as Pg11
import           Codd.Representations.Types     ( ObjName
                                                , ObjectRep(..)
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
    -> ObjectRep
    -> HashQuery
hashQueryFor allRoles schemaSel checksumAlgo schemaName tableName hobj =
    let hq = Pg11.hashQueryFor allRoles
                               schemaSel
                               checksumAlgo
                               schemaName
                               tableName
                               hobj
    in  case hobj of
            HColumn -> hq
                { checksumCols = checksumCols hq
                                     ++ [("generated", "attgenerated")]
                }
            HCollation -> hq
                { checksumCols = checksumCols hq
                                     ++ [ ( "deterministic"
                                          , "collisdeterministic"
                                          )
                                        ]
                }
            _ -> hq
