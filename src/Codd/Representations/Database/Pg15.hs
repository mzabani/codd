module Codd.Representations.Database.Pg15
  ( objRepQueryFor
  ) where

import           Codd.Representations.Database.Model
                                                ( DbObjRepresentationQuery(..) )
import qualified Codd.Representations.Database.Pg14
                                               as Pg14
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
  let hq = Pg14.objRepQueryFor allRoles
                               allSchemas
                               schemaAlgoOpts
                               schemaName
                               tableName
                               hobj
  in  case hobj of
        HCollation ->
          hq { repCols = repCols hq ++ [("iculocale", "colliculocale")] }
        HIndex ->
          hq { repCols = repCols hq ++ [("nullsnotdistinct", "indnullsnotdistinct")] }
        _ -> hq

