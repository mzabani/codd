module Codd.Representations.Database.Pg13
  ( objRepQueryFor,
  )
where

import Codd.Representations.Database.Model
  ( DbObjRepresentationQuery (..),
  )
import qualified Codd.Representations.Database.Pg12 as Pg12
import Codd.Representations.Types
  ( ObjName,
    ObjectRep (..),
  )
import Codd.Types
  ( SchemaAlgo,
    SchemaSelection,
    SqlRole,
  )

-- Postgres 13 doesn't seem to have any verifiable new features compared to 12.
objRepQueryFor ::
  [SqlRole] ->
  SchemaSelection ->
  SchemaAlgo ->
  Maybe ObjName ->
  Maybe ObjName ->
  ObjectRep ->
  DbObjRepresentationQuery
objRepQueryFor allRoles allSchemas schemaAlgoOpts schemaName tableName hobj =
  let hq =
        Pg12.objRepQueryFor
          allRoles
          allSchemas
          schemaAlgoOpts
          schemaName
          tableName
          hobj
   in case hobj of
        -- HStatistics ->
        --   hq
        --     { repCols = repCols hq ++ [("exprs", "pg_get_expr(stxexprs, stxrelid)")]
        --     -- TODO: more columns?
        --     }
        _ -> hq
