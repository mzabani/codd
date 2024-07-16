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

objRepQueryFor ::
  [SqlRole] ->
  SchemaSelection ->
  SchemaAlgo ->
  Maybe ObjName ->
  Maybe ObjName ->
  ObjectRep ->
  DbObjRepresentationQuery
objRepQueryFor =
  -- Postgres 13 doesn't seem to have any verifiable new features compared to 12.
  Pg12.objRepQueryFor
