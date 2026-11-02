module Codd.Representations.Database.Pg19
  ( objRepQueryFor,
  )
where

import Codd.Representations.Database.Model
  ( DbObjRepresentationQuery (..),
  )
import qualified Codd.Representations.Database.Pg18 as Pg18
import Codd.Representations.Types
  ( ObjName,
    ObjectRep (..),
  )
import Codd.Types
  ( SchemaAlgo (..),
    SchemaSelection,
    SqlRole,
  )

objRepQueryFor ::
  Int ->
  [SqlRole] ->
  SchemaSelection ->
  SchemaAlgo ->
  Maybe ObjName ->
  Maybe ObjName ->
  ObjectRep ->
  DbObjRepresentationQuery
objRepQueryFor = Pg18.objRepQueryFor

