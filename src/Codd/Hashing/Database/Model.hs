module Codd.Hashing.Database.Model where

import Codd.Hashing.Types
import Data.String (IsString(..))
import Database.PostgreSQL.Simple (ToRow, Query)
import qualified Database.PostgreSQL.Simple as DB

data QueryFrag = forall a. ToRow a => QueryFrag Query a
instance IsString QueryFrag where
    fromString s = QueryFrag (fromString s) ()
instance Semigroup QueryFrag where
    QueryFrag q1 p1 <> QueryFrag q2 p2 = QueryFrag (q1 <> q2) (p1 DB.:. p2)
instance Monoid QueryFrag where
  mempty = ""
instance Show QueryFrag where
  show (QueryFrag q _) = show q

withQueryFrag :: QueryFrag -> (forall a. ToRow a => Query -> a -> b) -> b
withQueryFrag (QueryFrag q args) f = f q args

class ToQueryFrag a where
  toQueryFrag :: a -> QueryFrag

infixr 7 <>>
(<>>) :: ToQueryFrag a => QueryFrag -> a -> QueryFrag
qf1 <>> qf2 = qf1 <> toQueryFrag qf2

infixr 7 <<>
(<<>) :: ToQueryFrag a => a -> QueryFrag -> QueryFrag
qf1 <<> qf2 = toQueryFrag qf1 <> qf2

instance ToQueryFrag QueryFrag where
  toQueryFrag = id

instance DbVersionHash a => ToQueryFrag (CatalogTableColumn a) where
  toQueryFrag = \case
    UnqualifiedColumn col -> col
    FullyQualifiedColumn col -> col
    JoinOid tbl col -> tableName tbl <> "." <> col
    JoinOidArray tbl col -> tableName tbl <> "." <> col

-- | Stores the table's column name, a table to join to and the value to compare the joined table's objname to.
data JoinFilter a = JoinFilter QueryFrag (CatTable a) ObjName
mapJoinFilterTbl :: (CatTable a -> CatTable b) -> JoinFilter a -> JoinFilter b
mapJoinFilterTbl f (JoinFilter q t n) = JoinFilter q (f t) n

class DbVersionHash a where
  -- | We force injectivity of the associated type to make our lives easier. This does mean we need a separate type for each version of the same DB, sadly.
  type CatTable a = b | b -> a
  tableName :: CatTable a -> QueryFrag
  -- | Given an object we take hashes of, returns the catalog table and an appropriate `WHERE` filter to apply on it, if any.
  hashableObjCatalogTable :: HashableObject -> (CatTable a, Maybe QueryFrag)
  fqObjNameCol :: CatTable a -> CatalogTableColumn a
  -- | Returns a list with all columns that uniquely Identify an Object in a catalog table. Usually this is just the object's name, but in some cases
  --   can be more than one column.
  -- Important note: we don't include columns here that are reflected in a hierarchical on-disk ancestor. One example is for "pg_class", which is uniquely
  -- identified by "relname, relnamespace": we only include "relname", because in our context we'll only be searching in "pg_class" filtering by a specific
  -- namespace already.
  fqTableIdentifyingCols :: CatTable a -> [CatalogTableColumn a]
  hashingColsOf :: CatTable a -> [CatalogTableColumn a]
  underSchemaFilter :: HashableObject -> ObjName -> [JoinFilter a]
  underTableFilter :: HashableObject -> ObjName -> [JoinFilter a]


-- | Just a helper that stores a straight column name or a col name which is an OID or an OID[], and another pg_catalog table to join on to use that object's name in the hashing instead.
data CatalogTableColumn a = UnqualifiedColumn QueryFrag | FullyQualifiedColumn QueryFrag | JoinOid (CatTable a) QueryFrag | JoinOidArray (CatTable a) QueryFrag
mapCatTableCol :: (CatTable a -> CatTable b) -> CatalogTableColumn a -> CatalogTableColumn b
mapCatTableCol f = \case
  UnqualifiedColumn q -> UnqualifiedColumn q
  FullyQualifiedColumn q -> FullyQualifiedColumn q
  JoinOid t q -> JoinOid (f t) q
  JoinOidArray t q -> JoinOidArray (f t) q
-- deriving instance Show a => Show (CatalogTableColumn a)
instance IsString (CatalogTableColumn a) where
  fromString = UnqualifiedColumn . fromString

-- | Fully qualified column name (returns table.colname)
fqcn :: DbVersionHash a => CatTable a -> QueryFrag -> CatalogTableColumn a
fqcn tbl col = FullyQualifiedColumn $ tableName tbl <> "." <> col