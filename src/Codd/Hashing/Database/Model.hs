{-# LANGUAGE AllowAmbiguousTypes #-}
module Codd.Hashing.Database.Model where

import Codd.Hashing.Types
import Codd.Types (Include(..), SqlSchema, SqlRole)
import Data.String (IsString(..))
import Database.PostgreSQL.Simple (ToRow, Query)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB

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
    RegularColumn tbl col -> tableName tbl <> "." <> col
    OidColumn tbl col -> tableName tbl <> "." <> col
    OidArrayColumn tbl col -> tableName tbl <> "." <> col
    PureSqlExpression q -> q

-- | Stores the name of the column with an OID and a table to join to based on equality for that column.
data JoinTable a = JoinTable (CatalogTableColumn a) (CatTable a) | LeftJoinTable (CatalogTableColumn a) (CatTable a) (CatalogTableColumn a) | JoinTableFull (CatTable a) [(CatalogTableColumn a, CatalogTableColumn a)]
mapJoinTableTbl :: (CatTable a -> CatTable b) -> JoinTable a -> JoinTable b
mapJoinTableTbl f (JoinTable col t) = JoinTable (mapCatTableCol f col) (f t)
mapJoinTableTbl f (LeftJoinTable col1 t col2) = LeftJoinTable (mapCatTableCol f col1) (f t) (mapCatTableCol f col2)
mapJoinTableTbl f (JoinTableFull t cols) = JoinTableFull (f t) $ map (\(col1, col2) -> (mapCatTableCol f col1, mapCatTableCol f col2)) cols

data ColumnComparison a = forall b. DB.ToField b => ColumnEq (CatalogTableColumn a) b | forall b. DB.ToField b => ColumnIn (CatalogTableColumn a) (Include b)
mapColumnComparisonTbl :: (CatTable a -> CatTable b) -> ColumnComparison a -> ColumnComparison b
mapColumnComparisonTbl f = \case
  ColumnEq col v -> ColumnEq (mapCatTableCol f col) v
  ColumnIn col ivs -> ColumnIn (mapCatTableCol f col) ivs

newtype WhereFilter = WhereFilter { unWhereFilter :: QueryFrag }
instance IsString WhereFilter where
  fromString = WhereFilter . fromString

class DbVersionHash a where
  -- | We force injectivity of the associated type to make our lives easier. This does mean we need a separate type for each version of the same DB, sadly.
  type CatTable a = b | b -> a
  tableName :: CatTable a -> QueryFrag
  -- | Given an object we take hashes of, returns the catalog table and an appropriate `WHERE` filter to apply on it, if any.
  hashableObjCatalogTable :: HashableObject -> (CatTable a, Maybe WhereFilter)
  fqObjNameCol :: CatTable a -> CatalogTableColumn a
  -- | Returns a list with all columns that uniquely Identify an Object in a catalog table. Usually this is just the object's name, but in some cases
  --   can be more than one column.
  -- Important note: we don't include columns here that are reflected in a hierarchical on-disk ancestor. One example is for "pg_class", which is uniquely
  -- identified by "relname, relnamespace": we only include "relname", because in our context we'll only be searching in "pg_class" filtering by a specific
  -- namespace already.
  fqTableIdentifyingCols :: CatTable a -> [CatalogTableColumn a]
  hashingColsOf :: CatTable a -> [CatalogTableColumn a]
  joinsFor :: HashableObject -> [JoinTable a]
  filtersForSchemas :: Include SqlSchema -> [ColumnComparison a]
  filtersForRoles :: Include SqlRole -> [ColumnComparison a]
  underSchemaFilter :: HashableObject -> ObjName -> [ColumnComparison a]
  -- | Receives schema and table name in this order.
  underTableFilter :: HashableObject -> ObjName -> ObjName -> [ColumnComparison a]


-- | Just a helper that stores a straight column name or a col name which is an OID or an OID[], and another pg_catalog table to join on to use that object's name in the hashing instead.
data CatalogTableColumn a = RegularColumn (CatTable a) QueryFrag | OidColumn (CatTable a) QueryFrag | OidArrayColumn (CatTable a) QueryFrag | PureSqlExpression QueryFrag
mapCatTableCol :: (CatTable a -> CatTable b) -> CatalogTableColumn a -> CatalogTableColumn b
mapCatTableCol f = \case
  RegularColumn t c -> RegularColumn (f t) c
  OidColumn t q -> OidColumn (f t) q
  OidArrayColumn t q -> OidArrayColumn (f t) q
  PureSqlExpression q -> PureSqlExpression q
-- deriving instance Show a => Show (CatalogTableColumn a)
instance IsString (CatalogTableColumn a) where
  fromString s = PureSqlExpression $ QueryFrag (fromString s) ()