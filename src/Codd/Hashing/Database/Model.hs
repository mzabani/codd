{-# LANGUAGE AllowAmbiguousTypes #-}
module Codd.Hashing.Database.Model where

import           Codd.Hashing.Types
import           Codd.Types                     ( Include(..)
                                                , SqlRole
                                                , SqlSchema
                                                )
import           Data.String                    ( IsString(..) )
import           Database.PostgreSQL.Simple     ( Query
                                                , ToRow
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.ToField
                                               as DB

data CatalogTable = PgDatabase | PgNamespace | PgClass | PgProc | PgAuthId | PgIndex | PgLanguage | PgType | PgConstraint | PgOperator | PgAttribute | PgTrigger | PgAccessMethod | PgCollation | PgPolicy | PgSequence | PgRoleSettings | PgViews deriving stock Show
pgTableName :: CatalogTable -> QueryFrag
pgTableName = \case
  PgDatabase     -> "pg_database"
  PgNamespace    -> "pg_namespace"
  PgClass        -> "pg_class"
  PgProc         -> "pg_proc"
  PgConstraint   -> "pg_constraint"
  PgAuthId       -> "pg_authid"
  PgIndex        -> "pg_index"
  PgLanguage     -> "pg_language"
  PgType         -> "pg_type"
  PgOperator     -> "pg_operator"
  PgAttribute    -> "pg_attribute"
  PgTrigger      -> "pg_trigger"
  PgAccessMethod -> "pg_am"
  PgCollation    -> "pg_collation"
  PgPolicy       -> "pg_policy"
  PgSequence     -> "pg_sequence"
  PgRoleSettings -> "pg_db_role_setting"
  PgViews        -> "pg_views"

-- | Returns "table AS alias".
tableNameAndAlias :: DbVersionHash a => CatTableAliased a -> QueryFrag
tableNameAndAlias atbl@(CatTableAliased _ alias) =
  tableName atbl <> " AS " <> alias

tableAlias :: CatTableAliased a -> QueryFrag
tableAlias (CatTableAliased _ alias) = alias

data QueryFrag = forall a . ToRow a => QueryFrag Query a
instance IsString QueryFrag where
  fromString s = QueryFrag (fromString s) ()
instance Semigroup QueryFrag where
  QueryFrag q1 p1 <> QueryFrag q2 p2 = QueryFrag (q1 <> q2) (p1 DB.:. p2)
instance Monoid QueryFrag where
  mempty = ""
instance Show QueryFrag where
  show (QueryFrag q _) = show q

withQueryFrag :: QueryFrag -> (forall a . ToRow a => Query -> a -> b) -> b
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
    RegularColumn  tbl col -> tableAlias tbl <> "." <> col
    AclItemsColumn tbl col -> tableAlias tbl <> "." <> col
    OidColumn      _   col -> toQueryFrag col
    OidArrayColumn tbl col -> tableAlias tbl <> "." <> col
    PureSqlExpression q    -> q

-- | Stores the name of the column with an OID and a table to join to based on equality for that column.
data JoinTable a = JoinTable (CatalogTableColumn a) (CatTableAliased a) | LeftJoinTable (CatalogTableColumn a) (CatTableAliased a) (CatalogTableColumn a) | JoinTableFull (CatTableAliased a) [(CatalogTableColumn a, CatalogTableColumn a)]
mapJoinTableTbl
  :: (CatTableAliased a -> CatTableAliased b) -> JoinTable a -> JoinTable b
mapJoinTableTbl f (JoinTable col t) = JoinTable (mapCatTableCol f col) (f t)
mapJoinTableTbl f (LeftJoinTable col1 t col2) =
  LeftJoinTable (mapCatTableCol f col1) (f t) (mapCatTableCol f col2)
mapJoinTableTbl f (JoinTableFull t cols) = JoinTableFull (f t)
  $ map (\(col1, col2) -> (mapCatTableCol f col1, mapCatTableCol f col2)) cols

data ColumnComparison a = forall b. DB.ToField b => ColumnEq (CatalogTableColumn a) b | forall b. DB.ToField b => ColumnIn (CatalogTableColumn a) (Include b)
mapColumnComparisonTbl
  :: (CatTableAliased a -> CatTableAliased b)
  -> ColumnComparison a
  -> ColumnComparison b
mapColumnComparisonTbl f = \case
  ColumnEq col v   -> ColumnEq (mapCatTableCol f col) v
  ColumnIn col ivs -> ColumnIn (mapCatTableCol f col) ivs

newtype WhereFilter = WhereFilter { unWhereFilter :: QueryFrag }
instance IsString WhereFilter where
  fromString = WhereFilter . fromString

class DbVersionHash a where
  -- | We force injectivity of the associated type to make our lives easier. This does mean we need a separate type for each version of the same DB, sadly.
  type CatTable a = b | b -> a
  tableName :: CatTableAliased a -> QueryFrag
  -- | Given an object we take hashes of, returns the catalog table and an appropriate `WHERE` filter to apply on it, if any.
  hashableObjCatalogTable :: HashableObject -> (CatTableAliased a, Maybe WhereFilter)
  fqObjNameCol :: CatTableAliased a -> CatalogTableColumn a
  -- | Returns a list with all columns that uniquely Identify an Object in a catalog table. Usually this is just the object's name, but in some cases
  --   can be more than one column.
  -- Important note: we don't include columns here that are reflected in a hierarchical on-disk ancestor. One example is for "pg_class", which is uniquely
  -- identified by "relname, relnamespace": we only include "relname", because in our context we'll only be searching in "pg_class" filtering by a specific
  -- namespace already.
  fqTableIdentifyingCols :: CatTableAliased a -> [CatalogTableColumn a]
  hashingColsOf :: CatTableAliased a -> [CatalogTableColumn a]
  joinsFor :: HashableObject -> [JoinTable a]
  filtersForSchemas :: Include SqlSchema -> [ColumnComparison a]
  filtersForRoles :: Include SqlRole -> [ColumnComparison a]
  underSchemaFilter :: HashableObject -> ObjName -> [ColumnComparison a]
  -- | Receives schema and table name in this order.
  underTableFilter :: HashableObject -> ObjName -> ObjName -> [ColumnComparison a]

data CatTableAliased a = CatTableAliased (CatTable a) QueryFrag

mapCatTableAliased
  :: (CatTable a -> CatTable b) -> CatTableAliased a -> CatTableAliased b
mapCatTableAliased f (CatTableAliased t qf) = CatTableAliased (f t) qf

-- | Just a helper that stores one of:
--   1. "regular" table + column names
--   2. table + CatalogTableColumn, joined by an "oid" column on "table", so that we hash on CatalogTableColumn, not the oid (which can vary across different DBs).
--   3. Same as above, but for an OID[], not an OID.
--   4. table + column name of a column that is an aclitem[], a set of permissions granted to the object, so that the role names are hashed without concerns with order in the array while
--      also ignoring unmapped roles.
data CatalogTableColumn a = RegularColumn (CatTableAliased a) QueryFrag | OidColumn (CatTableAliased a) (CatalogTableColumn a) | OidArrayColumn (CatTableAliased a) QueryFrag | AclItemsColumn (CatTableAliased a) QueryFrag | PureSqlExpression QueryFrag
mapCatTableCol
  :: (CatTableAliased a -> CatTableAliased b)
  -> CatalogTableColumn a
  -> CatalogTableColumn b
mapCatTableCol f = \case
  RegularColumn  t c   -> RegularColumn (f t) c
  OidColumn      t col -> OidColumn (f t) (mapCatTableCol f col)
  OidArrayColumn t q   -> OidArrayColumn (f t) q
  AclItemsColumn t q   -> AclItemsColumn (f t) q
  PureSqlExpression q  -> PureSqlExpression q
-- deriving instance Show a => Show (CatalogTableColumn a)
instance IsString (CatalogTableColumn a) where
  fromString s = PureSqlExpression $ QueryFrag (fromString s) ()
