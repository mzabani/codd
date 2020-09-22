module Codd.Hashing.Database where

import Debug.Trace (traceShowId)
import Codd.Hashing.Types
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.String (IsString(..))
import Database.PostgreSQL.Simple (ToRow, Query)
import qualified Database.PostgreSQL.Simple as DB
import GHC.Stack (HasCallStack)
import UnliftIO (MonadUnliftIO, MonadIO(..))

data CatalogTable = PgNamespace | PgClass | PgProc | PgAuthId | PgLanguage | PgType | PgConstraint | PgOperator | PgAttribute | PgTrigger deriving stock Show

-- | Given an object we take hashes of, returns the catalog table and an appropriate `WHERE` filter to apply on it, if any.
hashableObjCatalogTable :: HashableObject -> (CatalogTable, Maybe QueryFrag)
hashableObjCatalogTable = \case
  HSchema -> (PgNamespace, Nothing)
  HTable -> (PgClass, Just "pg_class.relkind IN ('r', 't', 'f', 'p')")
  HView -> (PgClass, Just "pg_class.relkind IN ('v', 'm')")
  HRoutine -> (PgProc, Nothing)
  HColumn -> (PgAttribute, Nothing)
  HTableConstraint -> (PgConstraint, Nothing)
  HTrigger -> (PgTrigger, Nothing)

-- | Just a helper that stores a straight column name or a col name which is an OID or an OID[], and another pg_catalog table to join on to use that object's name in the hashing instead.
data CatalogTableColumn = UnqualifiedColumn QueryFrag | FullyQualifiedColumn QueryFrag | JoinOid CatalogTable QueryFrag | JoinOidArray CatalogTable QueryFrag deriving stock Show
instance IsString CatalogTableColumn where
  fromString = UnqualifiedColumn . fromString

tableName :: CatalogTable -> QueryFrag
tableName = \case
  PgNamespace -> "pg_namespace"
  PgClass -> "pg_class"
  PgProc -> "pg_proc"
  PgConstraint -> "pg_constraint"
  PgAuthId -> "pg_authid"
  PgLanguage -> "pg_language"
  PgType -> "pg_type"
  PgOperator -> "pg_operator"
  PgAttribute -> "pg_attribute"
  PgTrigger -> "pg_trigger"

fqObjNameCol :: CatalogTable -> CatalogTableColumn
fqObjNameCol = \case
  PgNamespace -> fqcn PgNamespace "nspname"
  PgClass -> fqcn PgClass "relname"
  PgProc -> fqcn PgProc "proname"
  PgConstraint -> fqcn PgConstraint "conname"
  PgAuthId -> fqcn PgAuthId "rolname"
  PgLanguage -> fqcn PgLanguage "lanname"
  PgType -> fqcn PgType "typname"
  PgOperator -> fqcn PgOperator "oprname"
  PgAttribute -> fqcn PgAttribute "attname"
  PgTrigger -> fqcn PgTrigger "tgname"

-- | Returns a list with all columns that uniquely Identify an Object in a catalog table. Usually this is just the object's name, but in some cases
--   can be more than one column.
-- Important note: we don't include columns here that are reflected in a hierarchical on-disk ancestor. One example is for "pg_class", which is uniquely
-- identified by "relname, relnamespace": we only include "relname", because in our context we'll only be searching in "pg_class" filtering by a specific
-- namespace already.
fqTableIdentifyingCols :: CatalogTable -> [CatalogTableColumn]
fqTableIdentifyingCols tbl = fqObjNameCol tbl : case tbl of
  PgNamespace -> []
  PgClass -> []
  PgProc -> [JoinOidArray PgType "proargtypes"]
  PgConstraint -> [JoinOid PgType "contypid"]
  PgAuthId -> []
  PgLanguage -> []
  PgType -> []
  PgOperator -> [JoinOid PgType "oprleft", JoinOid PgType "oprright"]
  PgAttribute -> []
  PgTrigger -> []

hashingColsOf :: CatalogTable -> [CatalogTableColumn]
hashingColsOf = \case
  PgNamespace -> error "pgnamespace cols missing"
  PgClass -> error "pgclass cols missing"
  PgProc -> [ JoinOid PgAuthId "proowner", JoinOid PgLanguage "prolang", "procost", "prorows", JoinOid PgType "provariadic", "prokind", "prosecdef", "proleakproof", "proisstrict", "proretset", "provolatile", "proparallel", "pronargs", "pronargdefaults", JoinOid PgType "prorettype", JoinOidArray PgType "proargtypes" ,"proargmodes", "proargnames", "proargdefaults", JoinOidArray PgType "protrftypes", "prosrc", "probin", "proconfig", "proacl" ]
  PgConstraint -> [ "contype", "condeferrable", "condeferred", "convalidated", JoinOid PgClass "conrelid", JoinOid PgType "contypid", JoinOid PgClass "conindid", JoinOid PgConstraint "conparentid", JoinOid PgClass "confrelid", "confupdtype", "confdeltype", "confmatchtype", "conislocal", "coninhcount", "connoinherit", "conkey", "confkey", JoinOidArray PgOperator "conpfeqop", JoinOidArray PgOperator "conppeqop", JoinOidArray PgOperator "conffeqop", JoinOidArray PgOperator "conexclop", FullyQualifiedColumn "pg_get_constraintdef(pg_constraint.oid)" ]
  PgAuthId -> error "pgauthid cols missing"
  PgLanguage -> error "pglanguage cols missing"
  PgType -> error "pgtype cols missing"
  PgOperator -> error "pgoperator cols missing"
  PgAttribute -> error "pgattribute cols missing"
  PgTrigger -> [ JoinOid PgProc "tgfoid", "tgtype", "tgenabled", "tgisinternal", JoinOid PgClass "tgconstrrelid", JoinOid PgClass "tgconstrindid", JoinOid PgConstraint "tgconstraint", "tgdeferrable", "tginitdeferred", "tgnargs", "tgattr", "tgargs", "tgqual", "tgoldtable", "tgnewtable" ]

-- | Fully qualified column name (returns table.colname)
fqcn :: CatalogTable -> QueryFrag -> CatalogTableColumn
fqcn tbl col = FullyQualifiedColumn $ tableName tbl <> "." <> col

-- | We do not want columns of type OID or OID[] to affect hashing. They're instance-local values that might differ regardless of the DB's schema.
hashProjection :: [QueryFrag] -> QueryFrag
hashProjection cols = "MD5(" <> interspBy " || " (map hashExpr cols) <> ")"

hashExpr :: QueryFrag -> QueryFrag
hashExpr expr = "(CASE WHEN " <> expr <> " IS NULL THEN '' ELSE '_' || (" <> expr <> ") :: TEXT END)"

-- | Returns a SQL expression of type TEXT with the concatenation of all the non-OID columns that form the Identity of a row in the catalog table whose OID equals the supplied one.
concatenatedIdentityColsOf :: HasCallStack => CatalogTable -> QueryFrag -> QueryFrag
concatenatedIdentityColsOf tbl oid = "(SELECT " <> otherTblIdentifyingCols <> " FROM " <> tableName tbl <> " WHERE " <> oid <> "=" <> fqcn tbl "oid" <<> ")"
  where
    otherTblIdentifyingCols = interspBy " || " $ map colOf $ fqTableIdentifyingCols tbl
    colOf = \case
      FullyQualifiedColumn c -> c
      UnqualifiedColumn c -> tableName tbl <> "." <> c
      JoinOid joinTbl thisTblOidCol -> "(SELECT " <> concatenatedIdentityColsOf joinTbl thisTblOidCol <> " FROM " <> tableName joinTbl <> " WHERE " <> fqcn joinTbl "oid" <<> "=" <> fqcn tbl thisTblOidCol <<> ")"
      JoinOidArray joinTbl thisTblOidCol -> 
             "(SELECT ARRAY_TO_STRING(ARRAY_AGG(" <> concatenatedIdentityColsOf joinTbl "s.elem" <> " ORDER BY s.idx), ';') "
          <> " FROM pg_catalog." <> tableName joinTbl <> " JOIN UNNEST(" <> thisTblOidCol <> ") WITH ORDINALITY s(elem, idx) ON " <> fqcn joinTbl "oid" <<> "=s.elem)"

hashProjection2 :: HasCallStack => CatalogTable -> QueryFrag
hashProjection2 objTable = "MD5(" <> interspBy " || " (map toHash cols) <> ")"
  where
    cols = hashingColsOf objTable
    toHash (FullyQualifiedColumn col) = hashExpr col
    toHash (UnqualifiedColumn unqCol) = toHash $ fqcn objTable unqCol
    toHash (JoinOid otherTbl tblCol) = hashExpr $ concatenatedIdentityColsOf otherTbl tblCol
    toHash (JoinOidArray otherTbl tblCol) = hashExpr $
      -- This one is trickier: we want to ensure order changes results
      let
        otherTblIdentifyingCols = concatenatedIdentityColsOf otherTbl "s.elem"
        objTblOidCol = fqcn objTable tblCol
      in
      "(SELECT ARRAY_TO_STRING(ARRAY_AGG(" <> otherTblIdentifyingCols <<> " ORDER BY s.idx), ';') "
      <> " FROM pg_catalog." <> tableName otherTbl <> " JOIN UNNEST(" <> objTblOidCol <<> ") WITH ORDINALITY s(elem, idx) ON " <> fqcn otherTbl "oid" <<> "=s.elem)"

interspBy :: QueryFrag -> [QueryFrag] -> QueryFrag
interspBy _ [] = ""
interspBy _ (c:[]) = c
interspBy sep (c:cs) = c <> sep <> interspBy sep cs

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

instance ToQueryFrag CatalogTableColumn where
  toQueryFrag = \case
    UnqualifiedColumn col -> col
    FullyQualifiedColumn col -> col
    JoinOid tbl col -> tableName tbl <> "." <> col
    JoinOidArray tbl col -> tableName tbl <> "." <> col

-- | Stores the table's column name, a table to join to and the value to compare the joined table's objname to.
data JoinFilter = JoinFilter QueryFrag CatalogTable ObjName
queryObjNamesAndHashes3 :: (HasCallStack, MonadIO m) => DB.Connection -> HashableObject -> [JoinFilter] -> m [(ObjName, ObjHash)]
queryObjNamesAndHashes3 conn hobj joinFilters = liftIO $ withQueryFrag fullQuery (DB.query conn)
  where
    (objTbl, mfilters) = hashableObjCatalogTable hobj
    objNameCol = fqObjNameCol objTbl
    fullQuery = "SELECT " <> objNameCol <<> ", " <> hashProjection2 objTbl <> " FROM " <> tableName objTbl
                     <> joins
                     <> maybe "" ("\n WHERE " <>>) mfilters
                     <> "\n ORDER BY " <>> objNameCol
    -- TODO: Not all catalog tables have "oid" as their identifier column
    joins = foldMap (\(JoinFilter col joinTbl joinedTblObjName) -> "\n JOIN " <> tableName joinTbl <> " ON " <> fqcn objTbl col <<> "=" <> fqcn joinTbl "oid" <<> " AND " <> fqObjNameCol joinTbl <<> QueryFrag "=?" (DB.Only joinedTblObjName)) joinFilters

queryObjNamesAndHashes :: (HasCallStack, MonadIO m) => DB.Connection -> QueryFrag -> [QueryFrag] -> QueryFrag -> Maybe QueryFrag -> m [(ObjName, ObjHash)]
queryObjNamesAndHashes conn objNameCol hashCols table filterBy = liftIO $ withQueryFrag fullQuery (DB.query conn)
  where
    fullQuery = "SELECT " <> objNameCol <> ", " <> hashProjection hashCols <> " FROM " <> table
                     <> maybe "" (" WHERE " <>) filterBy
                     <> " ORDER BY " <> objNameCol

readHashesFromDatabase :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> m DbHashes
readHashesFromDatabase conn = do
    -- TODO: Do different installations of postgres come with different schemas? We should ask the User which schemas to consider
    schemas :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "schema_name" ["schema_owner", "default_character_set_catalog", "default_character_set_schema", "default_character_set_name" ] "information_schema.schemata" (Just $ QueryFrag "schema_name NOT IN ?" (DB.Only $ DB.In [ "information_schema" :: Text, "pg_catalog", "pg_temp_1", "pg_toast", "pg_toast_temp_1" ]))
    DbHashes <$> getSchemaHash conn schemas

getSchemaHash :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> [(ObjName, ObjHash)] -> m (Map ObjName SchemaHash)
getSchemaHash conn schemas = fmap Map.fromList $ forM schemas $ \(schemaName, schemaHash) -> do
    -- TODO: do it all in a single query? it'd be nice to batch.. we could batch n+1 queries with haxl!!
    tables :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["table_type", "self_referencing_column_name", "reference_generation", "is_insertable_into", "is_typed", "commit_action"] "information_schema.tables" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    views :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["view_definition", "check_option", "is_updatable", "is_insertable_into", "is_trigger_updatable", "is_trigger_deletable", "is_trigger_insertable_into"] "information_schema.views" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    -- routines :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "specific_name" ["routine_name", "routine_type", "data_type", "routine_body", "routine_definition", "external_name", "external_language", "is_deterministic", "is_null_call", "security_type"] "information_schema.routines" (Just $ QueryFrag "specific_schema=?" (DB.Only schemaName))
    routines :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes3 conn HRoutine [JoinFilter "pronamespace" PgNamespace schemaName]
    -- routines <- pure []
    sequences :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "sequence_name" ["data_type", "numeric_precision", "numeric_precision_radix", "numeric_scale", "start_value", "minimum_value", "maximum_value", "increment", "cycle_option"] "information_schema.sequences" (Just $ QueryFrag "sequence_schema=?" (DB.Only schemaName))
    tableHashes <- getTablesHashes conn schemaName tables
    let allObjs = listToMap $ tableHashes ++ map (uncurry ViewHash) views ++ map (uncurry RoutineHash) routines ++ map (uncurry SequenceHash) sequences
    return (schemaName, SchemaHash schemaName schemaHash allObjs)

getTablesHashes :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> ObjName -> [(ObjName, ObjHash)] -> m [SchemaObjectHash]
getTablesHashes conn schemaName tables = forM tables $ \(tblName, tableHash) -> do
    columns :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "column_name" ["column_default", "is_nullable", "data_type", "character_maximum_length", "character_octet_length", "numeric_precision", "numeric_precision_radix", "numeric_scale", "datetime_precision", "interval_type", "interval_precision", "collation_name", "is_identity", "identity_generation", "identity_start", "identity_increment", "identity_maximum", "identity_minimum", "identity_cycle", "is_generated", "generation_expression", "is_updatable"] "information_schema.columns" (Just $ QueryFrag "table_schema=? AND table_name=?" (schemaName, tblName))
    -- constraints :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "constraint_name" ["constraint_type", "is_deferrable", "initially_deferred"] "information_schema.table_constraints" (Just $ QueryFrag "constraint_schema=? AND table_name=?" (schemaName, tableName))
    constraints :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes3 conn HTableConstraint [JoinFilter "connamespace" PgNamespace schemaName, JoinFilter "conrelid" PgClass tblName ]
    -- ^ PG 10 Does not have the "conparentid" column..
    triggers :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes3 conn HTrigger [JoinFilter "tgrelid" PgClass tblName ]
    -- TODO: VERY IMPORTANT!! THERE CAN BE TABLES WITH THE SAME NAME IN SEPARATE SCHEMAS, AND FETCHING TRIGGERS HERE WILL FAIL ON THAT SCENARIO!!
    -- WE NEED THE CURRENT TABLE'S OID TO FIX THIS
    pure $ TableHash tblName tableHash (listToMap $ map (uncurry TableColumn) columns)
                                       (listToMap $ map (uncurry TableConstraint) constraints)
                                       (listToMap $ map (uncurry TableTrigger) triggers)