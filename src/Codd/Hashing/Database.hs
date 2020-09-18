module Codd.Hashing.Database (readHashesFromDatabase) where

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

-- | Returns the Object Name column given a table in pg_catalog. This is partial function, but every possible input value
-- that matters should be tested every time we obtain Db Hashes anyway.
pgCatalogInfo :: String -> QueryFrag
pgCatalogInfo "pg_namespace" = "nspname"
pgCatalogInfo "pg_class" = "relname"
pgCatalogInfo "pg_proc" = "nspname"
pgCatalogInfo "pg_type" = "typname"
pgCatalogInfo "pg_language" = "lanname"
pgCatalogInfo "pg_authid" = "rolname"
pgCatalogInfo t = error $ "pg_catalog table info not available for " <> t

-- | Fully qualified column name (returns table.colname)
fqcn :: String -> QueryFrag -> QueryFrag
fqcn tbl col = QueryFrag (fromString tbl) () <> "." <> col

-- | We do not want columns of type OID or OID[] to affect hashing. They're instance-local values that might differ regardless of the DB's schema.
hashProjection :: [QueryFrag] -> QueryFrag
hashProjection cols = "MD5(" <> interspBy " || " (map toHash cols) <> ")"
  where
    toHash col = "(CASE WHEN " <> col <> " IS NULL THEN '' WHEN " <> isOidCol col <> " THEN '' ELSE '_' || " <> col <> " :: TEXT END)"
    isOidCol col = "pg_typeof(" <> col <> ")=pg_typeof(0::OID) OR pg_typeof(" <> col <> ")=pg_typeof('{0}'::OID[])"
    interspBy _ [] = ""
    interspBy _ (c:[]) = c
    interspBy sep (c:cs) = c <> sep <> interspBy sep cs

-- | We do not want columns of type OID or OID[] to affect hashing. They're instance-local values that might differ regardless of the DB's schema.
-- We want to force ourselves to join to other columns in those cases and use a proper object name instead of an OID. We force division by 0 in that case.
hashProjection2 :: String -> [CatalogTableColumn] -> QueryFrag
hashProjection2 objTable cols = "MD5(" <> interspBy " || " (map toHash cols) <> ")"
  where
    toHash (StraightColumn col) = "(CASE WHEN " <> col <> " IS NULL THEN '' WHEN " <> isOidCol col <> " THEN (1 / (txid_current() - txid_current())) :: text ELSE '_' || " <> col <> " :: TEXT END)"
    toHash (JoinOid tblCol otherTbl) = toHash $ StraightColumn $
      let
        otherTblAssetName = toHash $ StraightColumn $ fqcn otherTbl (pgCatalogInfo otherTbl)
        objTblOidCol = fqcn objTable tblCol
      in
      "(SELECT " <> otherTblAssetName <> " FROM " <> fromString otherTbl <> " WHERE " <> objTblOidCol <> "=" <> fqcn otherTbl "oid" <> ")"
    toHash (JoinOidArray tblCol otherTbl) = toHash $ StraightColumn $
      -- This one is trickier: we want to ensure order changes results
      let
        otherTblAssetName = toHash $ StraightColumn $ fqcn otherTbl (pgCatalogInfo otherTbl)
        objTblOidCol = fqcn objTable tblCol
      in
      "(SELECT ARRAY_TO_STRING(ARRAY_AGG(" <> otherTblAssetName <> " ORDER BY s.idx), ';') "
      <> " FROM pg_catalog." <> fromString otherTbl <> " JOIN UNNEST(" <> objTblOidCol <> ") WITH ORDINALITY s(elem, idx) ON " <> fqcn otherTbl "oid" <> "=s.elem)"
    isOidCol col = "pg_typeof(" <> col <> ")=pg_typeof(0::OID) OR pg_typeof(" <> col <> ")=pg_typeof('{0}'::OID[])"
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

-- | Just a helper that stores a straight column name or a col name which is an OID or an OID[], and another pg_catalog table to join on to use that object's name in the hashing instead.
data CatalogTableColumn = StraightColumn QueryFrag | JoinOid QueryFrag String | JoinOidArray QueryFrag String
instance IsString CatalogTableColumn where
  fromString = StraightColumn . fromString

withQueryFrag :: QueryFrag -> (forall a. ToRow a => Query -> a -> b) -> b
withQueryFrag (QueryFrag q args) f = f q args

queryObjNamesAndHashes2 :: (HasCallStack, MonadIO m) => DB.Connection -> QueryFrag -> [CatalogTableColumn] -> String -> QueryFrag -> m [(ObjName, ObjHash)]
queryObjNamesAndHashes2 conn objNameCol cols table afterSelect = liftIO $ withQueryFrag fullQuery (DB.query conn)
  where
    fullQuery = "SELECT " <> objNameCol <> ", " <> hashProjection2 table cols <> " FROM " <> fromString table
                     <> "\n" <> afterSelect
                     <> "\n ORDER BY " <> objNameCol

queryObjNamesAndHashes :: (HasCallStack, MonadIO m) => DB.Connection -> QueryFrag -> [QueryFrag] -> QueryFrag -> Maybe QueryFrag -> m [(ObjName, ObjHash)]
queryObjNamesAndHashes conn objNameCol hashCols table filterBy = liftIO $ withQueryFrag fullQuery (DB.query conn)
  where
    fullQuery = "SELECT " <> objNameCol <> ", " <> hashProjection hashCols <> " FROM " <> table
                     <> maybe "" (" WHERE " <>) filterBy
                     <> " ORDER BY " <> objNameCol

rawQueryObjNamesAndHashes :: (HasCallStack, MonadIO m) => DB.Connection -> QueryFrag -> [QueryFrag] -> QueryFrag -> QueryFrag -> m [(ObjName, ObjHash)]
rawQueryObjNamesAndHashes conn objNameCol hashCols table afterSelect = liftIO $ withQueryFrag fullQuery (DB.query conn)
  where
    fullQuery = "SELECT " <> objNameCol <> ", " <> hashProjection hashCols <> " FROM " <> table
                     <> " " <> afterSelect
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
    routines :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes2 conn "proname" [ JoinOid "pronamespace" "pg_namespace", JoinOid "proowner" "pg_authid", JoinOid "prolang" "pg_language", "procost", "prorows", JoinOid "provariadic" "pg_type", "prokind", "prosecdef", "proleakproof", "proisstrict", "proretset", "provolatile", "proparallel", "pronargs", "pronargdefaults", JoinOid "prorettype" "pg_type", JoinOidArray "proargtypes" "pg_type" ,"proargmodes", "proargnames", "proargdefaults", JoinOidArray "protrftypes" "pg_type", "prosrc", "probin", "proconfig", "proacl" ] "pg_catalog.pg_proc" (QueryFrag "JOIN pg_namespace ON pg_namespace.oid=pg_proc.pronamespace AND pg_namespace.nspname=?" (DB.Only schemaName))
    sequences :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "sequence_name" ["data_type", "numeric_precision", "numeric_precision_radix", "numeric_scale", "start_value", "minimum_value", "maximum_value", "increment", "cycle_option"] "information_schema.sequences" (Just $ QueryFrag "sequence_schema=?" (DB.Only schemaName))
    tableHashes <- getTablesHashes conn schemaName tables
    let allObjs = listToMap $ tableHashes ++ map (uncurry ViewHash) views ++ map (uncurry RoutineHash) routines ++ map (uncurry SequenceHash) sequences
    return (schemaName, SchemaHash schemaName schemaHash allObjs)

getTablesHashes :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> ObjName -> [(ObjName, ObjHash)] -> m [SchemaObjectHash]
getTablesHashes conn schemaName tables = forM tables $ \(tableName, tableHash) -> do
    columns :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "column_name" ["column_default", "is_nullable", "data_type", "character_maximum_length", "character_octet_length", "numeric_precision", "numeric_precision_radix", "numeric_scale", "datetime_precision", "interval_type", "interval_precision", "collation_name", "is_identity", "identity_generation", "identity_start", "identity_increment", "identity_maximum", "identity_minimum", "identity_cycle", "is_generated", "generation_expression", "is_updatable"] "information_schema.columns" (Just $ QueryFrag "table_schema=? AND table_name=?" (schemaName, tableName))
    -- constraints :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "constraint_name" ["constraint_type", "is_deferrable", "initially_deferred"] "information_schema.table_constraints" (Just $ QueryFrag "constraint_schema=? AND table_name=?" (schemaName, tableName))
    constraints :: [(ObjName, ObjHash)] <- rawQueryObjNamesAndHashes conn "conname" [ "contype", "condeferrable", "condeferred", "convalidated", "conrelid", "contypid", "conindid", "conparentid", "confrelid", "confupdtype", "confdeltype", "confmatchtype", "conislocal", "coninhcount", "connoinherit", "conkey", "confkey", "conpfeqop", "conppeqop", "conffeqop", "conexclop", "pg_get_constraintdef(pg_constraint.oid)" ] "pg_catalog.pg_constraint" (QueryFrag "JOIN pg_catalog.pg_namespace ON connamespace=pg_namespace.oid AND pg_namespace.nspname=? JOIN pg_catalog.pg_class ON conrelid=pg_class.oid AND pg_class.relname=?" (schemaName, tableName))
    -- ^ PG 10 Does not have the "conparentid" column..
    pure $ TableHash tableName tableHash (listToMap $ map (uncurry TableColumn) columns) (listToMap $ map (uncurry TableConstraint) constraints)