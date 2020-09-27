{-# LANGUAGE AllowAmbiguousTypes #-}
module Codd.Hashing.Database where

import Codd.Hashing.Types
import Codd.Types (SqlRole(..), SqlSchema(..), CoddSettings(..), Include(..), alsoInclude)
import Codd.Hashing.Database.Model (QueryFrag(..), CatTable(..), CatalogTableColumn(..), DbVersionHash, JoinFilter(..), withQueryFrag, fqcn, (<<>), (<>>))
import Codd.Hashing.Database.Pg10 (Pg10)
import Codd.Hashing.Database.Pg11 (Pg11)
import Codd.Hashing.Database.Pg12 (Pg12)
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import GHC.Stack (HasCallStack)
import UnliftIO (MonadUnliftIO, MonadIO(..))

-- | Returns a SQL expression of type TEXT with the concatenation of all the non-OID columns that form the Identity of a row in the catalog table whose OID equals the supplied one.
concatenatedIdentityColsOf :: (HasCallStack, DbVersionHash a) => CatTable a -> QueryFrag -> QueryFrag
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

hashProjection :: (HasCallStack, DbVersionHash a) => CatTable a -> QueryFrag
hashProjection objTable = "MD5(" <> interspBy " || " (map toHash cols) <> ")"
  where
    cols = hashingColsOf objTable
    hashExpr expr = "(CASE WHEN " <> expr <> " IS NULL THEN '' ELSE '_' || (" <> expr <> ") :: TEXT END)"
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


newtype WhereFilter = WhereFilter { unWhereFilter :: QueryFrag }
queryObjNamesAndHashes :: forall a m. (HasCallStack, DbVersionHash a, MonadIO m) => DB.Connection -> HashableObject -> [JoinFilter a] -> [WhereFilter] -> m [(ObjName, ObjHash)]
queryObjNamesAndHashes conn hobj joinFilters whereFilters = liftIO $ withQueryFrag fullQuery (DB.query conn)
  where
    (objTbl :: CatTable a, mfilters) = hashableObjCatalogTable hobj
    objNameCol = fqObjNameCol objTbl
    fullQuery = "SELECT " <> objNameCol <<> ", " <> hashProjection objTbl <> " FROM " <> tableName objTbl
                     <> joins
                     <> whereClause
                     <> "\n ORDER BY " <>> objNameCol -- TODO: Does ordering matter? We should be careful to order in memory when hashing
    -- TODO: Not all catalog tables have "oid" as their identifier column
    joins = foldMap (\(JoinFilter col joinTbl joinedTblObjName) -> "\n JOIN " <> tableName joinTbl <> " ON " <> fqcn objTbl col <<> "=" <> fqcn joinTbl "oid" <<> " AND " <> fqObjNameCol joinTbl <<> QueryFrag "=?" (DB.Only joinedTblObjName)) joinFilters
    whereClause =
      case (mfilters, map unWhereFilter whereFilters) of
        (Nothing, []) -> ""
        (Just f1, []) -> "\n WHERE " <>> f1
        (Just f1, fs) -> "\n WHERE (" <> f1 <> ") AND (" <> interspBy " AND " fs <> ")"
        (Nothing, fs) -> "\n WHERE " <> interspBy " AND " fs

readHashesFromDatabaseWithSettings :: (MonadUnliftIO m, MonadIO m, HasCallStack) => CoddSettings -> DB.Connection -> m DbHashes
readHashesFromDatabaseWithSettings CoddSettings { superUserConnString, appUser, schemasToHash, extraRolesToHash } conn = do
  -- Why not just select the version from the Database, parse it and with that get a type version? No configuration needed!
  -- Extensibility is a problem if we do this, but we can worry about that later, if needed
  postgresVersion <- liftIO $ DB.query conn "SHOW server_version" ()
  -- Very poor parsing follows..
  case postgresVersion of
    [ DB.Only strVersion ] -> do
      let majorVersion :: Int = truncate $ read @Float strVersion
      let rolesToHash = alsoInclude [appUser, SqlRole . Text.pack . DB.connectUser $ superUserConnString] extraRolesToHash
      case majorVersion of
        10 -> readHashesFromDatabase @Pg10 conn schemasToHash rolesToHash
        11 -> readHashesFromDatabase @Pg11 conn schemasToHash rolesToHash
        12 -> readHashesFromDatabase @Pg12 conn schemasToHash rolesToHash
        _ -> error $ "Unsupported PostgreSQL version " ++ strVersion
    _ -> error "Error querying PostgreSQL version. This is a bug in Codd."
  

readHashesFromDatabase :: forall a m. (MonadUnliftIO m, MonadIO m, HasCallStack, DbVersionHash a) => DB.Connection -> Include SqlSchema -> Include SqlRole -> m DbHashes
readHashesFromDatabase conn allSchemas allRoles = do
    -- schemas <- queryObjNamesAndHashes conn "schema_name" ["schema_owner", "default_character_set_catalog", "default_character_set_schema", "default_character_set_name" ] "information_schema.schemata" (Just $ QueryFrag "schema_name NOT IN ?" (DB.Only $ DB.In [ "information_schema" :: Text, "pg_catalog", "pg_temp_1", "pg_toast", "pg_toast_temp_1" ]))
    schemas <- queryObjNamesAndHashes @a conn HSchema [] [ WhereFilter (includeSql allSchemas HSchema) ]
    roles <- queryObjNamesAndHashes @a conn HRole [] [ WhereFilter (includeSql allRoles HRole) ]
    DbHashes <$> getSchemaHash @a conn schemas <*> pure (listToMap $ map (uncurry RoleHash) roles)
    where
      includeSql :: DB.ToField b => Include b -> HashableObject -> QueryFrag
      includeSql inc hobj =
        let
          (tbl :: CatTable a, _) = hashableObjCatalogTable hobj
        in
        case inc of
          Include [] -> "FALSE"
          Exclude [] -> "TRUE"
          Exclude objNames -> fqObjNameCol tbl <<> QueryFrag " NOT IN ?" (DB.Only $ DB.In objNames)
          Include objNames -> fqObjNameCol tbl <<> QueryFrag " IN ?" (DB.Only $ DB.In objNames)
          IncludeExclude incNames excNames -> includeSql (Include incNames) hobj <> " AND " <> includeSql (Exclude excNames) hobj

getSchemaHash :: forall a m. (MonadUnliftIO m, MonadIO m, HasCallStack, DbVersionHash a) => DB.Connection -> [(ObjName, ObjHash)] -> m (Map ObjName SchemaHash)
getSchemaHash conn schemas = fmap Map.fromList $ forM schemas $ \(schemaName, schemaHash) -> do
    -- TODO: do it all in a single query? it'd be nice to batch.. we could batch n+1 queries with haxl!!
    -- tables :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["table_type", "self_referencing_column_name", "reference_generation", "is_insertable_into", "is_typed", "commit_action"] "information_schema.tables" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    -- tables <- queryObjNamesAndHashes conn HTable [JoinFilter "relnamespace" PgNamespace schemaName] []
    tables <- queryObjNamesAndHashes @a conn HTable (underSchemaFilter HTable schemaName) []
    -- views <- queryObjNamesAndHashes conn HView [JoinFilter "relnamespace" PgNamespace schemaName] []
    views <- queryObjNamesAndHashes @a conn HView (underSchemaFilter HView schemaName) []
    -- views :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["view_definition", "check_option", "is_updatable", "is_insertable_into", "is_trigger_updatable", "is_trigger_deletable", "is_trigger_insertable_into"] "information_schema.views" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    -- routines :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "specific_name" ["routine_name", "routine_type", "data_type", "routine_body", "routine_definition", "external_name", "external_language", "is_deterministic", "is_null_call", "security_type"] "information_schema.routines" (Just $ QueryFrag "specific_schema=?" (DB.Only schemaName))
    -- routines <- queryObjNamesAndHashes conn HRoutine [JoinFilter "pronamespace" PgNamespace schemaName] []
    routines <- queryObjNamesAndHashes @a conn HRoutine (underSchemaFilter HRoutine schemaName) []
    -- sequences <- queryObjNamesAndHashes conn HSequence [ JoinFilter "relnamespace" PgNamespace schemaName ] []
    sequences <- queryObjNamesAndHashes @a conn HSequence (underSchemaFilter HSequence schemaName) []

    tableHashes <- getTablesHashes @a conn schemaName tables
    let allObjs = listToMap $ tableHashes ++ map (uncurry ViewHash) views ++ map (uncurry RoutineHash) routines ++ map (uncurry SequenceHash) sequences
    return (schemaName, SchemaHash schemaName schemaHash allObjs)

getTablesHashes :: forall a m. (MonadUnliftIO m, MonadIO m, HasCallStack, DbVersionHash a) => DB.Connection -> ObjName -> [(ObjName, ObjHash)] -> m [SchemaObjectHash]
getTablesHashes conn _schemaName tables = forM tables $ \(tblName, tableHash) -> do
    -- columns :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "column_name" ["column_default", "is_nullable", "data_type", "character_maximum_length", "character_octet_length", "numeric_precision", "numeric_precision_radix", "numeric_scale", "datetime_precision", "interval_type", "interval_precision", "collation_name", "is_identity", "identity_generation", "identity_start", "identity_increment", "identity_maximum", "identity_minimum", "identity_cycle", "is_generated", "generation_expression", "is_updatable"] "information_schema.columns" (Just $ QueryFrag "table_schema=? AND table_name=?" (schemaName, tblName))
    -- columns <- queryObjNamesAndHashes conn HColumn [ JoinFilter "attrelid" PgClass tblName ] []
    columns <- queryObjNamesAndHashes @a conn HColumn (underTableFilter HColumn tblName) []
    -- ^ PG 11 does not support the "attgenerated" column
    -- constraints :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "constraint_name" ["constraint_type", "is_deferrable", "initially_deferred"] "information_schema.table_constraints" (Just $ QueryFrag "constraint_schema=? AND table_name=?" (schemaName, tableName))
    -- constraints <- queryObjNamesAndHashes conn HTableConstraint [JoinFilter "connamespace" PgNamespace schemaName, JoinFilter "conrelid" PgClass tblName ] []
    constraints <- queryObjNamesAndHashes @a conn HTableConstraint (underTableFilter HTableConstraint tblName) []
    -- ^ PG 10 Does not have the "conparentid" column..
    -- triggers <- queryObjNamesAndHashes conn HTrigger [JoinFilter "tgrelid" PgClass tblName ] []
    triggers <- queryObjNamesAndHashes @a conn HTrigger (underTableFilter HTrigger tblName) []
    pure $ TableHash tblName tableHash (listToMap $ map (uncurry TableColumn) columns)
                                       (listToMap $ map (uncurry TableConstraint) constraints)
                                       (listToMap $ map (uncurry TableTrigger) triggers)