
module Codd.Hashing.Database where

import Codd.Hashing.Types
import Codd.Types (SqlRole(..), SqlSchema(..), CoddSettings(..), Include(..), alsoInclude)
import Codd.Hashing.Database.Model (QueryFrag(..), CatTable(..), CatalogTableColumn(..), DbVersionHash, JoinTable(..), WhereFilter(..), ToQueryFrag(..), ColumnComparison(..), withQueryFrag, (<<>), (<>>))
import Codd.Hashing.Database.Pg10 (Pg10(..))
import Codd.Hashing.Database.Pg11 (Pg11(..))
import Codd.Hashing.Database.Pg12 (Pg12(..))
import Control.Monad (forM_)
import Data.Hashable
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import Data.Traversable (for)
import Data.Typeable
import Haxl.Core
import GHC.Stack (HasCallStack)
import UnliftIO (MonadUnliftIO, MonadIO(..))

data HashReq a where
  GetHashesReq :: HashableObject -> [ObjName] -> (forall b. DbVersionHash b => [ColumnComparison b]) -> HashReq [(ObjName, ObjHash)]
  deriving stock (Typeable)

instance Eq (HashReq a) where
  GetHashesReq hobj1 ids1 _ == GetHashesReq hobj2 ids2 _ = hobj1 == hobj2 && ids1 == ids2
instance Hashable (HashReq a) where
  hashWithSalt s (GetHashesReq hobj identifyingNames _) = hashWithSalt s (hobj, identifyingNames)

instance Show (HashReq a) where
  show (GetHashesReq hobj ids _) = "GetHashesReq: " ++ show (hobj, ids)
instance ShowP HashReq where showp = show

instance StateKey HashReq where
  data State HashReq = UserState {}

instance DataSourceName HashReq where
  dataSourceName _ = "CatalogHashSource"

type HaxlEnv = (PgVersion, DB.Connection)
type Haxl = GenHaxl HaxlEnv ()

data SameQueryFormatFetch = SameQueryFormatFetch {
  uniqueIdx :: Int
  , hobj :: HashableObject
  , ids :: [ObjName]
  , qp :: QueryInPieces
  , rvar :: ResultVar [(ObjName, ObjHash)]
}

instance DataSource HaxlEnv HashReq where
  fetch _ _ (pgVer, conn) = SyncFetch combineQueriesWithWhere
    where
      fst3 (a, _, _) = a

      getResultsGroupedPerIdx :: QueryInPieces -> IO [NE.NonEmpty (Int, ObjName, ObjHash)]
      getResultsGroupedPerIdx qip =
        let
          qf = queryInPiecesToQueryFrag qip <> "\n ORDER BY artificial_idx"
        in
        NE.groupWith fst3 <$> withQueryFrag qf (DB.query conn)

      mergeResults :: [(Int, ResultVar [(ObjName, ObjHash)])] -> [NE.NonEmpty (Int, ObjName, ObjHash)] -> [(ResultVar [(ObjName, ObjHash)], [(ObjName, ObjHash)])]
      mergeResults [] [] = []
      mergeResults [] _ = error "Empty idx list but non empty results in mergeResults"
      mergeResults (i:is) [] = (snd i, []) : mergeResults is []
      mergeResults (i:is) (r:rs) =
        let
          idx = fst i
          (ridx, _, _) = NE.head r
          rvar = snd i
        in
          if idx < ridx then (rvar, []) : mergeResults is (r:rs)
          else if idx == ridx then (rvar, map (\(_, n, h) -> (n, h)) $ NE.toList r) : mergeResults is rs
          else error "idx > ridx in mergeResults"

      -- This is a different batching mechanism, which will `OR` the conditions for queries with the same
      -- HashableObject, because they have the same SELECT, FROM and JOINs. Then, prepend an increasing number to the select exprs
      -- which is 1 if the first "WHERE expression" is True, 2 when the second is True, 3 for the third and so on..
      -- Then test, because we can't be sure if this will actually be faster than a bunch of UNION ALLed queries. Hopefully it'll all just
      -- turn into a big sequential scan, so we'll have one sequential scan for tables, one for views, one for triggers and so on..
      combineQueriesWithWhere blockedFetches = do
        let
          allHashReqs :: [SameQueryFormatFetch]
          allHashReqs = zipWith (\i (a, b, c, d) -> SameQueryFormatFetch i a b c d) [1..] [ (hobj, ids, queryObjNamesAndHashesQuery pgVer hobj (joinsFor hobj, joinFilters), r) | BlockedFetch (GetHashesReq hobj ids joinFilters) r <- blockedFetches ]

          fetchesPerQueryFormat :: [NonEmpty SameQueryFormatFetch]
          fetchesPerQueryFormat = NE.groupAllWith hobj allHashReqs

          queriesPerFormat :: [(QueryInPieces, NonEmpty SameQueryFormatFetch)]
          queriesPerFormat = flip map fetchesPerQueryFormat $ \sffs@(x :| _) ->
            let
              -- This form of batching only works if the WHERE expressions of each query are mutually exclusive!
              finalQip = (qp x) {
                selectExprs = "CASE "
                          <> foldMap (\qip -> "\n WHEN " <> fromMaybe "TRUE" (identifyingWheres (qp qip)) <> QueryFrag " THEN ?" (DB.Only (uniqueIdx qip))) sffs
                          <> " END AS artificial_idx, " <> selectExprs (qp x),
                identifyingWheres = case catMaybes $ map (identifyingWheres . qp) $ NE.toList sffs of
                              [] -> Nothing
                              fs -> Just $ interspBy True " OR " fs
              }
            in (finalQip, sffs)

        forM_ queriesPerFormat $ \(qip, sffs) -> do
          -- print qip
          allResults <- getResultsGroupedPerIdx qip
          -- print allResults
          let mergedResults = mergeResults (map (\sff -> (uniqueIdx sff, rvar sff)) $ NE.toList sffs) allResults
          forM_ mergedResults $ \(rvar, res) -> putSuccess rvar res

data PgVersion = forall a. DbVersionHash a => PgVersion a

-- | Returns a SQL expression of type TEXT with the concatenation of all the non-OID columns that form the Identity of a row in the catalog table whose OID equals the supplied one.
-- This function does not consider the queried table's identifier exists in scope. It does subselects instead.
concatenatedIdentityColsOf :: (HasCallStack, DbVersionHash a) => CatTable a -> QueryFrag -> QueryFrag
concatenatedIdentityColsOf tbl oid = "(SELECT " <> otherTblIdentifyingCols <> " FROM " <> tableName tbl <> " WHERE " <> oid <> "=" <> RegularColumn tbl "oid" <<> ")"
  where
    otherTblIdentifyingCols = interspBy False " || '_' || " $ map (coal . colOf) $ fqTableIdentifyingCols tbl
    coal q = "COALESCE(" <> parens q <> "::TEXT, '')"
    colOf = \case
      PureSqlExpression c -> c
      RegularColumn tblName c -> tableName tblName <> "." <> c
      OidColumn joinTbl thisTblOidCol -> "(SELECT " <> concatenatedIdentityColsOf joinTbl thisTblOidCol <> " FROM " <> tableName joinTbl <> " WHERE " <> RegularColumn joinTbl "oid" <<> "=" <> RegularColumn tbl thisTblOidCol <<> ")"
      OidArrayColumn joinTbl thisTblOidCol -> 
             "(SELECT ARRAY_TO_STRING(ARRAY_AGG(" <> concatenatedIdentityColsOf joinTbl "s.elem" <> " ORDER BY s.idx), ';') "
          <> " FROM pg_catalog." <> tableName joinTbl <> " JOIN UNNEST(" <> thisTblOidCol <> ") WITH ORDINALITY s(elem, idx) ON " <> RegularColumn joinTbl "oid" <<> "=s.elem)"

-- | Returns a SQL expression of type TEXT with the concatenation of all the non-OID columns that form the Identity of a row in the catalog table whose OID equals the supplied one.
-- This function CONSIDERS the queried table's identifier exists in scope. It does subselects for Oid/OidArray columns only.
-- TODO: this function is nearly a copy of the other one. Can we improve this?
concatenatedIdentityColsOfInContext :: (HasCallStack, DbVersionHash a) => CatTable a -> QueryFrag
concatenatedIdentityColsOfInContext tbl = interspBy False " || '_' || " $ map (coal . colOf) $ fqTableIdentifyingCols tbl
  where
    coal q = "COALESCE(" <> parens q <> "::TEXT, '')"
    colOf = \case
      PureSqlExpression c -> c
      RegularColumn tblName c -> tableName tblName <> "." <> c
      OidColumn joinTbl thisTblOidCol -> "(SELECT " <> concatenatedIdentityColsOf joinTbl thisTblOidCol <> " FROM " <> tableName joinTbl <> " WHERE " <> RegularColumn joinTbl "oid" <<> "=" <> RegularColumn tbl thisTblOidCol <<> ")"
      OidArrayColumn joinTbl thisTblOidCol -> 
             "(SELECT ARRAY_TO_STRING(ARRAY_AGG(" <> concatenatedIdentityColsOf joinTbl "s.elem" <> " ORDER BY s.idx), ';') "
          <> " FROM pg_catalog." <> tableName joinTbl <> " JOIN UNNEST(" <> thisTblOidCol <> ") WITH ORDINALITY s(elem, idx) ON " <> RegularColumn joinTbl "oid" <<> "=s.elem)"

hashProjection :: (HasCallStack, DbVersionHash a) => CatTable a -> QueryFrag
hashProjection objTable = "MD5(" <> interspBy False " || " (map toHash cols) <> ")"
  where
    cols = hashingColsOf objTable
    hashExpr expr = "(CASE WHEN " <> expr <> " IS NULL THEN '' ELSE '_' || (" <> expr <> ") :: TEXT END)"
    toHash (PureSqlExpression col) = hashExpr col
    toHash col@(RegularColumn {}) = hashExpr $ toQueryFrag col
    toHash (OidColumn otherTbl tblCol) = hashExpr $ concatenatedIdentityColsOf otherTbl tblCol
    toHash (OidArrayColumn otherTbl tblCol) = hashExpr $
      -- This one is trickier: we want to ensure order changes results
      let
        otherTblIdentifyingCols = concatenatedIdentityColsOf otherTbl "s.elem"
        objTblOidCol = RegularColumn objTable tblCol
      in
      "(SELECT ARRAY_TO_STRING(ARRAY_AGG(" <> otherTblIdentifyingCols <<> " ORDER BY s.idx), ';') "
      <> " FROM pg_catalog." <> tableName otherTbl <> " JOIN UNNEST(" <> objTblOidCol <<> ") WITH ORDINALITY s(elem, idx) ON " <> RegularColumn otherTbl "oid" <<> "=s.elem)"

interspBy :: Bool -> QueryFrag -> [QueryFrag] -> QueryFrag
interspBy _ _ [] = ""
interspBy ps _ (c:[]) = if ps then parens c else c
interspBy ps sep (c:cs) = (if ps then parens c else c) <> sep <> interspBy ps sep cs

parens :: QueryFrag -> QueryFrag
parens q = "(" <> q <> ")"

-- | Generates an "IN" or "NOT IN" for the supplied table's object's name column.
includeSql :: DB.ToField b => Include b -> QueryFrag -> QueryFrag
includeSql inc sqlexpr =
  case inc of
    Include [] -> "FALSE"
    Exclude [] -> "TRUE"
    Exclude objNames -> sqlexpr <> QueryFrag " NOT IN ?" (DB.Only $ DB.In objNames)
    Include objNames -> sqlexpr <> QueryFrag " IN ?" (DB.Only $ DB.In objNames)
    IncludeExclude incNames excNames -> includeSql (Include incNames) sqlexpr <> " AND " <> includeSql (Exclude excNames) sqlexpr

data QueryInPieces = QueryInPieces { selectExprs :: QueryFrag, fromTbl :: QueryFrag, joinClauses :: QueryFrag, nonIdentifyingWhere :: Maybe QueryFrag, identifyingWheres :: Maybe QueryFrag } deriving Show
queryObjNamesAndHashesQuery :: PgVersion -> HashableObject -> (forall a. DbVersionHash a => ([JoinTable a], [ColumnComparison a])) -> QueryInPieces
queryObjNamesAndHashesQuery (PgVersion (_ :: a)) hobj getJoinTables = fullQuery
  where
    fullQuery = QueryInPieces (objNameCol <> " AS obj_name, " <> hashProjection objTbl) (tableName objTbl) joins (unWhereFilter <$> nonIdWhere) idWheres
    (objTbl :: CatTable a, nonIdWhere) = hashableObjCatalogTable hobj
    objNameCol = concatenatedIdentityColsOfInContext objTbl
    (joinTbls, whereFilters) = getJoinTables @a
    joins = foldMap joinStatement joinTbls
    joinStatement = \case
      JoinTable col joinTbl -> "\n JOIN " <> tableName joinTbl <> " ON " <> col <<> "=" <>> RegularColumn joinTbl "oid"
      LeftJoinTable col1 joinTbl col2 -> "\n LEFT JOIN " <> tableName joinTbl <> " ON " <> col1 <<> "=" <>> col2
    toWhereFrag (ColumnEq col v) = col <<> QueryFrag "=?" (DB.Only v)
    toWhereFrag (ColumnIn col vs) = includeSql vs (col <<> "")
    idWheres =
      case map toWhereFrag whereFilters of
        [] -> Nothing
        fs -> Just $ interspBy True " AND " fs

queryInPiecesToQueryFrag :: QueryInPieces -> QueryFrag
queryInPiecesToQueryFrag QueryInPieces{..} = "SELECT " <> selectExprs <> " FROM " <> fromTbl <> "\n" <> joinClauses <> "\n" <> maybe "" ("WHERE " <>) allWhere
  where
    allWhere = case (nonIdentifyingWhere, identifyingWheres) of
      (Nothing, Nothing) -> Nothing
      (Just w1, Nothing) -> Just w1
      (Nothing, Just w2) -> Just w2
      (Just w1, Just w2) -> Just $ parens w1 <> " AND " <> parens w2

readHashesFromDatabaseWithSettings :: (MonadUnliftIO m, MonadIO m, HasCallStack) => CoddSettings -> DB.Connection -> m DbHashes
readHashesFromDatabaseWithSettings CoddSettings { superUserConnString, schemasToHash, extraRolesToHash } conn = do
  -- Why not just select the version from the Database, parse it and with that get a type version? No configuration needed!
  -- Extensibility is a problem if we do this, but we can worry about that later, if needed
  postgresVersion <- liftIO $ DB.query conn "SHOW server_version" ()
  -- Very poor parsing follows..
  case postgresVersion of
    [ DB.Only strVersion ] -> do
      let majorVersion :: Int = truncate $ read @Float strVersion
      let rolesToHash = alsoInclude [SqlRole . Text.pack . DB.connectUser $ superUserConnString] extraRolesToHash
      case majorVersion of
        10 -> readHashesFromDatabase (PgVersion Pg10) conn schemasToHash rolesToHash
        11 -> readHashesFromDatabase (PgVersion Pg11) conn schemasToHash rolesToHash
        12 -> readHashesFromDatabase (PgVersion Pg12) conn schemasToHash rolesToHash
        _ -> error $ "Unsupported PostgreSQL version " ++ strVersion
    _ -> error "Error querying PostgreSQL version. This is a bug in Codd."
  

readHashesFromDatabase :: (MonadUnliftIO m, MonadIO m, HasCallStack) => PgVersion -> DB.Connection -> Include SqlSchema -> Include SqlRole -> m DbHashes
readHashesFromDatabase pgVer conn allSchemas allRoles = do
    let stateStore = stateSet UserState{} stateEmpty
    env0 <- liftIO $ initEnv stateStore (pgVer, conn)
    liftIO $ runHaxl env0 $ do
      -- schemas <- queryObjNamesAndHashes conn "schema_name" ["schema_owner", "default_character_set_catalog", "default_character_set_schema", "default_character_set_name" ] "information_schema.schemata" (Just $ QueryFrag "schema_name NOT IN ?" (DB.Only $ DB.In [ "information_schema" :: Text, "pg_catalog", "pg_temp_1", "pg_toast", "pg_toast_temp_1" ]))
      schemas <- dataFetch $ GetHashesReq HSchema [] (filtersForSchemas allSchemas)
      roles <- dataFetch $ GetHashesReq HRole [] (filtersForRoles allRoles)
      DbHashes <$> getSchemaHash schemas <*> pure (listToMap $ map (uncurry RoleHash) roles)

getSchemaHash :: [(ObjName, ObjHash)] -> Haxl (Map ObjName SchemaHash)
getSchemaHash schemas = fmap Map.fromList $ for schemas $ \(schemaName, schemaHash) -> do
    -- tables :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["table_type", "self_referencing_column_name", "reference_generation", "is_insertable_into", "is_typed", "commit_action"] "information_schema.tables" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    tables <- dataFetch $ GetHashesReq HTable [schemaName] (underSchemaFilter HTable schemaName)
    views <- dataFetch $ GetHashesReq HView [schemaName] (underSchemaFilter HView schemaName)
    -- views :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["view_definition", "check_option", "is_updatable", "is_insertable_into", "is_trigger_updatable", "is_trigger_deletable", "is_trigger_insertable_into"] "information_schema.views" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    -- routines :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "specific_name" ["routine_name", "routine_type", "data_type", "routine_body", "routine_definition", "external_name", "external_language", "is_deterministic", "is_null_call", "security_type"] "information_schema.routines" (Just $ QueryFrag "specific_schema=?" (DB.Only schemaName))
    routines <- dataFetch $ GetHashesReq HRoutine [schemaName] (underSchemaFilter HRoutine schemaName)
    sequences <- dataFetch $ GetHashesReq HSequence [schemaName] (underSchemaFilter HSequence schemaName)

    tableHashes <- getTablesHashes schemaName tables
    let allObjs = listToMap $ tableHashes ++ map (uncurry ViewHash) views ++ map (uncurry RoutineHash) routines ++ map (uncurry SequenceHash) sequences
    return (schemaName, SchemaHash schemaName schemaHash allObjs)

getTablesHashes :: ObjName -> [(ObjName, ObjHash)] -> Haxl [SchemaObjectHash]
getTablesHashes schemaName tables = for tables $ \(tblName, tableHash) -> do
      -- columns :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "column_name" ["column_default", "is_nullable", "data_type", "character_maximum_length", "character_octet_length", "numeric_precision", "numeric_precision_radix", "numeric_scale", "datetime_precision", "interval_type", "interval_precision", "collation_name", "is_identity", "identity_generation", "identity_start", "identity_increment", "identity_maximum", "identity_minimum", "identity_cycle", "is_generated", "generation_expression", "is_updatable"] "information_schema.columns" (Just $ QueryFrag "table_schema=? AND table_name=?" (schemaName, tblName))
      columns <- dataFetch $ GetHashesReq HColumn [schemaName, tblName] (underTableFilter HColumn schemaName tblName)
      -- constraints :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "constraint_name" ["constraint_type", "is_deferrable", "initially_deferred"] "information_schema.table_constraints" (Just $ QueryFrag "constraint_schema=? AND table_name=?" (schemaName, tableName))
      constraints <- dataFetch $ GetHashesReq HTableConstraint [schemaName, tblName] (underTableFilter HTableConstraint schemaName tblName)
      triggers <- dataFetch $ GetHashesReq HTrigger [schemaName, tblName] (underTableFilter HTrigger schemaName tblName)
      policies <- dataFetch $ GetHashesReq HPolicy [schemaName, tblName] (underTableFilter HPolicy schemaName tblName)
      pure $ TableHash tblName tableHash (listToMap $ map (uncurry TableColumn) columns)
                                         (listToMap $ map (uncurry TableConstraint) constraints)
                                         (listToMap $ map (uncurry TableTrigger) triggers)
                                         (listToMap $ map (uncurry TablePolicy) policies)