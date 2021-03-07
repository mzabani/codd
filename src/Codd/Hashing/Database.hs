
module Codd.Hashing.Database where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing.Database.Model    ( (<<>)
                                                , (<>>)
                                                , CatTableAliased(..)
                                                , CatalogTableColumn(..)
                                                , ColumnComparison(..)
                                                , DbVersionHash(..)
                                                , JoinTable(..)
                                                , QueryFrag(..)
                                                , ToQueryFrag(..)
                                                , WhereFilter(..)
                                                , tableAlias
                                                , tableNameAndAlias
                                                , withQueryFrag
                                                )
import           Codd.Hashing.Database.Pg10     ( Pg10(..) )
import           Codd.Hashing.Database.Pg11     ( Pg11(..) )
import           Codd.Hashing.Database.Pg12     ( Pg12(..) )
import           Codd.Hashing.Types
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Types                     ( Include(..)
                                                , SqlRole(..)
                                                , SqlSchema(..)
                                                , alsoInclude
                                                )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Logger           ( MonadLogger
                                                , logWarnN
                                                )
import qualified Data.Attoparsec.Text          as Parsec
import           Data.Hashable
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Traversable               ( for )
import           Data.Typeable
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.ToField
                                               as DB
import           Debug.Trace                    ( traceShowId )
import           GHC.Stack                      ( HasCallStack )
import           Haxl.Core
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
                                                )

data HashReq a where
  GetHashesReq ::HashableObject -> [ObjName] -> (forall b. DbVersionHash b => [ColumnComparison b]) -> HashReq [(ObjName, ObjHash)]
  deriving stock (Typeable)

instance Eq (HashReq a) where
  GetHashesReq hobj1 ids1 _ == GetHashesReq hobj2 ids2 _ =
    hobj1 == hobj2 && ids1 == ids2
instance Hashable (HashReq a) where
  hashWithSalt s (GetHashesReq hobj identifyingNames _) =
    hashWithSalt s (hobj, identifyingNames)

instance Show (HashReq a) where
  show (GetHashesReq hobj ids _) = "GetHashesReq: " ++ show (hobj, ids)
instance ShowP HashReq where
  showp = show

instance StateKey HashReq where
  data State HashReq = UserState {}

instance DataSourceName HashReq where
  dataSourceName _ = "CatalogHashSource"

type HaxlEnv = (PgVersion, DB.Connection, Include SqlRole)
type Haxl = GenHaxl HaxlEnv ()

data SameQueryFormatFetch = SameQueryFormatFetch
  { uniqueIdx :: Int
  , hobj      :: HashableObject
  , ids       :: [ObjName]
  , qp        :: QueryInPieces
  , rvar      :: ResultVar [(ObjName, ObjHash)]
  }

instance DataSource HaxlEnv HashReq where
  fetch _ _ (pgVer, conn, allRoles) = SyncFetch combineQueriesWithWhere
   where
    fst3 (a, _, _) = a

    getResultsGroupedPerIdx
      :: QueryInPieces -> IO [NE.NonEmpty (Int, ObjName, ObjHash)]
    getResultsGroupedPerIdx qip =
      let
        qf =
          traceShowId
            $  queryInPiecesToQueryFrag qip
            <> "\n ORDER BY artificial_idx"
      in  NE.groupWith fst3 <$> withQueryFrag qf (DB.query conn)

    mergeResults
      :: [(Int, ResultVar [(ObjName, ObjHash)])]
      -> [NE.NonEmpty (Int, ObjName, ObjHash)]
      -> [(ResultVar [(ObjName, ObjHash)], [(ObjName, ObjHash)])]
    mergeResults [] [] = []
    mergeResults [] _ =
      error "Empty idx list but non empty results in mergeResults"
    mergeResults (i : is) [] = (snd i, []) : mergeResults is []
    mergeResults (i : is) (r : rs) =
      let idx          = fst i
          (ridx, _, _) = NE.head r
          rvar         = snd i
      in  if idx < ridx
            then (rvar, []) : mergeResults is (r : rs)
            else if idx == ridx
              then
                (rvar, map (\(_, n, h) -> (n, h)) $ NE.toList r)
                  : mergeResults is rs
              else error "idx > ridx in mergeResults"

    -- This is a different batching mechanism, which will `OR` the conditions for queries with the same
    -- HashableObject, because they have the same SELECT, FROM and JOINs. Then, prepend an increasing number to the select exprs
    -- which is 1 if the first "WHERE expression" is True, 2 when the second is True, 3 for the third and so on..
    -- Hopefully it'll all just turn into a big sequential scan, so we'll have one sequential scan for tables,
    -- one for views, one for triggers and so on..
    combineQueriesWithWhere blockedFetches = do
      let
        allHashReqs :: [SameQueryFormatFetch]
        allHashReqs = zipWith
          (\i (a, b, c, d) -> SameQueryFormatFetch i a b c d)
          [1 ..]
          [ ( hobj
            , ids
            , queryObjNamesAndHashesQuery pgVer
                                          hobj
                                          allRoles
                                          (joinsFor hobj, joinFilters)
            , r
            )
          | BlockedFetch (GetHashesReq hobj ids joinFilters) r <- blockedFetches
          ]

        fetchesPerQueryFormat :: [NonEmpty SameQueryFormatFetch]
        fetchesPerQueryFormat = NE.groupAllWith hobj allHashReqs

        queriesPerFormat :: [(QueryInPieces, NonEmpty SameQueryFormatFetch)]
        queriesPerFormat = flip map fetchesPerQueryFormat $ \sffs@(x :| _) ->
          let
            -- This form of batching only works if the WHERE expressions of each query are mutually exclusive!
              finalQip = (qp x)
                { selectExprs       = "CASE "
                                      <> foldMap
                                           (\qip ->
                                             "\n WHEN "
                                               <> fromMaybe
                                                    "TRUE"
                                                    (identifyingWheres (qp qip))
                                               <> QueryFrag
                                                    " THEN ?"
                                                    (DB.Only (uniqueIdx qip))
                                           )
                                           sffs
                                      <> " END AS artificial_idx, "
                                      <> selectExprs (qp x)
                , identifyingWheres = case
                                        mapMaybe (identifyingWheres . qp)
                                          $ NE.toList sffs
                                      of
                                        [] -> Nothing
                                        fs -> Just $ interspBy True " OR " fs
                , groupByExprs      = Just $ maybe "artificial_idx"
                                                   (<> ", artificial_idx")
                                                   (groupByExprs (qp x))
                }
          in  (finalQip, sffs)

      forM_ queriesPerFormat $ \(qip, sffs) -> do
        print qip
        allResults <- getResultsGroupedPerIdx qip
        -- print allResults
        let mergedResults = mergeResults
              (map (\sff -> (uniqueIdx sff, rvar sff)) $ NE.toList sffs)
              allResults
        forM_ mergedResults $ uncurry putSuccess

data PgVersion = forall a . DbVersionHash a => PgVersion a

-- | Returns a SQL expression of type TEXT with the concatenation of all the non-OID columns that form the Identity of a row in the catalog table whose OID equals the supplied one.
-- This function does not consider the queried table's identifier exists in scope. It does subselects instead.
concatenatedIdentityColsOf
  :: (HasCallStack, DbVersionHash a)
  => CatTableAliased a
  -> QueryFrag
  -> QueryFrag
concatenatedIdentityColsOf tbl oid = finalQuery
 where
  finalQuery =
    "(SELECT "
      <>  otherTblIdentifyingCols
      <>  " FROM "
      <>  tableNameAndAlias tbl
      <>  " WHERE "
      <>  oid
      <>  "="
      <>  RegularColumn tbl "oid"
      <<> ")"
  otherTblIdentifyingCols =
    interspBy False " || '_' || " $ map (coal . colOf) $ fqTableIdentifyingCols
      tbl
  coal q = "COALESCE(" <> parens q <> "::TEXT, '')"
  colOf = \case
    PureSqlExpression c      -> c
    RegularColumn  tblName c -> tableAlias tblName <> "." <> c
    AclItemsColumn _       _ -> error "AclItemsColumn ColOf"
    OidColumn joinTbl thisTblOidCol ->
      "(SELECT "
        <>  concatenatedIdentityColsOf joinTbl (toQueryFrag thisTblOidCol)
        <>  " FROM "
        <>  tableNameAndAlias joinTbl
        <>  " WHERE "
        <>  RegularColumn joinTbl "oid"
        <<> "="
        <>  thisTblOidCol
        <<> ")"
    OidArrayColumn joinTbl thisTblOidCol ->
      "(SELECT ARRAY_TO_STRING(ARRAY_AGG("
        <>  concatenatedIdentityColsOf joinTbl "s.elem"
        <>  " ORDER BY s.idx), ';') "
        <>  " FROM pg_catalog."
        <>  tableNameAndAlias joinTbl
        <>  " JOIN UNNEST("
        <>  thisTblOidCol
        <>  ") WITH ORDINALITY s(elem, idx) ON "
        <>  RegularColumn joinTbl "oid"
        <<> "=s.elem)"

-- | Returns a SQL expression of type TEXT with the concatenation of all the non-OID columns that form the Identity of a row in the catalog table whose OID equals the supplied one.
-- This function CONSIDERS the queried table's identifier exists in scope. It does subselects for Oid/OidArray columns only.
-- TODO: this function is nearly a copy of the other one. Can we improve this?
concatenatedIdentityColsOfInContext
  :: (HasCallStack, DbVersionHash a) => CatTableAliased a -> QueryFrag
concatenatedIdentityColsOfInContext tbl =
  interspBy False " || '_' || " $ map (coal . colOf) $ fqTableIdentifyingCols
    tbl
 where
  coal q = "COALESCE(" <> parens q <> "::TEXT, '')"
  colOf = \case
    PureSqlExpression c      -> c
    RegularColumn  tblName c -> tableAlias tblName <> "." <> c
    AclItemsColumn _       _ -> error "AclItemsColumn ColsOfInContext"
    OidColumn joinTbl thisTblOidCol ->
      "(SELECT "
        <>  concatenatedIdentityColsOf joinTbl (toQueryFrag thisTblOidCol)
        <>  " FROM "
        <>  tableNameAndAlias joinTbl
        <>  " WHERE "
        <>  RegularColumn joinTbl "oid"
        <<> "="
        <>  thisTblOidCol
        <<> ")"
    OidArrayColumn joinTbl thisTblOidCol ->
      "(SELECT ARRAY_TO_STRING(ARRAY_AGG("
        <>  concatenatedIdentityColsOf joinTbl "s.elem"
        <>  " ORDER BY s.idx), ';') "
        <>  " FROM pg_catalog."
        <>  tableNameAndAlias joinTbl
        <>  " JOIN UNNEST("
        <>  thisTblOidCol
        <>  ") WITH ORDINALITY s(elem, idx) ON "
        <>  RegularColumn joinTbl "oid"
        <<> "=s.elem)"

-- | Returns a single-column SQL expression that returns the MD5 hash of all columns for the supplied table/object,
-- considering object permissions only for *allRoles*, however.
hashProjection
  :: (HasCallStack, DbVersionHash a)
  => CatTableAliased a
  -> Include SqlRole
  -> QueryFrag
hashProjection objTable allRoles =
  "MD5(" <> interspBy False " || " (map toHash cols) <> ")"
 where
  cols = hashingColsOf objTable
  hashExpr expr =
    "(CASE WHEN "
      <> expr
      <> " IS NULL THEN '' ELSE '_' || ("
      <> expr
      <> ") :: TEXT END)"
  toHash (PureSqlExpression col) = hashExpr col
  toHash col@RegularColumn{}     = hashExpr $ toQueryFrag col
  toHash col@AclItemsColumn{} =
    let acls = "(ACLEXPLODE(" <> toQueryFrag col <> "))"
    in
      -- We only include mapped roles for grantees, not for grantors.
      -- Grantee 0 is PUBLIC, which we always want to include.
      -- NOTE: It is not clear what being a grantor means, so we remain open
      -- to having to include 
      hashExpr
      $ "(SELECT ARRAY_AGG(grantee_role.rolname || ';' || privilege_type || ';' || is_grantable ORDER BY COALESCE(grantee_role.rolname, ''), privilege_type, is_grantable) FROM (SELECT "
      <> acls
      <> ".grantee, "
      <> acls
      <> ".privilege_type, "
      <> acls
      <> ".is_grantable) perms_subq "
      <> "\n LEFT JOIN pg_catalog.pg_roles grantee_role ON grantee_role.oid=perms_subq.grantee "
      <> "\n WHERE grantee_role.rolname IS NULL OR "
      <> includeSql allRoles "grantee_role.rolname"
      <> ")"
  toHash (OidColumn otherTbl tblCol) =
    hashExpr $ concatenatedIdentityColsOf otherTbl (toQueryFrag tblCol)
  toHash (OidArrayColumn otherTbl tblCol) =
    hashExpr
      $
    -- This one is trickier: we want to ensure order changes results
        let otherTblIdentifyingCols =
              concatenatedIdentityColsOf otherTbl "s.elem"
            objTblOidCol = RegularColumn objTable tblCol
        in  "(SELECT ARRAY_TO_STRING(ARRAY_AGG("
            <>  otherTblIdentifyingCols
            <<> " ORDER BY s.idx), ';') "
            <>  " FROM pg_catalog."
            <>  tableNameAndAlias otherTbl
            <>  " JOIN UNNEST("
            <>  objTblOidCol
            <<> ") WITH ORDINALITY s(elem, idx) ON "
            <>  RegularColumn otherTbl "oid"
            <<> "=s.elem)"

interspBy :: Bool -> QueryFrag -> [QueryFrag] -> QueryFrag
interspBy _  _ []  = ""
interspBy ps _ [c] = if ps then parens c else c
interspBy ps sep (c : cs) =
  (if ps then parens c else c) <> sep <> interspBy ps sep cs

parens :: QueryFrag -> QueryFrag
parens q = "(" <> q <> ")"

-- | Generates an "IN" or "NOT IN" for the supplied table's object's name column.
includeSql :: DB.ToField b => Include b -> QueryFrag -> QueryFrag
includeSql inc sqlexpr = case inc of
  Include [] -> "FALSE"
  Exclude [] -> "TRUE"
  Exclude objNames ->
    sqlexpr <> QueryFrag " NOT IN ?" (DB.Only $ DB.In objNames)
  Include objNames -> sqlexpr <> QueryFrag " IN ?" (DB.Only $ DB.In objNames)
  IncludeExclude incNames excNames ->
    includeSql (Include incNames) sqlexpr
      <> " AND "
      <> includeSql (Exclude excNames) sqlexpr

data QueryInPieces = QueryInPieces
  { selectExprs         :: QueryFrag
  , fromTbl             :: QueryFrag
  , joinClauses         :: QueryFrag
  , nonIdentifyingWhere :: Maybe QueryFrag
  , identifyingWheres   :: Maybe QueryFrag
  , groupByExprs        :: Maybe QueryFrag
  }
  deriving Show
queryObjNamesAndHashesQuery
  :: PgVersion
  -> HashableObject
  -> Include SqlRole
  -> (  forall a
      . DbVersionHash a
     => ([JoinTable a], [ColumnComparison a])
     )
  -> QueryInPieces
queryObjNamesAndHashesQuery (PgVersion (_ :: a)) hobj allRoles getJoinTables =
  fullQuery
 where
  fullQuery = QueryInPieces
    (  objNameCol
    <> " AS obj_name, MD5(ARRAY_TO_STRING(ARRAY_AGG("
    <> hashProjection objTbl allRoles
    <> "), ';'))"
    )
    (tableNameAndAlias objTbl)
    joins
    (unWhereFilter <$> nonIdWhere)
    idWheres
    (Just "obj_name")
  (objTbl :: CatTableAliased a, nonIdWhere) = hashableObjCatalogTable hobj
  objNameCol               = concatenatedIdentityColsOfInContext objTbl
  (joinTbls, whereFilters) = getJoinTables @a
  joins                    = foldMap joinStatement joinTbls
  joinStatement            = \case
    JoinTable col joinTbl ->
      "\n JOIN "
        <>  tableNameAndAlias joinTbl
        <>  " ON "
        <>  col
        <<> "="
        <>> RegularColumn joinTbl "oid"
    LeftJoinTable col1 joinTbl col2 ->
      "\n LEFT JOIN "
        <>  tableNameAndAlias joinTbl
        <>  " ON "
        <>  col1
        <<> "="
        <>> col2
    JoinTableFull joinTbl cols ->
      "\n JOIN " <> tableNameAndAlias joinTbl <> " ON " <>> interspBy
        False
        " AND "
        (map (\(col1, col2) -> col1 <<> "=" <>> col2) cols)
  toWhereFrag (ColumnEq col v ) = col <<> QueryFrag "=?" (DB.Only v)
  toWhereFrag (ColumnIn col vs) = includeSql vs (col <<> "")
  idWheres = case map toWhereFrag whereFilters of
    [] -> Nothing
    fs -> Just $ interspBy True " AND " fs

queryInPiecesToQueryFrag :: QueryInPieces -> QueryFrag
queryInPiecesToQueryFrag QueryInPieces {..} =
  "SELECT "
    <> selectExprs
    <> "\n FROM "
    <> fromTbl
    <> "\n"
    <> joinClauses
    <> maybe "" ("\n WHERE " <>)    allWhere
    <> maybe "" ("\n GROUP BY " <>) groupByExprs
 where
  allWhere = case (nonIdentifyingWhere, identifyingWheres) of
    (Nothing, Nothing) -> Nothing
    (Just w1, Nothing) -> Just w1
    (Nothing, Just w2) -> Just w2
    (Just w1, Just w2) -> Just $ parens w1 <> " AND " <> parens w2

readHashesFromDatabaseWithSettings
  :: (MonadUnliftIO m, MonadIO m, MonadLogger m, HasCallStack)
  => CoddSettings
  -> DB.Connection
  -> m DbHashes
readHashesFromDatabaseWithSettings CoddSettings { superUserConnString, schemasToHash, extraRolesToHash } conn
  = do
  -- Why not just select the version from the Database, parse it and with that get a type version? No configuration needed!
  -- Extensibility is a problem if we do this, but we can worry about that later, if needed
    strVersion :: Text <-
      DB.fromOnly <$> unsafeQuery1 conn "SHOW server_version_num" ()
    case Parsec.parseOnly (Parsec.decimal <* Parsec.endOfInput) strVersion of
      Left _ -> error $ "Non-integral server_version_num: " <> show strVersion
      Right (numVersion :: Int) -> do
        let majorVersion = numVersion `div` 10000
            rolesToHash  = alsoInclude
              [SqlRole . Text.pack . DB.connectUser $ superUserConnString]
              extraRolesToHash
        case majorVersion of
          10 -> readHashesFromDatabase (PgVersion Pg10)
                                       conn
                                       schemasToHash
                                       rolesToHash
          11 -> readHashesFromDatabase (PgVersion Pg11)
                                       conn
                                       schemasToHash
                                       rolesToHash
          12 -> readHashesFromDatabase (PgVersion Pg12)
                                       conn
                                       schemasToHash
                                       rolesToHash
          v
            | v < 13 -> error
            $  "Unsupported PostgreSQL version "
            ++ show majorVersion
            | otherwise -> do
              logWarnN
                $ "Not all features of PostgreSQL version "
                <> Text.pack (show majorVersion)
                <> " may be supported by codd. Please file an issue for us to support this newer version properly."
              readHashesFromDatabase (PgVersion Pg12)
                                     conn
                                     schemasToHash
                                     rolesToHash

readHashesFromDatabase
  :: (MonadUnliftIO m, MonadIO m, HasCallStack)
  => PgVersion
  -> DB.Connection
  -> Include SqlSchema
  -> Include SqlRole
  -> m DbHashes
readHashesFromDatabase pgVer conn allSchemas allRoles = do
  let stateStore = stateSet UserState{} stateEmpty
  env0 <- liftIO $ initEnv stateStore (pgVer, conn, allRoles)
  liftIO $ runHaxl env0 $ do
    allDbSettings <- dataFetch $ GetHashesReq HDatabaseSettings [] []
    roles <- dataFetch $ GetHashesReq HRole [] (filtersForRoles allRoles)
    schemas <- dataFetch
      $ GetHashesReq HSchema [] (filtersForSchemas allSchemas)
    let
      dbSettings = case allDbSettings of
        [(_, h)] -> h
        _ ->
          error
            "More than one database returned from pg_database. Please file a bug."
    DbHashes dbSettings <$> getSchemaHash schemas <*> pure
      (listToMap $ map (uncurry RoleHash) roles)

getSchemaHash :: [(ObjName, ObjHash)] -> Haxl (Map ObjName SchemaHash)
getSchemaHash schemas =
  fmap Map.fromList $ for schemas $ \(schemaName, schemaHash) -> do
    tables <- dataFetch
      $ GetHashesReq HTable [schemaName] (underSchemaFilter HTable schemaName)
    views <- dataFetch
      $ GetHashesReq HView [schemaName] (underSchemaFilter HView schemaName)
    routines <- dataFetch $ GetHashesReq
      HRoutine
      [schemaName]
      (underSchemaFilter HRoutine schemaName)
    sequences <- dataFetch $ GetHashesReq
      HSequence
      [schemaName]
      (underSchemaFilter HSequence schemaName)

    tableHashes <- getTablesHashes schemaName tables
    let allObjs =
          listToMap
            $  tableHashes
            ++ map (uncurry ViewHash)     views
            ++ map (uncurry RoutineHash)  routines
            ++ map (uncurry SequenceHash) sequences
    return (schemaName, SchemaHash schemaName schemaHash allObjs)

getTablesHashes :: ObjName -> [(ObjName, ObjHash)] -> Haxl [SchemaObjectHash]
getTablesHashes schemaName tables = for tables $ \(tblName, tableHash) -> do
  columns <- dataFetch $ GetHashesReq
    HColumn
    [schemaName, tblName]
    (underTableFilter HColumn schemaName tblName)
  constraints <- dataFetch $ GetHashesReq
    HTableConstraint
    [schemaName, tblName]
    (underTableFilter HTableConstraint schemaName tblName)
  triggers <- dataFetch $ GetHashesReq
    HTrigger
    [schemaName, tblName]
    (underTableFilter HTrigger schemaName tblName)
  policies <- dataFetch $ GetHashesReq
    HPolicy
    [schemaName, tblName]
    (underTableFilter HPolicy schemaName tblName)
  indexes <- dataFetch $ GetHashesReq
    HIndex
    [schemaName, tblName]
    (underTableFilter HIndex schemaName tblName)
  pure $ TableHash tblName
                   tableHash
                   (listToMap $ map (uncurry TableColumn) columns)
                   (listToMap $ map (uncurry TableConstraint) constraints)
                   (listToMap $ map (uncurry TableTrigger) triggers)
                   (listToMap $ map (uncurry TablePolicy) policies)
                   (listToMap $ map (uncurry TableIndex) indexes)
