module Codd.Hashing.Database
  ( queryServerMajorVersion
  , readHashesFromDatabase
  , readHashesFromDatabaseWithSettings
  ) where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing.Database.Model    ( HashQuery(..)
                                                , QueryFrag(..)
                                                , withQueryFrag
                                                )
import qualified Codd.Hashing.Database.Pg10    as Pg10
import qualified Codd.Hashing.Database.Pg11    as Pg11
import qualified Codd.Hashing.Database.Pg12    as Pg12
import qualified Codd.Hashing.Database.Pg13    as Pg13
import qualified Codd.Hashing.Database.Pg14    as Pg14
import           Codd.Hashing.Database.SqlGen   ( interspBy
                                                , parens
                                                )
import           Codd.Hashing.Types
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Types                     ( ChecksumAlgo
                                                , SchemaSelection
                                                , SqlRole(..)
                                                )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Logger           ( MonadLogger
                                                , logWarnN
                                                )
import           Data.Aeson                     ( Value )
import qualified Data.Attoparsec.Text          as Parsec
import           Data.Hashable
import           Data.List                      ( foldl' )
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
import           GHC.Stack                      ( HasCallStack )
import           Haxl.Core
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
                                                )

data HashReq a where
  GetHashesReq ::HashableObject -> Maybe ObjName -> Maybe ObjName -> HashReq [(ObjName, Value)]
  deriving stock (Typeable)

instance Eq (HashReq a) where
  GetHashesReq hobj1 sn1 tn1 == GetHashesReq sqHobj sn2 tn2 =
    (hobj1, sn1, tn1) == (sqHobj, sn2, tn2)

instance Hashable (HashReq a) where
  hashWithSalt s (GetHashesReq hobj sn tn) = hashWithSalt s (hobj, sn, tn)


instance Show (HashReq a) where
  show (GetHashesReq hobj sn tn) = "GetHashesReq: " ++ show (hobj, sn, tn)

instance ShowP HashReq where
  showp = show


instance StateKey HashReq where
  data State HashReq = UserState {}


instance DataSourceName HashReq where
  dataSourceName _ = "CatalogHashSource"

type HaxlEnv
  = (PgVersionHasher, DB.Connection, SchemaSelection, [SqlRole], ChecksumAlgo)
type Haxl = GenHaxl HaxlEnv ()

data SameQueryFormatFetch = SameQueryFormatFetch
  { sqUniqueIdx :: Int
  , sqHobj      :: HashableObject
  , sqQueryObj  :: HashQuery
  , sqResults   :: ResultVar [(ObjName, Value)]
  }

instance DataSource HaxlEnv HashReq where
  fetch _ _ (hashQueryFor, conn, allSchemas, allRoles, checksumAlgo) =
    SyncFetch combineQueriesWithWhere
   where
    fst3 (a, _, _) = a

    getResultsGroupedPerIdx
      :: QueryInPieces -> IO [NE.NonEmpty (Int, ObjName, Value)]
    getResultsGroupedPerIdx qip =
      let qf = queryInPiecesToQueryFrag qip <> "\n ORDER BY artificial_idx"
      in  NE.groupWith fst3 <$> withQueryFrag qf (DB.query conn)

    mergeResults
      :: [(Int, ResultVar [(ObjName, Value)])]
      -> [NE.NonEmpty (Int, ObjName, Value)]
      -> [(ResultVar [(ObjName, Value)], [(ObjName, Value)])]
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

    -- This is a batching mechanism which will `OR` the conditions for queries with the same
    -- HashableObject, because they have the same SELECT, FROM and JOINs. Then, prepend an increasing number to the select exprs
    -- which is 1 if the first `identWhere` expression is True, 2 when the second is True, 3 for the third and so on..
    -- This requires mutually exclusive where conditions (`identWhere` satisfies that) and batches queries for objects
    -- of the same kind (e.g. all tables or all views or all triggers etc.) into a single query.
    combineQueriesWithWhere blockedFetches = do
      let
        allHashReqs :: [SameQueryFormatFetch]
        allHashReqs = zipWith
          (\i (a, c, d) -> SameQueryFormatFetch i a c d)
          [1 ..]
          [ ( hobj
            , hashQueryFor allRoles
                           allSchemas
                           checksumAlgo
                           schemaName
                           tblName
                           hobj
            , r
            )
          | BlockedFetch (GetHashesReq hobj schemaName tblName) r <-
            blockedFetches
          ]

        fetchesPerQueryFormat :: [NonEmpty SameQueryFormatFetch]
        fetchesPerQueryFormat = NE.groupAllWith sqHobj allHashReqs

        queriesPerFormat :: [(QueryInPieces, NonEmpty SameQueryFormatFetch)]
        queriesPerFormat = flip map fetchesPerQueryFormat $ \sffs@(x :| _) ->
          let
            -- We use nested jsonb_insert because it throws errors for duplicate keys
            jsonObject =
              foldl'
                  (\accQuery (field, value) ->
                    "JSONB_INSERT("
                      <> accQuery
                      <> ", '{"
                      <> field
                      <> "}'::text[], COALESCE(TO_JSONB("
                      <> value
                      <> "), 'null'::jsonb))"
                  )
                  "'{}'::jsonb"
                $ checksumCols (sqQueryObj x)
            -- TODO: Remove finalHashExpr identifier?
            finalHashExpr = jsonObject
            finalQip      = QueryInPieces
              { selectExprs         = "CASE "
                                      <> foldMap
                                           (\qip ->
                                             "\n WHEN "
                                               <> fromMaybe
                                                    "TRUE"
                                                    (identWhere (sqQueryObj qip))
                                               <> QueryFrag
                                                    " THEN ?"
                                                    (DB.Only (sqUniqueIdx qip))
                                           )
                                           sffs
                                      <> " END AS artificial_idx, "
                                      <> objNameCol (sqQueryObj x)
                                      <> ", "
                                      <> finalHashExpr
              , fromTbl             = fromTable (sqQueryObj x)
              , joinClauses         = joins (sqQueryObj x)
              , nonIdentifyingWhere = nonIdentWhere (sqQueryObj x)
              , identifyingWheres   = case
                                        mapMaybe (identWhere . sqQueryObj)
                                          $ NE.toList sffs
                                      of
                                        [] -> Nothing
                                        fs -> Just $ interspBy True " OR " fs
              , groupByExprs        = if null (groupByCols (sqQueryObj x))
                then Nothing
                else
                  Just $ interspBy False ", " $ "artificial_idx" : groupByCols
                    (sqQueryObj x)
              }
          in
            (finalQip, sffs)

      forM_ queriesPerFormat $ \(qip, sffs) -> do
        -- print qip
        allResults <- getResultsGroupedPerIdx qip
        -- print allResults
        let mergedResults = mergeResults
              (map (\sff -> (sqUniqueIdx sff, sqResults sff)) $ NE.toList sffs)
              allResults
        forM_ mergedResults $ uncurry putSuccess

type PgVersionHasher
  =  [SqlRole]
  -> SchemaSelection
  -> ChecksumAlgo
  -> Maybe ObjName
  -> Maybe ObjName
  -> HashableObject
  -> HashQuery

data QueryInPieces = QueryInPieces
  { selectExprs         :: QueryFrag
  , fromTbl             :: QueryFrag
  , joinClauses         :: QueryFrag
  , nonIdentifyingWhere :: Maybe QueryFrag
  , identifyingWheres   :: Maybe QueryFrag
  , groupByExprs        :: Maybe QueryFrag
  }
  deriving Show

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

queryServerMajorVersion :: MonadIO m => DB.Connection -> m Int
queryServerMajorVersion conn = do
  strVersion :: Text <-
    DB.fromOnly <$> unsafeQuery1 conn "SHOW server_version_num" ()
  case Parsec.parseOnly (Parsec.decimal <* Parsec.endOfInput) strVersion of
    Left _ -> error $ "Non-integral server_version_num: " <> show strVersion
    Right (numVersion :: Int) -> pure $ numVersion `div` 10000

readHashesFromDatabaseWithSettings
  :: (MonadUnliftIO m, MonadIO m, MonadLogger m, HasCallStack)
  => CoddSettings
  -> DB.Connection
  -> m DbHashes
readHashesFromDatabaseWithSettings CoddSettings { migsConnString, schemasToHash, checksumAlgo, extraRolesToHash } conn
  = do
    majorVersion <- queryServerMajorVersion conn
    let rolesToHash =
          (SqlRole . Text.pack . DB.connectUser $ migsConnString)
            : extraRolesToHash
    case majorVersion of
      10 -> readHashesFromDatabase Pg10.hashQueryFor
                                   conn
                                   schemasToHash
                                   rolesToHash
                                   checksumAlgo
      11 -> readHashesFromDatabase Pg11.hashQueryFor
                                   conn
                                   schemasToHash
                                   rolesToHash
                                   checksumAlgo
      12 -> readHashesFromDatabase Pg12.hashQueryFor
                                   conn
                                   schemasToHash
                                   rolesToHash
                                   checksumAlgo
      13 -> readHashesFromDatabase Pg13.hashQueryFor
                                   conn
                                   schemasToHash
                                   rolesToHash
                                   checksumAlgo
      14 -> readHashesFromDatabase Pg14.hashQueryFor
                                   conn
                                   schemasToHash
                                   rolesToHash
                                   checksumAlgo
      v
        | v < 10 -> error
        $  "Unsupported PostgreSQL version "
        ++ show majorVersion
        | otherwise -> do
          logWarnN
            $ "Not all features of PostgreSQL version "
            <> Text.pack (show majorVersion)
            <> " may be supported by codd. Please file an issue for us to support this newer version properly."
          readHashesFromDatabase Pg14.hashQueryFor
                                 conn
                                 schemasToHash
                                 rolesToHash
                                 checksumAlgo

readHashesFromDatabase
  :: (MonadUnliftIO m, MonadIO m, HasCallStack)
  => PgVersionHasher
  -> DB.Connection
  -> SchemaSelection
  -> [SqlRole]
  -> ChecksumAlgo
  -> m DbHashes
readHashesFromDatabase pgVer conn schemaSel allRoles checksumAlgo = do
  let stateStore = stateSet UserState{} stateEmpty
  env0 <- liftIO
    $ initEnv stateStore (pgVer, conn, schemaSel, allRoles, checksumAlgo)
  liftIO $ runHaxl env0 $ do
    allDbSettings <- dataFetch $ GetHashesReq HDatabaseSettings Nothing Nothing
    roles         <- dataFetch $ GetHashesReq HRole Nothing Nothing
    schemas       <- dataFetch $ GetHashesReq HSchema Nothing Nothing
    let
      dbSettings = case allDbSettings of
        [(_, h)] -> h
        _ ->
          error
            "More than one database returned from pg_database. Please file a bug."
    DbHashes dbSettings <$> getSchemaHash schemas <*> pure
      (listToMap $ map (uncurry RoleHash) roles)

getSchemaHash :: [(ObjName, Value)] -> Haxl (Map ObjName SchemaHash)
getSchemaHash schemas =
  fmap Map.fromList $ for schemas $ \(schemaName, schemaHash) -> do
    tables      <- dataFetch $ GetHashesReq HTable (Just schemaName) Nothing
    views       <- dataFetch $ GetHashesReq HView (Just schemaName) Nothing
    routines    <- dataFetch $ GetHashesReq HRoutine (Just schemaName) Nothing
    sequences   <- dataFetch $ GetHashesReq HSequence (Just schemaName) Nothing
    collations  <- dataFetch $ GetHashesReq HCollation (Just schemaName) Nothing
    types       <- dataFetch $ GetHashesReq HType (Just schemaName) Nothing

    tableHashes <- getTablesHashes schemaName tables
    pure
      ( schemaName
      , SchemaHash schemaName
                   schemaHash
                   (listToMap tableHashes)
                   (listToMap $ map (uncurry ViewHash) views)
                   (listToMap $ map (uncurry RoutineHash) routines)
                   (listToMap $ map (uncurry SequenceHash) sequences)
                   (listToMap $ map (uncurry CollationHash) collations)
                   (listToMap $ map (uncurry TypeHash) types)
      )

getTablesHashes :: ObjName -> [(ObjName, Value)] -> Haxl [TableHash]
getTablesHashes schemaName tables = for tables $ \(tblName, tableHash) -> do
  columns <- dataFetch $ GetHashesReq HColumn (Just schemaName) (Just tblName)
  constraints <- dataFetch
    $ GetHashesReq HTableConstraint (Just schemaName) (Just tblName)
  triggers <- dataFetch $ GetHashesReq HTrigger (Just schemaName) (Just tblName)
  policies <- dataFetch $ GetHashesReq HPolicy (Just schemaName) (Just tblName)
  indexes  <- dataFetch $ GetHashesReq HIndex (Just schemaName) (Just tblName)
  pure $ TableHash tblName
                   tableHash
                   (listToMap $ map (uncurry TableColumn) columns)
                   (listToMap $ map (uncurry TableConstraint) constraints)
                   (listToMap $ map (uncurry TableTrigger) triggers)
                   (listToMap $ map (uncurry TablePolicy) policies)
                   (listToMap $ map (uncurry TableIndex) indexes)
