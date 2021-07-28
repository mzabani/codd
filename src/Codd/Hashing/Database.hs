
module Codd.Hashing.Database where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing.Database.Model    ( HashQuery(..)
                                                , QueryFrag(..)
                                                , withQueryFrag
                                                )
import qualified Codd.Hashing.Database.Pg10    as Pg10
import qualified Codd.Hashing.Database.Pg11    as Pg11
import qualified Codd.Hashing.Database.Pg12    as Pg12
import           Codd.Hashing.Database.SqlGen   ( interspBy
                                                , parens
                                                , safeStringConcat
                                                )
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
import           GHC.Stack                      ( HasCallStack )
import           Haxl.Core
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
                                                )

data HashReq a where
  GetHashesReq ::HashableObject -> Maybe ObjName -> Maybe ObjName -> HashReq [(ObjName, ObjHash)]
  deriving stock (Typeable)

instance Eq (HashReq a) where
  GetHashesReq hobj1 sn1 tn1 == GetHashesReq hobj2 sn2 tn2 =
    (hobj1, sn1, tn1) == (hobj2, sn2, tn2)

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

type HaxlEnv = (PgVersion, DB.Connection, Include SqlSchema, Include SqlRole)
type Haxl = GenHaxl HaxlEnv ()

data SameQueryFormatFetch = SameQueryFormatFetch
  { uniqueIdx2 :: Int
  , hobj2      :: HashableObject
  , ids2       :: (Maybe ObjName, Maybe ObjName)
  , qp2        :: HashQuery
  , rvar2      :: ResultVar [(ObjName, ObjHash)]
  }

instance DataSource HaxlEnv HashReq where
  fetch _ _ (hashQueryFor, conn, allSchemas, allRoles) = SyncFetch
    combineQueriesWithWhere
   where
    fst3 (a, _, _) = a

    getResultsGroupedPerIdx
      :: QueryInPieces -> IO [NE.NonEmpty (Int, ObjName, ObjHash)]
    getResultsGroupedPerIdx qip =
      let qf = queryInPiecesToQueryFrag qip <> "\n ORDER BY artificial_idx"
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
            , (schemaName, tblName)
            , hashQueryFor allRoles allSchemas schemaName tblName hobj
            , r
            )
          | BlockedFetch (GetHashesReq hobj schemaName tblName) r <-
            blockedFetches
          ]

        fetchesPerQueryFormat :: [NonEmpty SameQueryFormatFetch]
        fetchesPerQueryFormat = NE.groupAllWith hobj2 allHashReqs

        queriesPerFormat :: [(QueryInPieces, NonEmpty SameQueryFormatFetch)]
        queriesPerFormat = flip map fetchesPerQueryFormat $ \sffs@(x :| _) ->
          let
            -- This form of batching only works if the WHERE expressions of each query are mutually exclusive!
            finalHashExpr =
              "MD5(" <> safeStringConcat (checksumCols (qp2 x)) <> ")"
            -- TODO: Create a "Debug" mode that allows us to see what gets hashed instead of hashes
            -- finalHashExpr = safeStringConcat (checksumCols (qp2 x))
            finalQip = QueryInPieces
              { selectExprs         = "CASE "
                                      <> foldMap
                                           (\qip ->
                                             "\n WHEN "
                                               <> fromMaybe "TRUE"
                                                            (identWhere (qp2 qip))
                                               <> QueryFrag " THEN ?"
                                                            (DB.Only (uniqueIdx2 qip))
                                           )
                                           sffs
                                      <> " END AS artificial_idx, "
                                      <> objNameCol (qp2 x)
                                      <> ", "
                                      <> finalHashExpr
              , fromTbl             = fromTable (qp2 x)
              , joinClauses         = joins (qp2 x)
              , nonIdentifyingWhere = nonIdentWhere (qp2 x)
              , identifyingWheres   = case
                                        mapMaybe (identWhere . qp2)
                                          $ NE.toList sffs
                                      of
                                        [] -> Nothing
                                        fs -> Just $ interspBy True " OR " fs
              , groupByExprs        = if null (groupByCols (qp2 x))
                then Nothing
                else
                  Just $ interspBy False ", " $ "artificial_idx" : groupByCols
                    (qp2 x)
              }
          in
            (finalQip, sffs)

      forM_ queriesPerFormat $ \(qip, sffs) -> do
        -- print qip
        allResults <- getResultsGroupedPerIdx qip
        -- print allResults
        let mergedResults = mergeResults
              (map (\sff -> (uniqueIdx2 sff, rvar2 sff)) $ NE.toList sffs)
              allResults
        forM_ mergedResults $ uncurry putSuccess

type PgVersion
  =  Include SqlRole
  -> Include SqlSchema
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
          10 -> readHashesFromDatabase Pg10.hashQueryFor
                                       conn
                                       schemasToHash
                                       rolesToHash
          11 -> readHashesFromDatabase Pg11.hashQueryFor
                                       conn
                                       schemasToHash
                                       rolesToHash
          12 -> readHashesFromDatabase Pg12.hashQueryFor
                                       conn
                                       schemasToHash
                                       rolesToHash
          -- Postgres 13 doesn't seem to have any hashable new features compared to 12
          13 -> readHashesFromDatabase Pg12.hashQueryFor
                                       conn
                                       schemasToHash
                                       rolesToHash
          v
            | v <= 13 -> error
            $  "Unsupported PostgreSQL version "
            ++ show majorVersion
            | otherwise -> do
              logWarnN
                $ "Not all features of PostgreSQL version "
                <> Text.pack (show majorVersion)
                <> " may be supported by codd. Please file an issue for us to support this newer version properly."
              readHashesFromDatabase Pg12.hashQueryFor
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
  env0 <- liftIO $ initEnv stateStore (pgVer, conn, allSchemas, allRoles)
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

getSchemaHash :: [(ObjName, ObjHash)] -> Haxl (Map ObjName SchemaHash)
getSchemaHash schemas =
  fmap Map.fromList $ for schemas $ \(schemaName, schemaHash) -> do
    tables      <- dataFetch $ GetHashesReq HTable (Just schemaName) Nothing
    views       <- dataFetch $ GetHashesReq HView (Just schemaName) Nothing
    routines    <- dataFetch $ GetHashesReq HRoutine (Just schemaName) Nothing
    sequences   <- dataFetch $ GetHashesReq HSequence (Just schemaName) Nothing

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
