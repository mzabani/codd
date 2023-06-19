module Codd.Representations.Database
  ( queryServerMajorVersion
  , readSchemaFromDatabase
  , readRepresentationsFromDbWithSettings
  , readRepsFromDbWithNewTxn
  ) where

import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Query                     ( InTxn
                                                , beginCommitTxnBracket
                                                , unsafeQuery1
                                                )
import           Codd.Representations.Database.Model
                                                ( DbObjRepresentationQuery(..)
                                                , QueryFrag(..)
                                                , withQueryFrag
                                                )
import qualified Codd.Representations.Database.Pg10
                                               as Pg10
import qualified Codd.Representations.Database.Pg11
                                               as Pg11
import qualified Codd.Representations.Database.Pg12
                                               as Pg12
import qualified Codd.Representations.Database.Pg13
                                               as Pg13
import qualified Codd.Representations.Database.Pg14
                                               as Pg14
import qualified Codd.Representations.Database.Pg15
                                               as Pg15
import           Codd.Representations.Database.SqlGen
                                                ( interspBy
                                                , parens
                                                )
import           Codd.Representations.Types
import           Codd.Types                     ( SchemaAlgo
                                                , SchemaSelection
                                                , SqlRole(..)
                                                )

import           Control.Monad                  ( forM_
                                                , void
                                                )
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

-- brittany-disable-next-binding
data RepresentationReq a where
  GetRepsReq :: ObjectRep -> Maybe ObjName -> Maybe ObjName -> RepresentationReq [(ObjName, Value)]
  deriving stock (Typeable)

instance Eq (RepresentationReq a) where
  GetRepsReq hobj1 sn1 tn1 == GetRepsReq sqHobj sn2 tn2 =
    (hobj1, sn1, tn1) == (sqHobj, sn2, tn2)

instance Hashable (RepresentationReq a) where
  hashWithSalt s (GetRepsReq hobj sn tn) = hashWithSalt s (hobj, sn, tn)


instance Show (RepresentationReq a) where
  show (GetRepsReq hobj sn tn) = "GetRepsReq: " ++ show (hobj, sn, tn)

instance ShowP RepresentationReq where
  showp = show


instance StateKey RepresentationReq where
  data State RepresentationReq = UserState {}


instance DataSourceName RepresentationReq where
  dataSourceName _ = "CatalogHashSource"

type HaxlEnv
  = (PgVersionHasher, DB.Connection, SchemaSelection, [SqlRole], SchemaAlgo)
type Haxl = GenHaxl HaxlEnv ()

data SameQueryFormatFetch = SameQueryFormatFetch
  { sqUniqueIdx :: Int
  , sqHobj      :: ObjectRep
  , sqQueryObj  :: DbObjRepresentationQuery
  , sqResults   :: ResultVar [(ObjName, Value)]
  }

instance DataSource HaxlEnv RepresentationReq where
  fetch _ _ (objRepQueryFor, conn, allSchemas, allRoles, schemaAlgoOpts) =
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
    -- ObjectRep, because they have the same SELECT, FROM and JOINs. Then, prepend an increasing number to the select exprs
    -- which is 1 if the first `identWhere` expression is True, 2 when the second is True, 3 for the third and so on..
    -- This requires mutually exclusive where conditions (`identWhere` satisfies that) and batches queries for objects
    -- of the same kind (e.g. all tables or all views or all triggers etc.) into a single query.
    combineQueriesWithWhere blockedFetches = do
      let
        allRepReqs :: [SameQueryFormatFetch]
        allRepReqs = zipWith
          (\i (a, c, d) -> SameQueryFormatFetch i a c d)
          [1 ..]
          [ ( hobj
            , objRepQueryFor allRoles
                             allSchemas
                             schemaAlgoOpts
                             schemaName
                             tblName
                             hobj
            , r
            )
          | BlockedFetch (GetRepsReq hobj schemaName tblName) r <-
            blockedFetches
          ]

        fetchesPerQueryFormat :: [NonEmpty SameQueryFormatFetch]
        fetchesPerQueryFormat = NE.groupAllWith sqHobj allRepReqs

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
                $ repCols (sqQueryObj x)
            finalQip = QueryInPieces
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
                                      <> jsonObject
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
  -> SchemaAlgo
  -> Maybe ObjName
  -> Maybe ObjName
  -> ObjectRep
  -> DbObjRepresentationQuery

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

-- | Like `readRepresentationsFromDbWithSettings` but starts a new transaction. Must not
-- be called if already inside a transaction.
-- Work to make such a requirement a class constraint will come in the future.
readRepsFromDbWithNewTxn
  :: (MonadUnliftIO m, MonadIO m, MonadLogger m, HasCallStack)
  => CoddSettings
  -> DB.Connection
  -> m DbRep
readRepsFromDbWithNewTxn sett@CoddSettings { txnIsolationLvl } conn =
  beginCommitTxnBracket txnIsolationLvl conn
    $ readRepresentationsFromDbWithSettings sett conn

-- | This function _must_ be called inside a transaction to behave correctly.
readRepresentationsFromDbWithSettings
  :: (MonadUnliftIO m, MonadIO m, MonadLogger m, InTxn m, HasCallStack)
  => CoddSettings
  -> DB.Connection
  -> m DbRep
readRepresentationsFromDbWithSettings CoddSettings { migsConnString, namespacesToCheck, schemaAlgoOpts, extraRolesToCheck } conn
  = do
    majorVersion <- queryServerMajorVersion conn
    let rolesToCheck =
          (SqlRole . Text.pack . DB.connectUser $ migsConnString)
            : extraRolesToCheck
    case majorVersion of
      10 -> readSchemaFromDatabase Pg10.objRepQueryFor
                                   conn
                                   namespacesToCheck
                                   rolesToCheck
                                   schemaAlgoOpts
      11 -> readSchemaFromDatabase Pg11.objRepQueryFor
                                   conn
                                   namespacesToCheck
                                   rolesToCheck
                                   schemaAlgoOpts
      12 -> readSchemaFromDatabase Pg12.objRepQueryFor
                                   conn
                                   namespacesToCheck
                                   rolesToCheck
                                   schemaAlgoOpts
      13 -> readSchemaFromDatabase Pg13.objRepQueryFor
                                   conn
                                   namespacesToCheck
                                   rolesToCheck
                                   schemaAlgoOpts
      14 -> readSchemaFromDatabase Pg14.objRepQueryFor
                                   conn
                                   namespacesToCheck
                                   rolesToCheck
                                   schemaAlgoOpts
      15 -> readSchemaFromDatabase Pg15.objRepQueryFor
                                   conn
                                   namespacesToCheck
                                   rolesToCheck
                                   schemaAlgoOpts
      v
        | v < 10 -> error
        $  "Unsupported PostgreSQL version "
        ++ show majorVersion
        | otherwise -> do
          logWarnN
            $ "Not all features of PostgreSQL version "
            <> Text.pack (show majorVersion)
            <> " may be supported by codd. Please file an issue for us to support this newer version properly."
          readSchemaFromDatabase Pg15.objRepQueryFor
                                 conn
                                 namespacesToCheck
                                 rolesToCheck
                                 schemaAlgoOpts

-- | This function _must_ be called inside a transaction to behave correctly.
-- Work to make such a requirement a class constraint will come in the future.
readSchemaFromDatabase
  :: (MonadUnliftIO m, MonadIO m, HasCallStack)
  => PgVersionHasher
  -> DB.Connection
  -> SchemaSelection
  -> [SqlRole]
  -> SchemaAlgo
  -> m DbRep
readSchemaFromDatabase pgVer conn schemaSel allRoles schemaAlgoOpts = do
  let stateStore = stateSet UserState{} stateEmpty
  liftIO $ do
    env0 <- initEnv stateStore
                    (pgVer, conn, schemaSel, allRoles, schemaAlgoOpts)
    -- We want full schemas to appear in the expression that defines objects,
    -- from constraints to default column values and everything else.
    -- The pg_get_expr and pg_get_constraintdef functions (and likely others) will take the current
    -- value of `search_path` into account when showing pretty expressions, but
    -- we want them to have fully qualified schema names and be independent of the user's
    -- `search_path` setting.
    -- We also want to undo the effects of setting search_path before returning (unless this function
    -- throws an exception).
    withEmptySearchPath $ runHaxl env0 $ do
      allDbSettings <- dataFetch $ GetRepsReq HDatabaseSettings Nothing Nothing
      roles         <- dataFetch $ GetRepsReq HRole Nothing Nothing
      schemas       <- dataFetch $ GetRepsReq HSchema Nothing Nothing
      let
        dbSettings = case allDbSettings of
          [(_, h)] -> h
          _ ->
            error
              "More than one database returned from pg_database. Please file a bug."
      DbRep dbSettings <$> getNamespacesReps schemas <*> pure
        (listToMap $ map (uncurry RoleRep) roles)

 where
  withEmptySearchPath f = do
    [currentSearchPath :: DB.Only Text] <- DB.query_
      conn
      "SELECT current_setting('search_path', false)"
    void $ DB.query @(DB.Only Text) @(DB.Only Text)
      conn
      "SELECT set_config('search_path', ?, true)"
      (DB.Only "")
    res <- f
    -- Undo search_path change
    void $ DB.query @(DB.Only Text) @(DB.Only Text)
      conn
      "SELECT set_config('search_path', ?, true)"
      currentSearchPath
    pure res

getNamespacesReps :: [(ObjName, Value)] -> Haxl (Map ObjName SchemaRep)
getNamespacesReps schemas =
  fmap Map.fromList $ for schemas $ \(schemaName, namespaceRep) -> do
    tables     <- dataFetch $ GetRepsReq HTable (Just schemaName) Nothing
    views      <- dataFetch $ GetRepsReq HView (Just schemaName) Nothing
    routines   <- dataFetch $ GetRepsReq HRoutine (Just schemaName) Nothing
    sequences  <- dataFetch $ GetRepsReq HSequence (Just schemaName) Nothing
    collations <- dataFetch $ GetRepsReq HCollation (Just schemaName) Nothing
    types      <- dataFetch $ GetRepsReq HType (Just schemaName) Nothing

    tableReps  <- getTablesReps schemaName tables
    pure
      ( schemaName
      , SchemaRep schemaName
                  namespaceRep
                  (listToMap tableReps)
                  (listToMap $ map (uncurry ViewRep) views)
                  (listToMap $ map (uncurry RoutineRep) routines)
                  (listToMap $ map (uncurry SequenceRep) sequences)
                  (listToMap $ map (uncurry CollationRep) collations)
                  (listToMap $ map (uncurry TypeRep) types)
      )

getTablesReps :: ObjName -> [(ObjName, Value)] -> Haxl [TableRep]
getTablesReps schemaName tables = for tables $ \(tblName, tableRep) -> do
  columns     <- dataFetch $ GetRepsReq HColumn (Just schemaName) (Just tblName)
  constraints <- dataFetch
    $ GetRepsReq HTableConstraint (Just schemaName) (Just tblName)
  triggers <- dataFetch $ GetRepsReq HTrigger (Just schemaName) (Just tblName)
  policies <- dataFetch $ GetRepsReq HPolicy (Just schemaName) (Just tblName)
  indexes  <- dataFetch $ GetRepsReq HIndex (Just schemaName) (Just tblName)
  pure $ TableRep tblName
                  tableRep
                  (listToMap $ map (uncurry TableColumnRep) columns)
                  (listToMap $ map (uncurry TableConstraintRep) constraints)
                  (listToMap $ map (uncurry TableTriggerRep) triggers)
                  (listToMap $ map (uncurry TablePolicyRep) policies)
                  (listToMap $ map (uncurry TableIndexRep) indexes)
