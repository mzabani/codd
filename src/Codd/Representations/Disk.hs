module Codd.Representations.Disk
  ( persistRepsToDisk,
    readRepsFromDisk,
    toFiles,
  )
where

import Codd.Representations.Types
import Codd.Types (PgMajorVersion (..))
import Control.DeepSeq (force)
import Control.Monad
  ( forM_,
    join,
    when,
  )
import Data.Aeson
  ( Value,
    decode,
  )
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.IO.Exception (IOErrorType (..), ioe_type)
import GHC.Stack (HasCallStack)
import System.FilePath
  ( takeDirectory,
    takeFileName,
    (</>),
  )
import System.IO.Error (isDoesNotExistError)
import UnliftIO
  ( MonadIO (..),
    MonadUnliftIO,
    evaluate,
    handle,
    pooledMapConcurrentlyN_,
    throwIO,
    tryJust,
  )
import UnliftIO.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    pathIsSymbolicLink,
    removePathForcibly,
    renameDirectory,
  )
import Prelude hiding
  ( readFile,
    writeFile,
  )

simpleDbDiskObj ::
  ObjName -> Value -> (FilePath, Value)
simpleDbDiskObj oname orep = (mkPathFrag oname, orep)

apprDbRep :: DbRep -> [(FilePath, Value)]
apprDbRep (DbRep dbSettingsRep schemas roles) =
  let x = ("db-settings", dbSettingsRep)
      rs = prependDir "roles" $ flip map (Map.elems roles) $ \(RoleRep n v) -> simpleDbDiskObj n v
      ss = prependDir "schemas" $ mconcat $ flip map (Map.toList schemas) $ \(schemaName, schema) ->
        prependDir (mkPathFrag schemaName) (apprSchemaRep schema)
   in x : rs ++ ss

apprSchemaRep :: SchemaRep -> [(FilePath, Value)]
apprSchemaRep (SchemaRep _schemaName namespaceRep tables views routines seqs colls types) =
  let x = ("objrep", namespaceRep)
      tbls = prependDir "tables" $ mconcat $ flip map (Map.toList tables) $ \(tableName, table) ->
        prependDir (mkPathFrag tableName) (apprTableRep table)
      vs = prependDir "views" $ map (\(ViewRep n v) -> simpleDbDiskObj n v) $ Map.elems views
      rs = prependDir "routines" $ map (\(RoutineRep n v) -> simpleDbDiskObj n v) (Map.elems routines)
      ss = prependDir "sequences" $ map (\(SequenceRep n v) -> simpleDbDiskObj n v) (Map.elems seqs)
      cs = prependDir "collations" $ map (\(CollationRep n v) -> simpleDbDiskObj n v) (Map.elems colls)
      ts = prependDir "types" $ map (\(TypeRep n v) -> simpleDbDiskObj n v) (Map.elems types)
   in x : tbls ++ vs ++ rs ++ ss ++ cs ++ ts

apprTableRep :: TableRep -> [(FilePath, Value)]
apprTableRep (TableRep _tblName tblRep columns constraints triggers policies indexes statistics) =
  let x = ("objrep", tblRep)
      cols = prependDir "cols" $ flip map (Map.elems columns) $ \(TableColumnRep colName colRep) -> simpleDbDiskObj colName colRep
      constrs = prependDir "constraints" $ flip map (Map.elems constraints) $ \(TableConstraintRep n v) -> simpleDbDiskObj n v
      trgrs = prependDir "triggers" $ flip map (Map.elems triggers) $ \(TableTriggerRep n v) -> simpleDbDiskObj n v
      pols = prependDir "policies" $ flip map (Map.elems policies) $ \(TablePolicyRep n v) -> simpleDbDiskObj n v
      idxs = prependDir "indexes" $ flip map (Map.elems indexes) $ \(TableIndexRep n v) -> simpleDbDiskObj n v
      stats = prependDir "statistics" $ flip map (Map.elems statistics) $ \(TableStatisticsRep n v) -> simpleDbDiskObj n v
   in x : cols ++ constrs ++ trgrs ++ pols ++ idxs ++ stats

prependDir :: FilePath -> [(FilePath, Value)] -> [(FilePath, Value)]
prependDir dir = map (first (dir </>))

toFiles :: DbRep -> [(FilePath, Value)]
toFiles = sortOn fst . apprDbRep

-- | Wipes out completely the supplied folder and writes the representations of the Database's structures to it again.
persistRepsToDisk :: forall m. (HasCallStack, MonadUnliftIO m) => PgMajorVersion -> DbRep -> FilePath -> m ()
persistRepsToDisk pgVersion dbSchema schemaDirBeforeVersions =
  maybeAtomicallyReplaceSchemaFolder $ \tempDir -> do
    liftIO $ writeRec tempDir dbSchema
  where
    schemaDir = schemaDirBeforeVersions </> show pgVersion
    -- \| Allows the caller to wipe and replace a folder with new contents as atomically
    -- as the user's permissions will let us. That is, the supplied callback should write
    -- to the path it's supplied with as if it were the target folder, and after this finishes
    -- the target folder will be replaced with the contents written.
    -- This does not guarantee it won't leave the target folder in some intermediate state,
    -- it just makes an effort to avoid that.
    -- The entire thing may happen more or less atomically, and no promise is made regarding
    -- file modification times. This function is not thread-safe.
    maybeAtomicallyReplaceSchemaFolder :: (FilePath -> m ()) -> m ()
    maybeAtomicallyReplaceSchemaFolder f = do
      -- We want this function to be fast and as atomic as it can be, but:
      -- 1. `renameFile` throws if the target path is in a different partition from the source path
      -- 2. The user might not have permissions to delete the target folder but does have permissions to modify its contents
      -- 3. Different operating systems can have different behaviours, like Windows, where renameDirectory fails if the target directory exists: https://hackage.haskell.org/package/directory-1.3.9.0/docs/System-Directory.html#v:renameDirectory
      -- 4. Windows and I think even Linux can have ACLs that have more than just a binary "can-write" privilege, with "can-delete" being
      --   separated from "can-create", IIRC.
      errBestScenario <- tryJust (\(e :: IOError) -> if ioe_type e `elem` [NoSuchThing, UnsatisfiedConstraints, PermissionDenied, IllegalOperation, UnsupportedOperation] then Just () else Nothing) $ do
        let nonCanonTempDir = schemaDir </> "../.temp-codd-dir-you-can-remove"
        -- We don't try to be too smart if the schema dir is a symlink so we preserve that symlink
        isLink <- pathIsSymbolicLink schemaDir
        if isLink
          then
            pure $ Left ()
          else do
            whenM (doesDirectoryExist nonCanonTempDir) $ removePathForcibly nonCanonTempDir
            createDirectoryIfMissing True nonCanonTempDir
            canonTempDir <- canonicalizePath nonCanonTempDir
            -- Non-canonicalized paths make `renameDirectory` fail for some reason, and canonicalization
            -- only works with existing folders
            whenM (doesDirectoryExist schemaDir) $ removePathForcibly schemaDir
            renameDirectory canonTempDir schemaDir
            removePathForcibly schemaDir
            createDirectoryIfMissing True canonTempDir
            pure $ Right canonTempDir
      case join errBestScenario of
        Right canonTempDir -> do
          f canonTempDir
          renameDirectory canonTempDir schemaDir
        Left _ -> do
          ensureEmptyDir schemaDir
          f schemaDir

    -- \| Removes every file and subfolder of the supplied path to
    -- leave it empty, keeping the empty folder.
    ensureEmptyDir :: FilePath -> m ()
    ensureEmptyDir path = do
      exists <- doesDirectoryExist path
      if exists
        then do
          entries <- listDirectory path
          forM_ entries $ \x -> removePathForcibly (path </> x)
        else createDirectoryIfMissing True path

    writeRec :: FilePath -> DbRep -> IO ()
    writeRec dir obj = do
      createDirectoryIfMissing True dir
      -- Limit number of open file descriptors even if in the future we
      -- increase codd's capabilities beyond the current 2
      pooledMapConcurrentlyN_ 2 (writeFolder dir) $ NE.groupBy (\(f1, _) (f2, _) -> takeDirectory f1 == takeDirectory f2) (sortOn fst $ toFiles obj)
    writeFolder :: FilePath -> NE.NonEmpty (FilePath, Value) -> IO ()
    writeFolder dir filesPerFolder = do
      let relFolderToCreate = takeDirectory $ fst $ NE.head filesPerFolder
      createDirectoryIfMissing True (dir </> relFolderToCreate)
      forM_ filesPerFolder (\(fn, jsonRep) -> LBS.writeFile (dir </> fn) (detEncodeJSONByteString jsonRep))

readNamespaceRep :: (MonadUnliftIO m) => FilePath -> m SchemaRep
readNamespaceRep dir =
  SchemaRep (readObjName dir)
    <$> readFileRep (dir </> "objrep")
    <*> readMultiple (dir </> "tables") readTable
    <*> readMultiple (dir </> "views") readView
    <*> readMultiple (dir </> "routines") readRoutine
    <*> readMultiple (dir </> "sequences") readSequence
    <*> readMultiple (dir </> "collations") readCollation
    <*> readMultiple (dir </> "types") readType

readTable :: (MonadUnliftIO m) => FilePath -> m TableRep
readView :: (MonadUnliftIO m) => FilePath -> m ViewRep
readRoutine :: (MonadUnliftIO m) => FilePath -> m RoutineRep
readSequence :: (MonadUnliftIO m) => FilePath -> m SequenceRep
readCollation :: (MonadUnliftIO m) => FilePath -> m CollationRep
readType :: (MonadUnliftIO m) => FilePath -> m TypeRep
readTable dir =
  TableRep (readObjName dir)
    <$> readFileRep (dir </> "objrep")
    <*> readMultiple (dir </> "cols") (simpleObjRepFileRead TableColumnRep)
    <*> readMultiple
      (dir </> "constraints")
      (simpleObjRepFileRead TableConstraintRep)
    <*> readMultiple
      (dir </> "triggers")
      (simpleObjRepFileRead TableTriggerRep)
    <*> readMultiple
      (dir </> "policies")
      (simpleObjRepFileRead TablePolicyRep)
    <*> readMultiple
      (dir </> "indexes")
      (simpleObjRepFileRead TableIndexRep)
    <*> readMultiple
      (dir </> "statistics")
      (simpleObjRepFileRead TableStatisticsRep)

readView = simpleObjRepFileRead ViewRep

readRoutine = simpleObjRepFileRead RoutineRep

readSequence = simpleObjRepFileRead SequenceRep

readCollation = simpleObjRepFileRead CollationRep

readType = simpleObjRepFileRead TypeRep

readObjName :: FilePath -> ObjName
readObjName = fromPathFrag . takeFileName

readFileRep ::
  forall m. (MonadUnliftIO m) => FilePath -> m Value
readFileRep filepath = rethrowIfNotExists $ do
  -- Careful, LBS.readFile is lazy and does not close
  -- the file handle unless we force the thunk, and not closing
  -- file handles can make shells with low ulimits barf.
  -- MacOS has particularly low ulimits.
  !fileContents <- liftIO $ LBS.readFile filepath
  !decodedJson <-
    evaluate
      $ fromMaybe
        ( error $
            "File '"
              ++ filepath
              ++ "' was supposed to contain a JSON value"
        )
      $ force
      $ decode
        fileContents
  pure decodedJson
  where
    rethrowIfNotExists =
      handle
        ( \(e :: IOError) ->
            if isDoesNotExistError e
              then
                throwIO $
                  userError $
                    "File "
                      <> filepath
                      <> " was expected but does not exist"
              else throwIO e
        )

simpleObjRepFileRead ::
  (MonadUnliftIO m) =>
  (ObjName -> Value -> a) ->
  FilePath ->
  m a
simpleObjRepFileRead f filepath =
  f (readObjName filepath) <$> readFileRep filepath

readMultiple ::
  (MonadUnliftIO m, HasName o) =>
  FilePath ->
  (FilePath -> m o) ->
  m (Map ObjName o)
readMultiple dir f = do
  foldersOrNotOne <- handle checkDoesNotExist $ Right . filter (/= "objrep") <$> listDirectory dir
  case foldersOrNotOne of
    Right folders -> do
      objList <- traverse (f . (dir </>)) folders
      return $ listToMap objList
    Left () -> pure Map.empty
  where
    checkDoesNotExist (e :: IOError) = if isDoesNotExistError e then pure (Left ()) else throwIO e

readRepsFromDisk ::
  (MonadUnliftIO m) =>
  PgMajorVersion ->
  -- | The path of CODD_EXPECTED_SCHEMA_DIR, without any major version numbers
  FilePath ->
  m DbRep
readRepsFromDisk pgVersion schemaDir =
  let dir = schemaDir </> show pgVersion
   in DbRep
        <$> readFileRep (dir </> "db-settings")
        <*> readMultiple (dir </> "schemas") readNamespaceRep
        <*> readMultiple (dir </> "roles") (simpleObjRepFileRead RoleRep)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM s r = s >>= flip when r
