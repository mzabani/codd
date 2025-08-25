{-# LANGUAGE QuasiQuotes #-}

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
  ( forM,
    forM_,
    join,
    when,
  )
import Control.Monad.Identity (runIdentity)
import Data.Aeson
  ( Value,
    decode,
  )
import Data.Bifunctor (first)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.IO.Exception (IOErrorType (..), ioe_type)
import GHC.Stack (HasCallStack)
import qualified System.Directory.OsPath as OS
import qualified System.File.OsPath as OS
import System.FilePath
  ( (</>),
  )
import System.IO.Error (isDoesNotExistError)
import System.OsPath (OsPath, osp)
import qualified System.OsPath as OS
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

{-
This module contains functions and data models to write and read schema representations
from disk. It has been rewritten a few times in attempt to find a model
that is sufficiently clean and robust.

The unsolved challenge at this point is to unify both reading and writing
under a unique model that represents how files and directories are laid out
on disk, but it doesn't seem worth the trouble, honestly.
-}

-- | This class is equivalent to some form of monadic unfolding, mixed
-- with a mockable "writeFile" function. It allows us to derive both
-- `toFiles` and `persistRepsToDisk`, for example, but isn't a very elegant
-- model otherwise.
class DbDiskObj a where
  appr ::
    (Monad m) =>
    a ->
    -- | When recursing into a new sub-structure, this function
    -- will be called with a relative folder where that substructure's
    -- root belongs to.
    (forall b. (DbDiskObj b) => OsPath -> b -> m d) ->
    -- | This function will be called when writing to a file. A relative path
    -- to this structure's root will be passed.
    (OsPath -> Value -> m d) ->
    m [d]

simpleDbDiskObj ::
  (Functor m) => ObjName -> Value -> (OsPath -> Value -> m d) -> m [d]
simpleDbDiskObj oname orep ffile = (: []) <$> ffile (mkPathFrag oname) orep

instance DbDiskObj DbRep where
  appr (DbRep dbSettingsRep schemas roles) frec ffile = do
    x <- ffile [osp|db-settings|] dbSettingsRep
    rs <- forM (Map.elems roles) $ frec [osp|roles|]
    ss <- forM (Map.toList schemas) $ \(schemaName, schema) ->
      frec ([osp|schemas|] OS.</> mkPathFrag schemaName) schema
    pure $ x : rs ++ ss

instance DbDiskObj RoleRep where
  appr (RoleRep roleName roleRep) _ = simpleDbDiskObj roleName roleRep

instance DbDiskObj SchemaRep where
  appr (SchemaRep _schemaName namespaceRep tables views routines seqs colls types) frec ffile =
    do
      x <- ffile [osp|objrep|] namespaceRep
      tbls <- forM (Map.toList tables) $ \(tableName, table) ->
        frec ([osp|tables|] OS.</> mkPathFrag tableName) table
      vs <- forM (Map.elems views) $ frec [osp|views|]
      rs <- forM (Map.elems routines) $ frec [osp|routines|]
      ss <- forM (Map.elems seqs) $ frec [osp|sequences|]
      cs <- forM (Map.elems colls) $ frec [osp|collations|]
      ts <- forM (Map.elems types) $ frec [osp|types|]
      pure $ x : tbls ++ vs ++ rs ++ ss ++ cs ++ ts

instance DbDiskObj TableRep where
  appr (TableRep _tblName tblRep columns constraints triggers policies indexes statistics) frec ffile =
    do
      let mkpath p = p
      x <- ffile (mkpath [osp|objrep|]) tblRep
      cols <- forM (Map.elems columns) $ frec (mkpath [osp|cols|])
      constrs <-
        forM (Map.elems constraints) $
          frec (mkpath [osp|constraints|])
      trgrs <- forM (Map.elems triggers) $ frec (mkpath [osp|triggers|])
      pols <- forM (Map.elems policies) $ frec (mkpath [osp|policies|])
      idxs <- forM (Map.elems indexes) $ frec (mkpath [osp|indexes|])
      stats <- forM (Map.elems statistics) $ frec (mkpath [osp|statistics|])
      pure $ x : cols ++ constrs ++ trgrs ++ pols ++ idxs ++ stats

instance DbDiskObj TableColumnRep where
  appr (TableColumnRep colName colRep) _ = simpleDbDiskObj colName colRep

instance DbDiskObj TableConstraintRep where
  appr (TableConstraintRep constrName constrRep) _ =
    simpleDbDiskObj constrName constrRep

instance DbDiskObj TableTriggerRep where
  appr (TableTriggerRep triggerName triggerRep) _ =
    simpleDbDiskObj triggerName triggerRep

instance DbDiskObj TablePolicyRep where
  appr (TablePolicyRep polName polRep) _ = simpleDbDiskObj polName polRep

instance DbDiskObj TableStatisticsRep where
  appr (TableStatisticsRep stxName stxRep) _ = simpleDbDiskObj stxName stxRep

instance DbDiskObj TableIndexRep where
  appr (TableIndexRep idxName indexRep) _ = simpleDbDiskObj idxName indexRep

instance DbDiskObj ViewRep where
  appr (ViewRep viewName viewRep) _ = simpleDbDiskObj viewName viewRep

instance DbDiskObj RoutineRep where
  appr (RoutineRep routineName routineRep) _ =
    simpleDbDiskObj routineName routineRep

instance DbDiskObj SequenceRep where
  appr (SequenceRep seqName seqRep) _ = simpleDbDiskObj seqName seqRep

instance DbDiskObj CollationRep where
  appr (CollationRep collName collationRep) _ =
    simpleDbDiskObj collName collationRep

instance DbDiskObj TypeRep where
  appr (TypeRep typeName typeRep) _ = simpleDbDiskObj typeName typeRep

toFiles :: DbRep -> [(OsPath, Value)]
toFiles = sortOn fst . frec
  where
    frec :: (DbDiskObj a) => a -> [(OsPath, Value)]
    frec sobj =
      concat $
        runIdentity $
          appr
            sobj
            (\parentDir obj -> pure $ prependDir parentDir $ frec obj)
            (\fn h -> pure [(fn, h)])
    prependDir dir = map (first (dir OS.</>))

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
    writeRec dir' obj = do
      createDirectoryIfMissing True dir'
      dir <- OS.encodeFS dir'
      -- Limit number of open file descriptors even if in the future we
      -- increase codd's capabilities beyond the current 2
      pooledMapConcurrentlyN_ 2 (writeFolder dir) $ NE.groupBy (\(f1, _) (f2, _) -> OS.takeDirectory f1 == OS.takeDirectory f2) (sortOn fst $ toFiles obj)
    writeFolder :: OsPath -> NE.NonEmpty (OsPath, Value) -> IO ()
    writeFolder dir filesPerFolder = do
      let relFolderToCreate = OS.takeDirectory $ fst $ NE.head filesPerFolder
      OS.createDirectoryIfMissing True (dir OS.</> relFolderToCreate)
      forM_ filesPerFolder (\(fn, jsonRep) -> OS.writeFile (dir OS.</> fn) (detEncodeJSONByteString jsonRep))

readNamespaceRep :: (MonadUnliftIO m) => OsPath -> m SchemaRep
readNamespaceRep dir =
  SchemaRep (readObjName dir)
    <$> readFileRep (dir OS.</> [osp|objrep|])
    <*> readMultiple (dir OS.</> [osp|tables|]) readTable
    <*> readMultiple (dir OS.</> [osp|views|]) readView
    <*> readMultiple (dir OS.</> [osp|routines|]) readRoutine
    <*> readMultiple (dir OS.</> [osp|sequences|]) readSequence
    <*> readMultiple (dir OS.</> [osp|collations|]) readCollation
    <*> readMultiple (dir OS.</> [osp|types|]) readType

readTable :: (MonadUnliftIO m) => OsPath -> m TableRep
readView :: (MonadUnliftIO m) => OsPath -> m ViewRep
readRoutine :: (MonadUnliftIO m) => OsPath -> m RoutineRep
readSequence :: (MonadUnliftIO m) => OsPath -> m SequenceRep
readCollation :: (MonadUnliftIO m) => OsPath -> m CollationRep
readType :: (MonadUnliftIO m) => OsPath -> m TypeRep
readTable dir =
  TableRep (readObjName dir)
    <$> readFileRep (dir OS.</> [osp|objrep|])
    <*> readMultiple (dir OS.</> [osp|cols|]) (simpleObjRepFileRead TableColumnRep)
    <*> readMultiple
      (dir OS.</> [osp|constraints|])
      (simpleObjRepFileRead TableConstraintRep)
    <*> readMultiple
      (dir OS.</> [osp|triggers|])
      (simpleObjRepFileRead TableTriggerRep)
    <*> readMultiple
      (dir OS.</> [osp|policies|])
      (simpleObjRepFileRead TablePolicyRep)
    <*> readMultiple
      (dir OS.</> [osp|indexes|])
      (simpleObjRepFileRead TableIndexRep)
    <*> readMultiple
      (dir OS.</> [osp|statistics|])
      (simpleObjRepFileRead TableStatisticsRep)

readView = simpleObjRepFileRead ViewRep

readRoutine = simpleObjRepFileRead RoutineRep

readSequence = simpleObjRepFileRead SequenceRep

readCollation = simpleObjRepFileRead CollationRep

readType = simpleObjRepFileRead TypeRep

readObjName :: OsPath -> ObjName
readObjName = fromPathFrag . OS.takeFileName

readFileRep ::
  forall m. (MonadUnliftIO m) => OsPath -> m Value
readFileRep filepath = rethrowIfNotExists $ do
  -- Careful, LBS.readFile is lazy and does not close
  -- the file handle unless we force the thunk, and not closing
  -- file handles can make shells with low ulimits barf.
  -- MacOS has particularly low ulimits.
  !fileContents <- liftIO $ OS.readFile filepath
  !decodedJson <-
    evaluate
      $ fromMaybe
        ( error $
            "File '"
              ++ show filepath
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
                      <> show filepath
                      <> " was expected but does not exist"
              else throwIO e
        )

simpleObjRepFileRead ::
  (MonadUnliftIO m) =>
  (ObjName -> Value -> a) ->
  OsPath ->
  m a
simpleObjRepFileRead f filepath =
  f (readObjName filepath) <$> readFileRep filepath

readMultiple ::
  (MonadUnliftIO m, HasName o) =>
  OsPath ->
  (OsPath -> m o) ->
  m (Map ObjName o)
readMultiple dir f = do
  foldersOrNotOne <- handle checkDoesNotExist $ Right . filter (/= [osp|objrep|]) <$> liftIO (OS.listDirectory dir)
  case foldersOrNotOne of
    Right folders -> do
      objList <- traverse (f . (dir OS.</>)) folders
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
readRepsFromDisk pgVersion schemaDir = do
  dir <- liftIO $ OS.encodeFS $ schemaDir </> show pgVersion
  DbRep
    <$> readFileRep (dir OS.</> [osp|db-settings|])
    <*> readMultiple (dir OS.</> [osp|schemas|]) readNamespaceRep
    <*> readMultiple (dir OS.</> [osp|roles|]) (simpleObjRepFileRead RoleRep)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM s r = s >>= flip when r
