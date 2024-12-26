module Codd.Representations.Disk
  ( persistRepsToDisk,
    readRepsFromDisk,
    toFiles,
  )
where

import Codd.Representations.Types
import Control.DeepSeq (force)
import Control.Monad
  ( forM,
    forM_,
    void,
    when,
  )
import Control.Monad.Identity (runIdentity)
import Data.Aeson
  ( Value,
    decode,
  )
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as Text
import GHC.Stack (HasCallStack)
import System.FilePath
  ( takeFileName,
    (</>),
  )
import System.IO.Error (isDoesNotExistError, isPermissionError)
import UnliftIO
  ( MonadIO (..),
    MonadUnliftIO,
    evaluate,
    handle,
    throwIO,
  )
import UnliftIO.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    getTemporaryDirectory,
    listDirectory,
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
    (forall b. (DbDiskObj b) => FilePath -> b -> m d) ->
    -- | This function will be called when writing to a file. A relative path
    -- to this structure's root will be passed.
    (FilePath -> Value -> m d) ->
    m [d]

simpleDbDiskObj ::
  (Functor m) => ObjName -> Value -> (FilePath -> Value -> m d) -> m [d]
simpleDbDiskObj oname orep ffile = (: []) <$> ffile (mkPathFrag oname) orep

instance DbDiskObj DbRep where
  appr (DbRep dbSettingsRep schemas roles) frec ffile = do
    x <- ffile "db-settings" dbSettingsRep
    rs <- forM (Map.elems roles) $ frec "roles"
    ss <- forM (Map.toList schemas) $ \(schemaName, schema) ->
      frec ("schemas" </> mkPathFrag schemaName) schema
    pure $ x : rs ++ ss

instance DbDiskObj RoleRep where
  appr (RoleRep roleName roleRep) _ = simpleDbDiskObj roleName roleRep

instance DbDiskObj SchemaRep where
  appr (SchemaRep _schemaName namespaceRep tables views routines seqs colls types) frec ffile =
    do
      x <- ffile "objrep" namespaceRep
      tbls <- forM (Map.toList tables) $ \(tableName, table) ->
        frec ("tables" </> mkPathFrag tableName) table
      vs <- forM (Map.elems views) $ frec "views"
      rs <- forM (Map.elems routines) $ frec "routines"
      ss <- forM (Map.elems seqs) $ frec "sequences"
      cs <- forM (Map.elems colls) $ frec "collations"
      ts <- forM (Map.elems types) $ frec "types"
      pure $ x : tbls ++ vs ++ rs ++ ss ++ cs ++ ts

instance DbDiskObj TableRep where
  appr (TableRep _tblName tblRep columns constraints triggers policies indexes statistics) frec ffile =
    do
      let mkpath p = p
      x <- ffile (mkpath "objrep") tblRep
      cols <- forM (Map.elems columns) $ frec (mkpath "cols")
      constrs <-
        forM (Map.elems constraints) $
          frec (mkpath "constraints")
      trgrs <- forM (Map.elems triggers) $ frec (mkpath "triggers")
      pols <- forM (Map.elems policies) $ frec (mkpath "policies")
      idxs <- forM (Map.elems indexes) $ frec (mkpath "indexes")
      stats <- forM (Map.elems statistics) $ frec (mkpath "statistics")
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

toFiles :: DbRep -> [(FilePath, Value)]
toFiles = sortOn fst . frec
  where
    frec :: (DbDiskObj a) => a -> [(FilePath, Value)]
    frec sobj =
      concat $
        runIdentity $
          appr
            sobj
            (\parentDir obj -> pure $ prependDir parentDir $ frec obj)
            (\fn h -> pure [(fn, h)])
    prependDir dir = map (first (dir </>))

data HandledIOError = DoesNotExist | PermissionDenied

-- | Wipes out completely the supplied folder and writes the representations of the Database's structures to it again.
persistRepsToDisk :: forall m. (HasCallStack, MonadUnliftIO m) => DbRep -> FilePath -> m ()
persistRepsToDisk dbSchema schemaDir = do
  -- We want this function to be fast and as atomic as it can be, but:
  -- - `renameFile` throws if the target path is in a different partition from the source path
  -- - The user might not have permissions to delete the target folder but does have permissions to modify its contents
  -- - Different operating systems can have different behaviours, like Windows, where renameDirectory fails on Windows if the target directory exists: https://hackage.haskell.org/package/directory-1.3.9.0/docs/System-Directory.html#v:renameDirectory
  -- - Windows and I think even Linux can have ACLs that have more than just a binary "can-write" privilege, with "can-delete" being
  --   separated from "can-create", IIRC.
  --
  -- We of course can't handle cases where the user doesn't have privileges of modifying the contents of the expected-schema
  -- folder, but we do try to optimise for speed when the user has a bit more privileges than necessary, falling back to
  -- a slower method when they don't.

  tempDir <- getEmptyTempFolder
  wipePathSafely schemaDir
  liftIO $ writeRec tempDir dbSchema

  renameDirectorySafely tempDir schemaDir
  where
    handleTypicalErrors :: m () -> m (Either HandledIOError ())
    handleTypicalErrors f =
      handle
        ( \(e :: IOError) ->
            if isPermissionError e
              then pure (Left PermissionDenied)
              else
                if isDoesNotExistError e
                  then
                    pure (Left DoesNotExist)
                  else throwIO e
        )
        (Right <$> f)
    -- \| Returns an empty folder which is probabilistically atomically/quickly rename-able into the expected schema folder,
    -- but if that's not possible this still returns a usable and empty temporary folder.
    getEmptyTempFolder :: m FilePath
    getEmptyTempFolder = do
      let tempDir = schemaDir </> "../.temp-codd-dir-you-can-remove"
      errBestScenario <- handleTypicalErrors $ removePathForcibly tempDir >> createDirectoryIfMissing True tempDir
      case errBestScenario of
        Left PermissionDenied -> liftIO getTemporaryDirectory
        Left DoesNotExist -> liftIO getTemporaryDirectory
        Right () -> pure tempDir
    -- \| Able to remove a path completely if permissions allow, but can otherwise
    -- fallback to removing every file and subfolder of the supplied path to
    -- leave it empty (keeping the path alive in that case).
    wipePathSafely :: FilePath -> m ()
    wipePathSafely path = do
      err <- handleTypicalErrors $ removePathForcibly path
      case err of
        Right () -> pure ()
        Left DoesNotExist -> pure ()
        Left PermissionDenied -> do
          entries <- listDirectory path
          forM_ entries $ \x -> removePathForcibly (path </> x)

    -- \| Uses the atomic and fast `renameDirectory`, but handles lack of permissions gracefully by recursively copying
    -- files and folders from the source to the target and then deleting the source.
    renameDirectorySafely :: FilePath -> FilePath -> m ()
    renameDirectorySafely src dst = do
      err <- handleTypicalErrors $ liftIO $ renameDirectory src dst
      case err of
        Right () -> pure ()
        Left DoesNotExist -> throwIO $ userError "Got a DoesNotExist error while renaming a directory. If you aren't actively modifying paths, please report this as a bug in codd"
        Left PermissionDenied -> do
    writeRec :: (DbDiskObj a) => FilePath -> a -> IO ()
    writeRec dir obj =
      void $
        appr
          obj
          ( \parentDir sobj -> do
              createDirectoryIfMissing True (dir </> parentDir)
              writeRec (dir </> parentDir) sobj
          )
          (\fn jsonRep -> Text.writeFile (dir </> fn) (detEncodeJSON jsonRep))

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

readRepsFromDisk :: (MonadUnliftIO m) => FilePath -> m DbRep
readRepsFromDisk dir =
  DbRep
    <$> readFileRep (dir </> "db-settings")
    <*> readMultiple (dir </> "schemas") readNamespaceRep
    <*> readMultiple (dir </> "roles") (simpleObjRepFileRead RoleRep)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM s r = s >>= flip when r
