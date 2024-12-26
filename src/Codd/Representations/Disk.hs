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
import GHC.IO.Exception (IOErrorType (UnsatisfiedConstraints), ioe_type)
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
    tryJust,
  )
import UnliftIO.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
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

  -- We detect permissions and if we don't have them, we write directly to the expected
  -- schema folder. It shouldn't be any slower than copying files one by one into the expected schema folder, so
  -- just as interruptible but no worse.
  maybeAtomicallyReplaceFolder schemaDir $ \tempDir -> do
    liftIO $ writeRec tempDir dbSchema
  where
    -- \| Allows the caller to wipe and replace a folder with new contents as atomically
    -- as the user's permissions will let us. That is, the supplied callback should write
    -- to the path it's supplied with as if it were the target folder, and after this finishes
    -- the target folder will be replaced with the contents written.
    -- This does not guarantee it won't leave the target folder in some intermediate state,
    -- it just makes an effort to avoid that.
    -- The entire thing may happen more or less atomically, and no promise is made regarding
    -- file modification times. This function is not thread-safe.
    maybeAtomicallyReplaceFolder :: FilePath -> (FilePath -> m ()) -> m ()
    maybeAtomicallyReplaceFolder folderToReplace f = do
      let tempDir = folderToReplace </> "../.temp-codd-dir-you-can-remove"
      errBestScenario <- tryJust (\(e :: IOError) -> if isPermissionError e || ioe_type e == UnsatisfiedConstraints then Just () else Nothing) $ do
        whenM (doesDirectoryExist tempDir) $ removePathForcibly tempDir
        createDirectoryIfMissing True tempDir
        renameDirectory tempDir schemaDir
        createDirectoryIfMissing True tempDir
      case errBestScenario of
        Right () -> do
          f tempDir
          renameDirectory tempDir folderToReplace
        Left _ -> do
          wipeDirSafely folderToReplace
          f folderToReplace

    -- \| Removes every file and subfolder of the supplied path to
    -- leave it empty, keeping the empty folder.
    wipeDirSafely :: FilePath -> m ()
    wipeDirSafely path = do
      entries <- listDirectory path
      forM_ entries $ \x -> removePathForcibly (path </> x)

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
