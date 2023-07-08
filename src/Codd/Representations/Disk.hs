module Codd.Representations.Disk
    ( persistRepsToDisk
    , readRepsFromDisk
    , toFiles
    ) where

import           Prelude                 hiding ( readFile
                                                , writeFile
                                                )

import           Codd.Representations.Types
import           Control.Monad                  ( forM
                                                , forM_
                                                , unless
                                                , void
                                                , when
                                                )
import           Control.Monad.Except           ( MonadError(..)
                                                , runExceptT
                                                )
import           Control.Monad.Identity         ( runIdentity )
import           Data.Aeson                     ( Value
                                                , decode
                                                )
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.List                      ( sortOn )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text.IO                  as Text
import           GHC.Stack                      ( HasCallStack )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                )
import           UnliftIO                       ( MonadIO(..)
                                                , throwIO
                                                )
import           UnliftIO.Directory             ( copyFile
                                                , createDirectoryIfMissing
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , getTemporaryDirectory
                                                , listDirectory
                                                , removePathForcibly
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
    appr :: Monad m => a
        -> (forall b. DbDiskObj b => FilePath -> b -> m d)
        -- ^ When recursing into a new sub-structure, this function
        -- will be called with a relative folder where that substructure's
        -- root belongs to.
        -> (FilePath -> Value -> m d)
        -- ^ This function will be called when writing to a file. A relative path
        -- to this structure's root will be passed.
        -> m [d]

simpleDbDiskObj
    :: Functor m => ObjName -> Value -> (FilePath -> Value -> m d) -> m [d]
simpleDbDiskObj oname orep ffile = (: []) <$> ffile (mkPathFrag oname) orep

instance DbDiskObj DbRep where
    appr (DbRep dbSettingsRep schemas roles) frec ffile = do
        x  <- ffile "db-settings" dbSettingsRep
        rs <- forM (Map.elems roles) $ frec "roles"
        ss <- forM (Map.toList schemas) $ \(schemaName, schema) ->
            frec ("schemas" </> mkPathFrag schemaName) schema
        pure $ x : rs ++ ss

instance DbDiskObj RoleRep where
    appr (RoleRep roleName roleRep) _ = simpleDbDiskObj roleName roleRep
instance DbDiskObj SchemaRep where
    appr (SchemaRep _schemaName namespaceRep tables views routines seqs colls types) frec ffile
        = do
            x    <- ffile "objrep" namespaceRep
            tbls <- forM (Map.toList tables) $ \(tableName, table) ->
                frec ("tables" </> mkPathFrag tableName) table
            vs <- forM (Map.elems views) $ frec "views"
            rs <- forM (Map.elems routines) $ frec "routines"
            ss <- forM (Map.elems seqs) $ frec "sequences"
            cs <- forM (Map.elems colls) $ frec "collations"
            ts <- forM (Map.elems types) $ frec "types"
            pure $ x : tbls ++ vs ++ rs ++ ss ++ cs ++ ts

instance DbDiskObj TableRep where
    appr (TableRep _tblName tblRep columns constraints triggers policies indexes) frec ffile
        = do
            let mkpath p = p
            x       <- ffile (mkpath "objrep") tblRep
            cols    <- forM (Map.elems columns) $ frec (mkpath "cols")
            constrs <- forM (Map.elems constraints)
                $ frec (mkpath "constraints")
            trgrs <- forM (Map.elems triggers) $ frec (mkpath "triggers")
            pols  <- forM (Map.elems policies) $ frec (mkpath "policies")
            idxs  <- forM (Map.elems indexes) $ frec (mkpath "indexes")
            pure $ x : cols ++ constrs ++ trgrs ++ pols ++ idxs

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
    frec :: DbDiskObj a => a -> [(FilePath, Value)]
    frec sobj = concat $ runIdentity $ appr
        sobj
        (\parentDir obj -> pure $ prependDir parentDir $ frec obj)
        (\fn h -> pure [(fn, h)])
    prependDir dir = map (first (dir </>))

-- | Wipes out completely the supplied folder and writes the representations of the Database's structures to it again.
persistRepsToDisk :: (HasCallStack, MonadIO m) => DbRep -> FilePath -> m ()
persistRepsToDisk dbSchema rootDir = do
    tempDir <- (</> "temp-db-dir") <$> getTemporaryDirectory
    whenM (doesDirectoryExist tempDir) $ wipeDir tempDir
    createDirectoryIfMissing False tempDir
    liftIO $ writeRec tempDir dbSchema

    -- If the directory doesn't exist, we should simply ignore it
    -- Note: the folder parent to "dir" might not have permissions for us to delete things inside it,
    -- so we modify only strictly inside "dir"
    whenM (doesDirectoryExist rootDir) $ wipeDir rootDir
    -- Important: renameDirectory will fail when the destination is a different partition. So we make a Copy instead.
    copyDir tempDir rootDir
  where
    wipeDir d = do
        xs <- listDirectory d
        forM_ xs $ \x -> removePathForcibly (d </> x)
    writeRec :: DbDiskObj a => FilePath -> a -> IO ()
    writeRec dir obj = void $ appr
        obj
        (\parentDir sobj -> do
            createDirectoryIfMissing True (dir </> parentDir)
            writeRec (dir </> parentDir) sobj
        )
        (\fn jsonRep -> Text.writeFile (dir </> fn) (detEncodeJSON jsonRep))

readExpectedSchema :: (MonadError Text m, MonadIO m) => FilePath -> m DbRep
readExpectedSchema dir =
    DbRep
        <$> readFileRep (dir </> "db-settings")
        <*> readMultiple (dir </> "schemas") readNamespaceRep
        <*> readMultiple (dir </> "roles")   (simpleObjRepFileRead RoleRep)

readNamespaceRep :: (MonadError Text m, MonadIO m) => FilePath -> m SchemaRep
readNamespaceRep dir =
    SchemaRep (readObjName dir)
        <$> readFileRep (dir </> "objrep")
        <*> readMultiple (dir </> "tables")     readTable
        <*> readMultiple (dir </> "views")      readView
        <*> readMultiple (dir </> "routines")   readRoutine
        <*> readMultiple (dir </> "sequences")  readSequence
        <*> readMultiple (dir </> "collations") readCollation
        <*> readMultiple (dir </> "types")      readType

readTable :: (MonadError Text m, MonadIO m) => FilePath -> m TableRep
readView :: (MonadError Text m, MonadIO m) => FilePath -> m ViewRep
readRoutine :: (MonadError Text m, MonadIO m) => FilePath -> m RoutineRep
readSequence :: (MonadError Text m, MonadIO m) => FilePath -> m SequenceRep
readCollation :: (MonadError Text m, MonadIO m) => FilePath -> m CollationRep
readType :: (MonadError Text m, MonadIO m) => FilePath -> m TypeRep
readTable dir =
    TableRep (readObjName dir)
        <$> readFileRep (dir </> "objrep")
        <*> readMultiple (dir </> "cols") (simpleObjRepFileRead TableColumnRep)
        <*> readMultiple (dir </> "constraints")
                         (simpleObjRepFileRead TableConstraintRep)
        <*> readMultiple (dir </> "triggers")
                         (simpleObjRepFileRead TableTriggerRep)
        <*> readMultiple (dir </> "policies")
                         (simpleObjRepFileRead TablePolicyRep)
        <*> readMultiple (dir </> "indexes")
                         (simpleObjRepFileRead TableIndexRep)
readView = simpleObjRepFileRead ViewRep
readRoutine = simpleObjRepFileRead RoutineRep
readSequence = simpleObjRepFileRead SequenceRep
readCollation = simpleObjRepFileRead CollationRep
readType = simpleObjRepFileRead TypeRep

readObjName :: FilePath -> ObjName
readObjName = fromPathFrag . takeFileName
readFileRep
    :: forall m . (MonadError Text m, MonadIO m) => FilePath -> m Value
readFileRep filepath = do
    exists <- liftIO $ doesFileExist filepath
    unless exists
        $  throwError
        $  "File "
        <> pack filepath
        <> " was expected but does not exist"
    -- Careful, LBS.readFile is lazy and does not close
    -- the file handle unless we force the thunk. So we
    -- use BS.readFile to be on the safe side
    fileContents <- liftIO $ BS.readFile filepath
    pure
        $ fromMaybe
              (  error
              $  "File '"
              ++ filepath
              ++ "' was supposed to contain a JSON value"
              )
        $ decode
        $ LBS.fromStrict fileContents

simpleObjRepFileRead
    :: (MonadError Text m, MonadIO m)
    => (ObjName -> Value -> a)
    -> FilePath
    -> m a
simpleObjRepFileRead f filepath =
    f (readObjName filepath) <$> readFileRep filepath

readMultiple
    :: (MonadError Text m, MonadIO m, HasName o)
    => FilePath
    -> (FilePath -> m o)
    -> m (Map ObjName o)
readMultiple dir f = do
    dirExists <- doesDirectoryExist dir
    if dirExists
        then do
            folders <- filter (/= "objrep") <$> listDirectory dir
            objList <- traverse (f . (dir </>)) folders
            return $ listToMap objList
        else pure Map.empty

readRepsFromDisk :: MonadIO m => FilePath -> m DbRep
readRepsFromDisk dir = do
    allRepsE <- runExceptT $ readExpectedSchema dir
    case allRepsE of
        Left err ->
            throwIO
                $  userError
                $  "An error happened when reading representations from disk: "
                <> unpack err
        Right allReps -> return allReps

-- | Taken from https://stackoverflow.com/questions/6807025/what-is-the-haskell-way-to-copy-a-directory and modified
copyDir :: (HasCallStack, MonadIO m) => FilePath -> FilePath -> m ()
copyDir src dst = do
    createDirectoryIfMissing True dst
    xs <- listDirectory src
    forM_ xs $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
            then copyDir srcPath dstPath
            else copyFile srcPath dstPath

whenM :: Monad m => m Bool -> m () -> m ()
whenM s r = s >>= flip when r
