module Codd.Hashing.Disk
    ( persistHashesToDisk
    , readHashesFromDisk
    , toFiles
    ) where

import           Prelude                 hiding ( readFile
                                                , writeFile
                                                )

import           Codd.Hashing.Types
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
import           Data.Bifunctor                 ( first )
import           Data.List                      ( sortOn )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Text.IO                   ( readFile
                                                , writeFile
                                                )
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
This module contains functions and data models to write and read checksums
from disk. It has been rewritten a few times in attempt to find a model
that is sufficiently clean and robust.

The unsolved challenge at this point is to unify both reading and writing
under a unique model that represents how files and directories are laid out
on disk.
-}

-- | This class is equivalent to some form of monadic unfolding, mixed
-- with a mockable "writeFile" function. It allows us to derive both
-- `toFiles` and `persistHashesToDisk`, for example, but isn't a very elegant
-- model otherwise.
class DbDiskObj a where
    appr :: Monad m => a
        -> (forall b. DbDiskObj b => FilePath -> b -> m d)
        -- ^ When recursing into a new sub-structure, this function
        -- will be called with a relative folder where that substructure's
        -- root belongs to.
        -> (FilePath -> ObjHash -> m d)
        -- ^ This function will be called when writing to a file. A relative path
        -- to this structure's root will be passed.
        -> m [d]

simpleDbDiskObj
    :: Functor m => ObjName -> ObjHash -> (FilePath -> ObjHash -> m d) -> m [d]
simpleDbDiskObj oname ohash ffile = (: []) <$> ffile (mkPathFrag oname) ohash

instance DbDiskObj DbHashes where
    appr (DbHashes dbSettingsHash schemas roles) frec ffile = do
        x  <- ffile "db-settings" dbSettingsHash
        rs <- forM (Map.elems roles) $ frec "roles"
        ss <- forM (Map.toList schemas) $ \(schemaName, schema) ->
            frec ("schemas" </> mkPathFrag schemaName) schema
        pure $ x : rs ++ ss

instance DbDiskObj RoleHash where
    appr (RoleHash roleName roleHash) _ = simpleDbDiskObj roleName roleHash
instance DbDiskObj SchemaHash where
    appr (SchemaHash _schemaName schemaHash tables views routines seqs colls) frec ffile
        = do
            let mkpath p = p
            x    <- ffile (mkpath "objhash") schemaHash
            tbls <- forM (Map.toList tables) $ \(tableName, table) ->
                frec ("tables" </> mkPathFrag tableName) table
            vs <- forM (Map.elems views) $ frec (mkpath "views")
            rs <- forM (Map.elems routines) $ frec (mkpath "routines")
            ss <- forM (Map.elems seqs) $ frec (mkpath "sequences")
            cs <- forM (Map.elems colls) $ frec (mkpath "collations")
            pure $ x : tbls ++ vs ++ rs ++ ss ++ cs

instance DbDiskObj TableHash where
    appr (TableHash _tblName tblHash columns constraints triggers policies indexes) frec ffile
        = do
            let mkpath p = p
            x       <- ffile (mkpath "objhash") tblHash
            cols    <- forM (Map.elems columns) $ frec (mkpath "cols")
            constrs <- forM (Map.elems constraints)
                $ frec (mkpath "constraints")
            trgrs <- forM (Map.elems triggers) $ frec (mkpath "triggers")
            pols  <- forM (Map.elems policies) $ frec (mkpath "policies")
            idxs  <- forM (Map.elems indexes) $ frec (mkpath "indexes")
            pure $ x : cols ++ constrs ++ trgrs ++ pols ++ idxs

instance DbDiskObj TableColumn where
    appr (TableColumn colName colHash) _ = simpleDbDiskObj colName colHash
instance DbDiskObj TableConstraint where
    appr (TableConstraint constrName constrHash) _ =
        simpleDbDiskObj constrName constrHash
instance DbDiskObj TableTrigger where
    appr (TableTrigger triggerName triggerHash) _ =
        simpleDbDiskObj triggerName triggerHash
instance DbDiskObj TablePolicy where
    appr (TablePolicy polName polHash) _ = simpleDbDiskObj polName polHash
instance DbDiskObj TableIndex where
    appr (TableIndex idxName idxHash) _ = simpleDbDiskObj idxName idxHash
instance DbDiskObj ViewHash where
    appr (ViewHash viewName viewHash) _ = simpleDbDiskObj viewName viewHash
instance DbDiskObj RoutineHash where
    appr (RoutineHash routineName routineHash) _ =
        simpleDbDiskObj routineName routineHash
instance DbDiskObj SequenceHash where
    appr (SequenceHash seqName seqHash) _ = simpleDbDiskObj seqName seqHash
instance DbDiskObj CollationHash where
    appr (CollationHash collName collHash) _ =
        simpleDbDiskObj collName collHash


toFiles :: DbHashes -> [(FilePath, ObjHash)]
toFiles = sortOn fst . frec
  where
    frec :: DbDiskObj a => a -> [(FilePath, ObjHash)]
    frec sobj = concat $ runIdentity $ appr
        sobj
        (\parentDir obj -> pure $ prependDir parentDir $ frec obj)
        (\fn h -> pure [(fn, h)])
    prependDir dir = map (first (dir </>))

-- | Wipes out completely the supplied folder and writes the hashes of the Database's structures to it again.
persistHashesToDisk
    :: (HasCallStack, MonadIO m) => DbHashes -> FilePath -> m ()
persistHashesToDisk dbHashes rootDir = do
    tempDir <- (</> "temp-db-dir") <$> getTemporaryDirectory
    whenM (doesDirectoryExist tempDir) $ wipeDir tempDir
    createDirectoryIfMissing False tempDir
    liftIO $ writeRec tempDir dbHashes

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
        (\fn (ObjHash fh) -> writeFile (dir </> fn) fh)

readAllHashes :: (MonadError Text m, MonadIO m) => FilePath -> m DbHashes
readAllHashes dir =
    DbHashes
        <$> readFileAsHash (dir </> "db-settings")
        <*> readMultiple (dir </> "schemas") readSchemaHash
        <*> readMultiple (dir </> "roles")   (simpleObjHashFileRead RoleHash)
readSchemaHash :: (MonadError Text m, MonadIO m) => FilePath -> m SchemaHash
readSchemaHash dir =
    SchemaHash (readObjName dir)
        <$> readFileAsHash (dir </> "objhash")
        <*> readMultiple (dir </> "tables")     readTable
        <*> readMultiple (dir </> "views")      readView
        <*> readMultiple (dir </> "routines")   readRoutine
        <*> readMultiple (dir </> "sequences")  readSequence
        <*> readMultiple (dir </> "collations") readCollation

readTable :: (MonadError Text m, MonadIO m) => FilePath -> m TableHash
readView :: (MonadError Text m, MonadIO m) => FilePath -> m ViewHash
readRoutine :: (MonadError Text m, MonadIO m) => FilePath -> m RoutineHash
readSequence :: (MonadError Text m, MonadIO m) => FilePath -> m SequenceHash
readCollation :: (MonadError Text m, MonadIO m) => FilePath -> m CollationHash
readTable dir =
    TableHash (readObjName dir)
        <$> readFileAsHash (dir </> "objhash")
        <*> readMultiple (dir </> "cols") (simpleObjHashFileRead TableColumn)
        <*> readMultiple (dir </> "constraints")
                         (simpleObjHashFileRead TableConstraint)
        <*> readMultiple (dir </> "triggers")
                         (simpleObjHashFileRead TableTrigger)
        <*> readMultiple (dir </> "policies")
                         (simpleObjHashFileRead TablePolicy)
        <*> readMultiple (dir </> "indexes") (simpleObjHashFileRead TableIndex)
readView = simpleObjHashFileRead ViewHash
readRoutine = simpleObjHashFileRead RoutineHash
readSequence = simpleObjHashFileRead SequenceHash
readCollation = simpleObjHashFileRead CollationHash

readObjName :: FilePath -> ObjName
readObjName = fromPathFrag . takeFileName
readFileAsHash :: (MonadError Text m, MonadIO m) => FilePath -> m ObjHash
readFileAsHash filepath = do
    exists <- liftIO $ doesFileExist filepath
    unless exists
        $  throwError
        $  "File "
        <> pack filepath
        <> " was expected but does not exist"
    liftIO $ ObjHash <$> readFile filepath

simpleObjHashFileRead
    :: (MonadError Text m, MonadIO m)
    => (ObjName -> ObjHash -> a)
    -> FilePath
    -> m a
simpleObjHashFileRead f filepath =
    f (readObjName filepath) <$> readFileAsHash filepath

readMultiple
    :: (MonadError Text m, MonadIO m, HasName o)
    => FilePath
    -> (FilePath -> m o)
    -> m (Map ObjName o)
readMultiple dir f = do
    dirExists <- doesDirectoryExist dir
    if dirExists
        then do
            folders <- filter (/= "objhash") <$> listDirectory dir
            objList <- traverse (f . (dir </>)) folders
            return $ listToMap objList
        else pure Map.empty

readHashesFromDisk :: (HasCallStack, MonadIO m) => FilePath -> m DbHashes
readHashesFromDisk dir = do
    allHashesE <- runExceptT $ readAllHashes dir
    case allHashesE of
        Left err ->
            throwIO
                $  userError
                $  "An error happened when reading hashes from disk: "
                <> unpack err
        Right allHashes -> return allHashes

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
