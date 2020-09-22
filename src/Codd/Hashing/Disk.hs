module Codd.Hashing.Disk (persistHashesToDisk, readHashesFromDisk) where

import Prelude hiding (writeFile, readFile)
import Codd.Prelude

import Codd.Hashing.Types
import Control.Monad (forM_, when, unless)
import Control.Monad.Except (MonadError(..), runExceptT)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (writeFile, readFile)
import GHC.Stack (HasCallStack)
import System.FilePath ((</>), takeFileName, takeDirectory)
import UnliftIO (MonadIO(..), throwIO)
import UnliftIO.Directory (getTemporaryDirectory, createDirectory, removeDirectoryRecursive, doesDirectoryExist, listDirectory, doesFileExist, copyFile, createDirectoryIfMissing)

-- TODO: A single tree-like structure that can be used both to read from disk and write to disk
readSchemaHash :: (MonadError Text m, MonadIO m) => FilePath -> m SchemaHash
readSchemaHash dir = SchemaHash (readObjName dir)
                            <$> readFileAsHash (dir </> "objhash")
                            <*> concatReaders [ readMultiple (dir </> "tables") readTable
                                              , readMultiple (dir </> "views") readView
                                              , readMultiple (dir </> "routines") readRoutine
                                              , readMultiple (dir </> "sequences") readSequence ]

readTable, readView, readRoutine, readSequence :: (MonadError Text m, MonadIO m) => FilePath -> m SchemaObjectHash
readTable dir = TableHash (readObjName dir)
                        <$> readFileAsHash (dir </> "objhash")
                        <*> readMultiple (dir </> "cols") readTableColumn
                        <*> readMultiple (dir </> "constraints") readTableConstraint
                        <*> readMultiple (dir </> "triggers") readTableTrigger
readView = simpleObjHashFileRead ViewHash
readRoutine = simpleObjHashFileRead RoutineHash
readSequence = simpleObjHashFileRead SequenceHash

readTableColumn :: (MonadError Text m, MonadIO m) => FilePath -> m TableColumn
readTableColumn = simpleObjHashFileRead TableColumn
readTableConstraint :: (MonadError Text m, MonadIO m) => FilePath -> m TableConstraint
readTableConstraint = simpleObjHashFileRead TableConstraint
readTableTrigger :: (MonadError Text m, MonadIO m) => FilePath -> m TableTrigger
readTableTrigger = simpleObjHashFileRead TableTrigger

readObjName :: FilePath -> ObjName
readObjName = fromPathFrag . takeFileName
readFileAsHash :: (MonadError Text m, MonadIO m) => FilePath -> m ObjHash
readFileAsHash filepath = do
    exists <- liftIO $ doesFileExist filepath
    unless exists $ throwError $ "File " <> pack filepath <> " was expected but does not exist"
    liftIO $ ObjHash <$> readFile filepath

simpleObjHashFileRead :: (MonadError Text m, MonadIO m) => (ObjName -> ObjHash -> a) -> FilePath -> m a
simpleObjHashFileRead f filepath = f (readObjName filepath) <$> readFileAsHash filepath
readMultiple :: (MonadError Text m, MonadIO m, IsDbObject o) => FilePath -> (FilePath -> m o) -> m (Map ObjName o)
readMultiple dir f = do
    dirExists <- doesDirectoryExist dir
    if dirExists then do
        folders <- filter (/= "objhash") <$> listDirectory dir
        objList <- traverse f $ map (dir </>) folders
        return $ listToMap objList
    else pure Map.empty

concatReaders :: (MonadError Text m, MonadIO m, Monoid s) => [m s] -> m s
concatReaders readers = mconcat <$> sequenceA readers -- do
    -- objList <- mconcat <$> sequenceA readers
    -- return $ Map.fromList $ map (\obj -> (objName obj, obj)) objList

toFiles :: DbHashes -> [(FilePath, ObjHash)]
toFiles (DbHashes (Map.elems -> schemas)) = concatMap objToFiles (map DbObject schemas)
    where
        objToFiles :: DbObject -> [(FilePath, ObjHash)]
        objToFiles obj =
            let dir = takeDirectory $ hashFileRelativeToParent obj
            in (hashFileRelativeToParent obj, objHash obj) : concatMap (\childObj -> map (prepend dir) $ objToFiles childObj) (childrenObjs obj)
        prepend folderName (file, c) = (folderName </> file, c)

-- | Wipes out completely the supplied folder and writes the hashes of the Database's structures to it again.
persistHashesToDisk :: (HasCallStack, MonadIO m) => DbHashes -> FilePath -> m ()
persistHashesToDisk dbHashes dir = do
    tempDir <- (</> "temp-db-dir") <$> getTemporaryDirectory
    whenM (doesDirectoryExist tempDir) $ removeDirectoryRecursive tempDir
    createDirectoryIfMissing False tempDir
    forM_ (nubOrd $ map fst $ toFiles dbHashes) $ \filepath ->
        createDirectoryIfMissing True (tempDir </> takeDirectory filepath)
    forM_ (toFiles dbHashes) $ \(filepath, (ObjHash filecontents)) ->
        liftIO $ writeFile (tempDir </> filepath) filecontents

    -- If the directory doesn't exist, we should simply ignore it
    whenM (doesDirectoryExist dir) $ removeDirectoryRecursive dir
    -- Important: renameDirectory will fail when the destination is a different partition. So we make a Copy instead.
    copyDir tempDir dir

readHashesFromDisk :: (HasCallStack, MonadIO m) => FilePath -> m DbHashes
readHashesFromDisk dir = do
    schemaFolders <- map (dir </>) <$> listDirectory dir
    schemaHashesE <- runExceptT $ traverse readSchemaHash schemaFolders
    case schemaHashesE of
        Left err -> throwIO $ userError $ "An error happened when reading hashes from disk: " <> unpack err
        Right schemaHashes -> return $ DbHashes $ Map.fromList $ map (\s -> (objName s,s)) schemaHashes

-- | Taken from https://stackoverflow.com/questions/6807025/what-is-the-haskell-way-to-copy-a-directory and modified
copyDir :: (HasCallStack, MonadIO m) => FilePath -> FilePath -> m ()
copyDir src dst = do
  createDirectory dst
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