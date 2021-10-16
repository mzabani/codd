module Codd.Hashing.Disk
    ( persistHashesToDisk
    , readHashesFromDisk
    , toFiles
    ) where

import           Codd.Prelude
import           Prelude                 hiding ( readFile
                                                , writeFile
                                                )

import           Codd.Hashing.Types
import           Control.Monad                  ( forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.Except           ( MonadError(..)
                                                , runExceptT
                                                )
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
                                                , takeDirectory
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

-- TODO: A single tree-like structure that can be used both to read from disk and write to disk
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
        <*> concatReaders
                [ readMultiple (dir </> "tables")     readTable
                , readMultiple (dir </> "views")      readView
                , readMultiple (dir </> "routines")   readRoutine
                , readMultiple (dir </> "sequences")  readSequence
                , readMultiple (dir </> "collations") readCollation
                ]

readTable, readView, readRoutine, readSequence, readCollation
    :: (MonadError Text m, MonadIO m) => FilePath -> m SchemaObjectHash
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
    :: (MonadError Text m, MonadIO m, IsDbObject o)
    => FilePath
    -> (FilePath -> m o)
    -> m (Map ObjName o)
readMultiple dir f = do
    dirExists <- doesDirectoryExist dir
    if dirExists
        then do
            folders <- filter (/= "objhash") <$> listDirectory dir
            objList <- traverse f $ map (dir </>) folders
            return $ listToMap objList
        else pure Map.empty

concatReaders :: (MonadError Text m, MonadIO m, Monoid s) => [m s] -> m s
concatReaders readers = mconcat <$> sequenceA readers

toFiles :: DbHashes -> [(FilePath, ObjHash)]
toFiles (DbHashes dbSettingsHash (Map.elems -> schemas) (Map.elems -> roles)) =
    ("db-settings", dbSettingsHash)
        : concatMap objToFiles (map DbObject schemas ++ map DbObject roles)
  where
    objToFiles :: DbObject -> [(FilePath, ObjHash)]
    objToFiles obj =
        let dir = takeDirectory $ hashFileRelativeToParent obj
        in  (hashFileRelativeToParent obj, objHash obj) : concatMap
                (map (prepend dir) . objToFiles)
                (childrenObjs obj)
    prepend folderName (file, c) = (folderName </> file, c)

-- | Wipes out completely the supplied folder and writes the hashes of the Database's structures to it again.
persistHashesToDisk
    :: (HasCallStack, MonadIO m) => DbHashes -> FilePath -> m ()
persistHashesToDisk dbHashes dir = do
    tempDir <- (</> "temp-db-dir") <$> getTemporaryDirectory
    whenM (doesDirectoryExist tempDir) $ wipeDir tempDir
    createDirectoryIfMissing False tempDir
    forM_ (nubOrd $ map fst $ toFiles dbHashes) $ \filepath ->
        createDirectoryIfMissing True (tempDir </> takeDirectory filepath)
    forM_ (toFiles dbHashes) $ \(filepath, ObjHash filecontents) ->
        liftIO $ writeFile (tempDir </> filepath) filecontents

    -- If the directory doesn't exist, we should simply ignore it
    -- Note: the folder parent to "dir" might not have permissions for us to delete things inside it,
    -- so we modify only strictly inside "dir"
    whenM (doesDirectoryExist dir) $ wipeDir dir
    -- Important: renameDirectory will fail when the destination is a different partition. So we make a Copy instead.
    copyDir tempDir dir
  where
    wipeDir d = do
        xs <- listDirectory d
        forM_ xs $ \x -> removePathForcibly (d </> x)

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
