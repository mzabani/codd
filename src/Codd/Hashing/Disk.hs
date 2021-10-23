{-# LANGUAGE DataKinds #-}
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
                                                , createDirectory
                                                , createDirectoryIfMissing
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , getTemporaryDirectory
                                                , listDirectory
                                                , removePathForcibly
                                                )

-- data OnDiskRoot = OnDiskRoot FilePath ObjHash [OnDiskSchema]
-- data OnDiskSchema = OnDiskSchema FilePath ObjHash 

-- TODO: Is this equivalent to monadic unfolding?
class DbDiskObj a where
    appr :: Monad m => a -> (forall b. DbDiskObj b => FilePath -> b -> m d) -> (FilePath -> ObjHash -> m d) -> m [d]

instance DbDiskObj DbHashes where
    appr (DbHashes dbSettingsHash schemas roles) frec ffile = do
        x  <- ffile "db-settings" dbSettingsHash
        rs <- forM (Map.elems roles) $ frec "roles"
        ss <- forM (Map.elems schemas) $ frec "schemas"
        pure $ x : rs ++ ss

instance DbDiskObj RoleHash where
    appr (RoleHash roleName roleHash) _ ffile =
        (: []) <$> ffile (mkPathFrag roleName) roleHash

instance DbDiskObj SchemaHash where
    appr (SchemaHash schemaName schemaHash _tables _views _routines _seqs _colls) _frec ffile
        = do
            x <- ffile (mkPathFrag schemaName </> "objhash") schemaHash
            pure [x]

toFiles :: DbHashes -> [(FilePath, ObjHash)]
toFiles = sortOn fst . frec
  where
    frec :: DbDiskObj a => a -> [(FilePath, ObjHash)]
    frec sobj = concat $ runIdentity $ appr
        sobj
        (\parentDir obj -> pure $ prependDir parentDir $ frec obj)
        (\fn h -> pure [(fn, h)])
    prependDir dir = map (first (dir </>))

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
    :: (MonadError Text m, MonadIO m, IsDbObject o)
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

writeAllHashesNew :: DbHashes -> FilePath -> IO ()
writeAllHashesNew dbHashes rootDir = frec rootDir dbHashes
  where
    frec :: DbDiskObj a => FilePath -> a -> IO ()
    frec dir obj = void $ appr
        obj
        (\parentDir sobj -> do
            createDirectoryIfMissing False (dir </> parentDir)
            frec parentDir sobj
        )
        (\fn (ObjHash fh) -> writeFile (dir </> fn) fh)

writeAllHashes :: DbHashes -> FilePath -> IO ()
writeAllHashes (DbHashes dbSettings schemas roles) rootDir = do
    createDirectoryIfMissing False rootDir
    writeHashFile (rootDir </> "db-settings") dbSettings
    createDirectory (rootDir </> "schemas")
    createDirectory (rootDir </> "roles")
    forM_ schemas $ writeSchema (rootDir </> "schemas")
    forM_ roles $ writeRole (rootDir </> "roles")

  where
    writeSchema dir (SchemaHash sname shash tables views routines seqs colls) =
        do
            let schemaDir = dir </> mkPathFrag sname
            createDirectory schemaDir
            writeHashFile (schemaDir </> "objhash") shash

            createDirectory (schemaDir </> "tables")
            createDirectory (schemaDir </> "views")
            createDirectory (schemaDir </> "routines")
            createDirectory (schemaDir </> "sequences")
            createDirectory (schemaDir </> "collations")
            forM_ tables $ writeTable (schemaDir </> "tables")
            forM_ views $ writeView (schemaDir </> "views")
            forM_ routines $ writeRoutine (schemaDir </> "routines")
            forM_ seqs $ writeSequence (schemaDir </> "sequences")
            forM_ colls $ writeCollation (schemaDir </> "collations")

    writeRole dir (RoleHash rname rhash) =
        writeHashFile (dir </> mkPathFrag rname) rhash

    writeTable tablesDir (TableHash tname thash cols constraints triggers policies indexes)
        = do
            let tableDir = tablesDir </> mkPathFrag tname
            createDirectory tableDir
            writeHashFile (tableDir </> "objhash") thash
            createDirectory (tableDir </> "cols")
            createDirectory (tableDir </> "constraints")
            createDirectory (tableDir </> "triggers")
            createDirectory (tableDir </> "policies")
            createDirectory (tableDir </> "indexes")
            forM_ cols $ writeColumn (tableDir </> "cols")
            forM_ constraints $ writeConstraint (tableDir </> "constraints")
            forM_ triggers $ writeTrigger (tableDir </> "triggers")
            forM_ policies $ writePolicy (tableDir </> "policies")
            forM_ indexes $ writeIndex (tableDir </> "indexes")

    writeView dir (ViewHash name hash) =
        writeHashFile (dir </> mkPathFrag name) hash
    writeRoutine dir (RoutineHash name hash) =
        writeHashFile (dir </> mkPathFrag name) hash
    writeSequence dir (SequenceHash name hash) =
        writeHashFile (dir </> mkPathFrag name) hash
    writeCollation dir (CollationHash name hash) =
        writeHashFile (dir </> mkPathFrag name) hash

    writeColumn dir (TableColumn name hash) =
        writeHashFile (dir </> mkPathFrag name) hash
    writeConstraint dir (TableConstraint name hash) =
        writeHashFile (dir </> mkPathFrag name) hash
    writeTrigger dir (TableTrigger name hash) =
        writeHashFile (dir </> mkPathFrag name) hash
    writePolicy dir (TablePolicy name hash) =
        writeHashFile (dir </> mkPathFrag name) hash
    writeIndex dir (TableIndex name hash) =
        writeHashFile (dir </> mkPathFrag name) hash

    writeHashFile file (ObjHash textHash) = writeFile file textHash

-- | Wipes out completely the supplied folder and writes the hashes of the Database's structures to it again.
persistHashesToDisk
    :: (HasCallStack, MonadIO m) => DbHashes -> FilePath -> m ()
persistHashesToDisk dbHashes dir = do
    tempDir <- (</> "temp-db-dir") <$> getTemporaryDirectory
    whenM (doesDirectoryExist tempDir) $ wipeDir tempDir
    liftIO $ writeAllHashes dbHashes tempDir

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
