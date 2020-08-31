module Codd.Hashing (getDbHashes, persistHashesToDisk, readHashesFromDisk, DbHashes(..), SchemaHash(..), SchemaObjectHash(..), TableColumn(..), ObjHash(..), ObjName(..), IsDbObject(..), DbObject(..)) where

import Prelude hiding (writeFile, readFile)
import Control.Monad (forM, forM_, when, unless, (<=<))
import Control.Monad.Except (runExceptT, throwError, MonadError)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Char as Char
import Data.List (sortOn)
import Data.Text (Text, pack)
import Data.Text.IO (writeFile, readFile)
import qualified Data.Text as Text
import Data.String (IsString(..))
import Database.PostgreSQL.Simple (ToRow, Query)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import qualified Database.PostgreSQL.Simple as DB
import Codd.Internal (connectAndDispose)
import GHC.Stack (HasCallStack)
import System.FilePath ((</>), joinPath, splitPath, takeFileName, takeDirectory)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (getTemporaryDirectory, createDirectory, removeDirectoryRecursive, renameDirectory, doesDirectoryExist, listDirectory, doesFileExist, copyFile, createDirectoryIfMissing)
import UnliftIO.Exception (throwIO)

hashProjection :: [QueryFrag] -> QueryFrag
hashProjection cols = "MD5(" <> interspBy " || " (map toHash cols) <> ")"
  where
    toHash col = "(CASE WHEN " <> col <> " IS NULL THEN '' ELSE '_' || " <> col <> " :: TEXT END)"
    interspBy _ [] = ""
    interspBy _ (c:[]) = c
    interspBy sep (c:cs) = c <> sep <> interspBy sep cs

data QueryFrag = forall a. ToRow a => QueryFrag Query a
instance IsString QueryFrag where
    fromString s = QueryFrag (fromString s) ()
instance Semigroup QueryFrag where
    QueryFrag q1 p1 <> QueryFrag q2 p2 = QueryFrag (q1 <> q2) (p1 DB.:. p2)

withQueryFrag :: QueryFrag -> (forall a. ToRow a => Query -> a -> b) -> b
withQueryFrag (QueryFrag q args) f = f q args

queryObjNamesAndHashes :: (HasCallStack, MonadIO m) => DB.Connection -> QueryFrag -> [QueryFrag] -> QueryFrag -> Maybe QueryFrag -> m [(ObjName, ObjHash)]
queryObjNamesAndHashes conn objNameCol hashCols table filterBy = liftIO $ withQueryFrag fullQuery (DB.query conn)
  where
    fullQuery = "SELECT " <> objNameCol <> ", " <> hashProjection hashCols <> " FROM " <> table
                     <> maybe "" (" WHERE " <>) filterBy
                     <> " ORDER BY " <> objNameCol

data DbHashes = DbHashes [SchemaHash] deriving stock Show
data SchemaHash = SchemaHash ObjName ObjHash [SchemaObjectHash] deriving stock Show
-- TODO: VIEWs, Functions, sequences, collations, triggers, FKs, constraints, row level security policies... What else?
data SchemaObjectHash = TableHash ObjName ObjHash [TableColumn] deriving stock Show
data TableColumn = TableColumn ObjName ObjHash deriving stock (Show, Eq)

class IsDbObject a where
    objName :: a -> ObjName
    childrenObjs :: a -> [DbObject]
instance IsDbObject SchemaHash where
    objName (SchemaHash n _ _) = n
    childrenObjs (SchemaHash _ _ cs) = map DbObject cs
instance IsDbObject SchemaObjectHash where
    objName (TableHash n _ _) = n
    childrenObjs (TableHash _ _ cs) = map DbObject cs
instance IsDbObject TableColumn where
    objName (TableColumn n _) = n
    childrenObjs (TableColumn _ _) = []

-- | This existential type helps us recurse over the DB's hierarchy
data DbObject = forall a. IsDbObject a => DbObject a
instance IsDbObject DbObject where
    objName (DbObject obj) = objName obj
    childrenObjs (DbObject obj) = childrenObjs obj

-- Filesystem collation, haskell collation and DB collation for ordering are things we just cannot risk
-- considering are the same, so our Eq instances do sorting by object name in Haskell, always.
-- TODO: Use Maps instead of lists? They guarantee uniqueness and make sorting a non-problem.. sounds nice
instance Eq DbHashes where
    DbHashes schemas1 == DbHashes schemas2 = sortOn objName schemas1 == sortOn objName schemas2
instance Eq SchemaHash where
    SchemaHash n1 h1 objs1 == SchemaHash n2 h2 objs2 = n1 == n2 && h1 == h2 && sortOn objName objs1 == sortOn objName objs2
instance Eq SchemaObjectHash where
    TableHash n1 h1 cols1 == TableHash n2 h2 cols2 = n1 == n2 && h1 == h2 && sortOn objName cols1 == sortOn objName cols2

newtype ObjHash = ObjHash { unObjHash :: Text }
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromField)
newtype ObjName = ObjName { unObjName :: Text } 
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromField, ToField)

toFiles :: DbHashes -> [(FilePath, ObjHash)]
toFiles (DbHashes schemas) = concatMap schemaToFiles schemas
    where
        prepend folderName (file, c) = (folderName </> file, c)
        schemaToFiles (SchemaHash schemaName schemaHash objs) = map (prepend (mkPathFrag schemaName)) $ ("objhash", schemaHash) : concatMap objToFiles objs
        objToFiles = \case
            TableHash tableName tableHash cols -> map (prepend ("tables" </> mkPathFrag tableName)) $ ("objhash", tableHash) : map colToFiles cols
        colToFiles (TableColumn colName colHash) = (mkPathFrag colName, colHash)

getDbHashes :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> m DbHashes
getDbHashes conn = do
    -- TODO: Ignore Pg's schemas!
    schemas :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "schema_name" ["schema_owner", "default_character_set_catalog", "default_character_set_schema", "default_character_set_name" ] "information_schema.schemata" Nothing
    DbHashes <$> getSchemaHash conn schemas

getSchemaHash :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> [(ObjName, ObjHash)] -> m [SchemaHash]
getSchemaHash conn schemas = forM schemas $ \(schemaName, schemaHash) -> do
    -- TODO: do it all in a single query? Probably not worth it..
    -- 1. Tables
    tables :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "table_name" ["table_type", "self_referencing_column_name", "reference_generation", "is_insertable_into", "is_typed", "commit_action"] "information_schema.tables" (Just $ QueryFrag "table_schema=?" (DB.Only schemaName))
    SchemaHash schemaName schemaHash <$> getTablesHashes conn schemaName tables

getTablesHashes :: (MonadUnliftIO m, MonadIO m, HasCallStack) => DB.Connection -> ObjName -> [(ObjName, ObjHash)] -> m [SchemaObjectHash]
getTablesHashes conn schemaName tables = forM tables $ \(tableName, tableHash) -> do
    columns :: [(ObjName, ObjHash)] <- queryObjNamesAndHashes conn "column_name" ["table_schema", "table_name", "column_default", "is_nullable", "data_type", "character_maximum_length", "character_octet_length", "numeric_precision", "numeric_precision_radix", "numeric_scale", "datetime_precision", "interval_type", "interval_precision", "collation_name", "is_identity", "identity_generation", "identity_start", "identity_increment", "identity_maximum", "identity_minimum", "identity_cycle", "is_generated", "generation_expression", "is_updatable"] "information_schema.columns" (Just $ QueryFrag "table_schema=? AND table_name=?" (schemaName, tableName))
    pure $ TableHash tableName tableHash $ map (uncurry TableColumn) columns

-- | TODO: Make sure valid DB characters are replaced by valid on-disk characters when necessary
mkPathFrag :: ObjName -> FilePath
mkPathFrag (ObjName n) = Text.unpack n

fromPathFrag :: FilePath -> ObjName
fromPathFrag fp = ObjName $ Text.pack fp

-- | Wipes out completely the supplied folder and writes the hashes of the Database's structures to it again.
persistHashesToDisk :: MonadIO m => DbHashes -> FilePath -> m ()
persistHashesToDisk dbHashes dir = do
    tempDir <- (</> "temp-db-dir") <$> getTemporaryDirectory
    whenM (doesDirectoryExist tempDir) $ removeDirectoryRecursive tempDir
    createDirectoryIfMissing False tempDir
    forM_ (toFiles dbHashes) $ \(filepath, (ObjHash filecontents)) -> do
        createDirectoryIfMissing True (tempDir </> takeDirectory filepath)
        liftIO $ writeFile (tempDir </> filepath) filecontents

    -- If the directory doesn't exist, we should simply ignore it
    whenM (doesDirectoryExist dir) $ removeDirectoryRecursive dir
    -- Important: renameDirectory will fail when the destination is a different partition. So we make a Copy instead.
    copyDir tempDir dir

-- | TODO: Do we allow callers to choose to ignore extra files?
readHashesFromDisk :: MonadIO m => FilePath -> m (Either Text DbHashes)
readHashesFromDisk dir = do
    schemaFolders <- listDirectory dir
    schemaHashesE <- runExceptT $ forM schemaFolders $
        \schemaFolder -> readObjWithObjHashFile (dir </> schemaFolder) SchemaHash $ \foldersInSchema -> do
            fmap mconcat $ forM foldersInSchema $ \folderInSchema -> do
                objFolders <- listDirectory (dir </> schemaFolder </> folderInSchema)
                case folderInSchema of
                    "tables" ->
                        forM objFolders $ \tableFolder -> readObjWithObjHashFile (dir </> schemaFolder </> folderInSchema </> tableFolder) TableHash $ \colNames -> forM colNames $ \colName -> do
                            colHash <- liftIO $ readFile (dir </> schemaFolder </> folderInSchema </> tableFolder </> colName)
                            pure $ TableColumn (fromPathFrag colName) (ObjHash colHash)

                    _ -> throwError "Invalid folder under schemas"

    return $ DbHashes <$> schemaHashesE

    where
        readObjWithObjHashFile :: (MonadError Text m, MonadIO m) => FilePath -> (ObjName -> ObjHash -> b -> a) -> ([FilePath] -> m b) -> m a
        readObjWithObjHashFile objNameFolder f fsub = do
            unlessM (doesDirectoryExist objNameFolder) $ throwError $ "There's a file called '" <> pack objNameFolder <> "' but a folder was expected"

            let objhashfile = objNameFolder </> "objhash"
            unlessM (doesFileExist objhashfile) $ throwError $ "Missing file '" <> pack objhashfile <> "'"
            objHash <- liftIO $ readFile objhashfile
            foldersAndFiles <- filter (/= "objhash") <$> listDirectory objNameFolder
            f (fromPathFrag (takeFileName objNameFolder)) (ObjHash objHash) <$> fsub foldersAndFiles


-- | Taken from https://stackoverflow.com/questions/6807025/what-is-the-haskell-way-to-copy-a-directory and modified
copyDir :: MonadIO m => FilePath -> FilePath -> m ()
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

doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
orM xs = or <$> sequence xs
whenM s r = s >>= flip when r
unlessM s r = s >>= flip unless r