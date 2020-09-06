module Codd.Hashing.Types where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import System.FilePath ((</>))

data DbHashes = DbHashes [SchemaHash] deriving stock Show
data SchemaHash = SchemaHash ObjName ObjHash [SchemaObjectHash] deriving stock Show
-- TODO: Functions, orphaned sequences, collations, triggers, row level security policies... What else?
data SchemaObjectHash = TableHash ObjName ObjHash [TableColumn] [TableConstraint] | ViewHash ObjName ObjHash | RoutineHash ObjName ObjHash | SequenceHash ObjName ObjHash deriving stock Show
data TableColumn = TableColumn ObjName ObjHash deriving stock (Show, Eq)
data TableConstraint = TableConstraint ObjName ObjHash deriving stock (Show, Eq)

class IsDbObject a where
    objName :: a -> ObjName
    objHash :: a -> ObjHash
    hashFileRelativeToParent :: a -> FilePath
    childrenObjs :: a -> [DbObject]

instance IsDbObject SchemaHash where
    objName (SchemaHash n _ _) = n
    objHash (SchemaHash _ h _) = h
    hashFileRelativeToParent (SchemaHash n _ _ ) = mkPathFrag n </> "objhash"
    childrenObjs (SchemaHash _ _ cs) = map DbObject cs

instance IsDbObject SchemaObjectHash where
    objName =
        \case
            TableHash n _ _ _ -> n
            ViewHash n _ -> n
            RoutineHash n _ -> n
            SequenceHash n _ -> n
    objHash =
        \case
            TableHash _ h _ _ -> h
            ViewHash _ h -> h
            RoutineHash _ h -> h
            SequenceHash _ h -> h
    hashFileRelativeToParent =
        \case
            TableHash n _ _ _ -> "tables" </> mkPathFrag n </> "objhash"
            ViewHash n _ -> "views" </> mkPathFrag n
            RoutineHash n _ -> "routines" </> mkPathFrag n
            SequenceHash n _ -> "sequences" </> mkPathFrag n
    childrenObjs =
        \case
            TableHash _ _ cols cks -> map DbObject cols ++ map DbObject cks
            ViewHash _ _ -> []
            RoutineHash _ _ -> []
            SequenceHash {} -> []

instance IsDbObject TableColumn where
    objName (TableColumn n _) = n
    objHash (TableColumn _ h) = h
    hashFileRelativeToParent (TableColumn n _) = "cols" </> mkPathFrag n
    childrenObjs (TableColumn _ _) = []
    
instance IsDbObject TableConstraint where
    objName (TableConstraint n _) = n
    objHash (TableConstraint _ h) = h
    hashFileRelativeToParent (TableConstraint n _) = "constraints" </> mkPathFrag n
    childrenObjs (TableConstraint _ _) = []

-- | This existential type helps us recurse over the DB's hierarchy
data DbObject = forall a. IsDbObject a => DbObject a
instance IsDbObject DbObject where
    objName (DbObject obj) = objName obj
    objHash (DbObject obj) = objHash obj
    hashFileRelativeToParent (DbObject obj) = hashFileRelativeToParent obj
    childrenObjs (DbObject obj) = childrenObjs obj

-- | TODO: Make sure valid DB characters are replaced by valid on-disk characters when necessary
mkPathFrag :: ObjName -> FilePath
mkPathFrag (ObjName n) = Text.unpack n

fromPathFrag :: FilePath -> ObjName
fromPathFrag fp = ObjName $ Text.pack fp

-- Filesystem collation, haskell collation and DB collation for ordering are things we just cannot risk
-- considering are the same, so our Eq instances do sorting by object name in Haskell, always.
-- TODO: Use Maps instead of lists? They guarantee uniqueness and make sorting a non-problem.. sounds nice
instance Eq DbHashes where
    DbHashes schemas1 == DbHashes schemas2 = sortOn objName schemas1 == sortOn objName schemas2
instance Eq SchemaHash where
    -- TODO: Objects of different types with the same name!! Is that possible??
    SchemaHash n1 h1 objs1 == SchemaHash n2 h2 objs2 = n1 == n2 && h1 == h2 && sortOn objName objs1 == sortOn objName objs2
instance Eq SchemaObjectHash where
    TableHash n1 h1 cols1 cks1 == TableHash n2 h2 cols2 cks2 = n1 == n2 && h1 == h2 && sortOn objName cols1 == sortOn objName cols2 && sortOn objName cks1 == sortOn objName cks2
    ViewHash n1 h1 == ViewHash n2 h2 = n1 == n2 && h1 == h2
    RoutineHash n1 h1 == RoutineHash n2 h2 = n1 == n2 && h1 == h2
    SequenceHash n1 h1 == SequenceHash n2 h2 = n1 == n2 && h1 == h2
    _ == _ = False

newtype ObjHash = ObjHash { unObjHash :: Text }
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromField)
newtype ObjName = ObjName { unObjName :: Text } 
    deriving stock (Eq, Ord, Show)
    deriving newtype (FromField, ToField)