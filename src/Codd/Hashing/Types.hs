module Codd.Hashing.Types where

import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import System.FilePath ((</>))

data HashableObject = HSchema | HTable | HView | HRoutine | HColumn | HIndex | HTableConstraint | HTrigger | HRole | HSequence | HPolicy
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

data DbHashes = DbHashes (Map ObjName SchemaHash) (Map ObjName RoleHash) deriving stock (Show, Eq)
data SchemaHash = SchemaHash ObjName ObjHash (Map ObjName SchemaObjectHash) deriving stock (Show, Eq)
-- TODO: schema search path, collations, Full Text dictionaries, operators, permissions per table, per column... What else?
data SchemaObjectHash = TableHash ObjName ObjHash (Map ObjName TableColumn) (Map ObjName TableConstraint) (Map ObjName TableTrigger) (Map ObjName TablePolicy) (Map ObjName TableIndex) | ViewHash ObjName ObjHash | RoutineHash ObjName ObjHash | SequenceHash ObjName ObjHash deriving stock (Show, Eq)
data TableColumn = TableColumn ObjName ObjHash deriving stock (Show, Eq)
data TableConstraint = TableConstraint ObjName ObjHash deriving stock (Show, Eq)
data TableTrigger = TableTrigger ObjName ObjHash deriving stock (Show, Eq)
data TablePolicy = TablePolicy ObjName ObjHash deriving stock (Show, Eq)
data TableIndex = TableIndex ObjName ObjHash deriving stock (Show, Eq)
data RoleHash = RoleHash ObjName ObjHash deriving stock (Show, Eq)

class IsDbObject a where
    objName :: a -> ObjName
    objHash :: a -> ObjHash
    hashFileRelativeToParent :: a -> FilePath
    childrenObjs :: a -> [DbObject]

instance IsDbObject SchemaHash where
    objName (SchemaHash n _ _) = n
    objHash (SchemaHash _ h _) = h
    hashFileRelativeToParent (SchemaHash n _ _ ) = "schemas" </> mkPathFrag n </> "objhash"
    childrenObjs (SchemaHash _ _ cs) = map DbObject $ Map.elems cs

instance IsDbObject RoleHash where
    objName (RoleHash n _) = n
    objHash (RoleHash _ h) = h
    hashFileRelativeToParent (RoleHash n _) = "roles" </> mkPathFrag n
    childrenObjs (RoleHash _ _) = []

instance IsDbObject SchemaObjectHash where
    objName =
        \case
            TableHash n _ _ _ _ _ _ -> n
            ViewHash n _ -> n
            RoutineHash n _ -> n
            SequenceHash n _ -> n
    objHash =
        \case
            TableHash _ h _ _ _ _ _ -> h
            ViewHash _ h -> h
            RoutineHash _ h -> h
            SequenceHash _ h -> h
    hashFileRelativeToParent =
        \case
            TableHash n _ _ _ _ _ _ -> "tables" </> mkPathFrag n </> "objhash"
            ViewHash n _ -> "views" </> mkPathFrag n
            RoutineHash n _ -> "routines" </> mkPathFrag n
            SequenceHash n _ -> "sequences" </> mkPathFrag n
    childrenObjs =
        \case
            TableHash _ _ cols cks triggers pols idxs -> map DbObject (Map.elems cols) ++ map DbObject (Map.elems cks) ++ map DbObject (Map.elems triggers) ++ map DbObject (Map.elems pols) ++ map DbObject (Map.elems idxs)
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

instance IsDbObject TableTrigger where
    objName (TableTrigger n _) = n
    objHash (TableTrigger _ h) = h
    hashFileRelativeToParent (TableTrigger n _) = "triggers" </> mkPathFrag n
    childrenObjs (TableTrigger _ _) = []

instance IsDbObject TablePolicy where
    objName (TablePolicy n _) = n
    objHash (TablePolicy _ h) = h
    hashFileRelativeToParent (TablePolicy n _) = "policies" </> mkPathFrag n
    childrenObjs (TablePolicy _ _) = []

instance IsDbObject TableIndex where
    objName (TableIndex n _) = n
    objHash (TableIndex _ h) = h
    hashFileRelativeToParent (TableIndex n _) = "indexes" </> mkPathFrag n
    childrenObjs _ = []

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

newtype ObjHash = ObjHash { unObjHash :: Text }
    deriving newtype (FromField, Eq, Ord, Show, Hashable)
newtype ObjName = ObjName { unObjName :: Text }
    deriving newtype (FromField, ToField, Eq, Ord, Show, Hashable)

listToMap :: IsDbObject o => [o] -> Map ObjName o
listToMap = Map.fromList . map (\obj -> (objName obj, obj))