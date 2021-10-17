module Codd.Hashing.Types where

import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON(..)
                                                , ToJSONKey
                                                , Value(String)
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField )
import           GHC.Generics                   ( Generic )
import           System.FilePath                ( (</>) )

data HashableObject = HDatabaseSettings | HSchema | HTable | HView | HRoutine | HColumn | HIndex | HTableConstraint | HTrigger | HRole | HSequence | HPolicy | HCollation
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

data DbHashes = DbHashes ObjHash
                         (Map ObjName SchemaHash)
                         (Map ObjName RoleHash)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data SchemaHash = SchemaHash ObjName
                             ObjHash
                             (Map ObjName TableHash)
                             (Map ObjName ViewHash)
                             (Map ObjName RoutineHash)
                             (Map ObjName SequenceHash)
                             (Map ObjName CollationHash)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TableHash = TableHash ObjName
                           ObjHash
                           (Map ObjName TableColumn)
                           (Map ObjName TableConstraint)
                           (Map ObjName TableTrigger)
                           (Map ObjName TablePolicy)
                           (Map ObjName TableIndex)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ViewHash = ViewHash ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data RoutineHash = RoutineHash ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data SequenceHash = SequenceHash ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data CollationHash = CollationHash ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableColumn = TableColumn ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableConstraint = TableConstraint ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableTrigger = TableTrigger ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TablePolicy = TablePolicy ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableIndex = TableIndex ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data RoleHash = RoleHash ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

class IsDbObject a where
    objName :: a -> ObjName
    objHash :: a -> ObjHash
    hashFileRelativeToParent :: a -> FilePath
    childrenObjs :: a -> [DbObject]

instance IsDbObject SchemaHash where
    objName (SchemaHash n _ _ _ _ _ _) = n
    objHash (SchemaHash _ h _ _ _ _ _) = h
    hashFileRelativeToParent (SchemaHash n _ _ _ _ _ _) =
        "schemas" </> mkPathFrag n </> "objhash"
    childrenObjs (SchemaHash _ _ ts vs rs ss cs) =
        map DbObject (Map.elems ts)
            <> map DbObject (Map.elems vs)
            <> map DbObject (Map.elems rs)
            <> map DbObject (Map.elems ss)
            <> map DbObject (Map.elems cs)

instance IsDbObject RoleHash where
    objName (RoleHash n _) = n
    objHash (RoleHash _ h) = h
    hashFileRelativeToParent (RoleHash n _) = "roles" </> mkPathFrag n
    childrenObjs (RoleHash _ _) = []

instance IsDbObject TableHash where
    objName (TableHash n _ _ _ _ _ _) = n
    objHash (TableHash _ h _ _ _ _ _) = h
    hashFileRelativeToParent (TableHash n _ _ _ _ _ _) =
        "tables" </> mkPathFrag n </> "objhash"
    childrenObjs (TableHash _ _ cols cks triggers pols idxs) =
        map DbObject (Map.elems cols)
            ++ map DbObject (Map.elems cks)
            ++ map DbObject (Map.elems triggers)
            ++ map DbObject (Map.elems pols)
            ++ map DbObject (Map.elems idxs)

instance IsDbObject ViewHash where
    objName (ViewHash n _) = n
    objHash (ViewHash _ h) = h
    hashFileRelativeToParent (ViewHash n _) = "views" </> mkPathFrag n
    childrenObjs (ViewHash _ _) = []

instance IsDbObject RoutineHash where
    objName (RoutineHash n _) = n
    objHash (RoutineHash _ h) = h
    hashFileRelativeToParent (RoutineHash n _) = "routines" </> mkPathFrag n
    childrenObjs (RoutineHash _ _) = []

instance IsDbObject SequenceHash where
    objName (SequenceHash n _) = n
    objHash (SequenceHash _ h) = h
    hashFileRelativeToParent (SequenceHash n _) = "sequences" </> mkPathFrag n
    childrenObjs (SequenceHash _ _) = []

instance IsDbObject CollationHash where
    objName (CollationHash n _) = n
    objHash (CollationHash _ h) = h
    hashFileRelativeToParent (CollationHash n _) =
        "collations" </> mkPathFrag n
    childrenObjs (CollationHash _ _) = []
instance IsDbObject TableColumn where
    objName (TableColumn n _) = n
    objHash (TableColumn _ h) = h
    hashFileRelativeToParent (TableColumn n _) = "cols" </> mkPathFrag n
    childrenObjs (TableColumn _ _) = []

instance IsDbObject TableConstraint where
    objName (TableConstraint n _) = n
    objHash (TableConstraint _ h) = h
    hashFileRelativeToParent (TableConstraint n _) =
        "constraints" </> mkPathFrag n
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
data DbObject = forall a . IsDbObject a => DbObject a
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
    deriving newtype (FromField, Eq, Ord, Show, Hashable, FromJSON, ToJSON)
newtype ObjName = ObjName { unObjName :: Text }
    deriving newtype (FromField, ToField, Eq, Ord, Show, Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

listToMap :: IsDbObject o => [o] -> Map ObjName o
listToMap = Map.fromList . map (\obj -> (objName obj, obj))

data DiffType = OnlyRight | OnlyLeft | BothButDifferent
    deriving stock (Eq, Show)

instance ToJSON DiffType where
    toJSON v = String $ case v of
        OnlyRight        -> "only-right"
        OnlyLeft         -> "only-left"
        BothButDifferent -> "different-checksums"

data HashDiff = HashDiff
    { objectName :: FilePath
    , objectDiff :: DiffType
    }
    deriving stock (Eq, Show)
