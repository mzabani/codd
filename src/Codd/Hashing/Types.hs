module Codd.Hashing.Types where

import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON(..)
                                                , ToJSONKey
                                                , Value(String)
                                                )
import           Data.Hashable                  ( Hashable )
import qualified Data.Map                      as Map
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField )
import           GHC.Generics                   ( Generic )

data HashableObject = HDatabaseSettings | HSchema | HTable | HView | HRoutine | HColumn | HIndex | HTableConstraint | HTrigger | HRole | HSequence | HPolicy | HCollation | HType
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
                             (Map ObjName TypeHash)
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
data TypeHash = TypeHash ObjName ObjHash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | TODO: Make sure valid DB characters are replaced by valid on-disk characters when necessary
mkPathFrag :: ObjName -> FilePath
mkPathFrag (ObjName n) = Text.unpack n

fromPathFrag :: FilePath -> ObjName
fromPathFrag fp = ObjName $ Text.pack fp

newtype ObjHash = ObjHash { unObjHash :: Text }
    deriving newtype (FromField, Eq, Ord, Show, Hashable, FromJSON, ToJSON)
newtype ObjName = ObjName { unObjName :: Text }
    deriving newtype (FromField, ToField, Eq, Ord, Show, Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)


class HasName a where
    objName :: a -> ObjName

instance HasName RoleHash where
    objName (RoleHash n _) = n
instance HasName SchemaHash where
    objName (SchemaHash n _ _ _ _ _ _ _) = n

instance HasName TableHash where
    objName (TableHash n _ _ _ _ _ _) = n

instance HasName TableColumn where
    objName (TableColumn n _) = n
instance HasName TableConstraint where
    objName (TableConstraint n _) = n
instance HasName TableTrigger where
    objName (TableTrigger n _) = n
instance HasName TablePolicy where
    objName (TablePolicy n _) = n
instance HasName TableIndex where
    objName (TableIndex n _) = n
instance HasName ViewHash where
    objName (ViewHash n _) = n
instance HasName RoutineHash where
    objName (RoutineHash n _) = n
instance HasName SequenceHash where
    objName (SequenceHash n _) = n
instance HasName CollationHash where
    objName (CollationHash n _) = n

instance HasName TypeHash where
    objName (TypeHash n _) = n

listToMap :: HasName a => [a] -> Map ObjName a
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
