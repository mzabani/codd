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

data DbHashes = DbHashes Value (Map ObjName SchemaHash) (Map ObjName RoleHash)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data SchemaHash = SchemaHash ObjName
                             Value
                             (Map ObjName TableHash)
                             (Map ObjName ViewHash)
                             (Map ObjName RoutineHash)
                             (Map ObjName SequenceHash)
                             (Map ObjName CollationHash)
                             (Map ObjName TypeHash)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TableHash = TableHash ObjName
                           Value
                           (Map ObjName TableColumn)
                           (Map ObjName TableConstraint)
                           (Map ObjName TableTrigger)
                           (Map ObjName TablePolicy)
                           (Map ObjName TableIndex)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ViewHash = ViewHash ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data RoutineHash = RoutineHash ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data SequenceHash = SequenceHash ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data CollationHash = CollationHash ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableColumn = TableColumn ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableConstraint = TableConstraint ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableTrigger = TableTrigger ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TablePolicy = TablePolicy ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableIndex = TableIndex ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data RoleHash = RoleHash ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TypeHash = TypeHash ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | TODO: Make sure valid DB characters are replaced by valid on-disk characters when necessary
mkPathFrag :: ObjName -> FilePath
mkPathFrag (ObjName n) = Text.unpack n

fromPathFrag :: FilePath -> ObjName
fromPathFrag fp = ObjName $ Text.pack fp

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

data DiffType = ExpectedButNotFound | NotExpectedButFound Value | BothButDifferent Value
    deriving stock (Eq, Show)

instance ToJSON DiffType where
    toJSON v = case v of
        ExpectedButNotFound -> String "expected-but-not-found"
        NotExpectedButFound foundInDB ->
            toJSON ("not-expected-but-found" :: Text, foundInDB)
        BothButDifferent foundInDB ->
            toJSON ("different-checksums" :: Text, foundInDB)

data HashDiff = HashDiff
    { objectName :: FilePath
    , objectDiff :: DiffType
    }
    deriving stock (Eq, Show)
