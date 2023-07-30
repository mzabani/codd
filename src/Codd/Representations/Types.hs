{-# LANGUAGE CPP #-}
module Codd.Representations.Types
    ( ObjectRep(..)
    , ObjName(..)
    , HasName(..)
    , DiffType(..)
    , DbRep(..)
    , SchemaRep(..)
    , TableRep(..)
    , RoleRep(..)
    , ViewRep(..)
    , RoutineRep(..)
    , SequenceRep(..)
    , CollationRep(..)
    , TableColumnRep(..)
    , TableConstraintRep(..)
    , TableIndexRep(..)
    , TablePolicyRep(..)
    , TableTriggerRep(..)
    , TypeRep(..)
    , detEncodeJSON
    , fromPathFrag
    , listToMap
    , mkPathFrag
    ) where

import           Data.Aeson                     ( FromJSON
                                                , FromJSONKey
                                                , ToJSON(..)
                                                , ToJSONKey
                                                , Value(..)
                                                )
import           Data.Aeson.Encoding            ( encodingToLazyByteString )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Hashable                  ( Hashable )
import qualified Data.Map                      as Map
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
#if !MIN_VERSION_aeson(2,0,0)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Vector                   as Vector
#endif
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField )
import           GHC.Generics                   ( Generic )

data ObjectRep = HDatabaseSettings | HSchema | HTable | HView | HRoutine | HColumn | HIndex | HTableConstraint | HTrigger | HRole | HSequence | HPolicy | HCollation | HType
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

#if MIN_VERSION_aeson(2,0,0)
-- | Deterministic `ToJSON` instances, i.e. instances whose implementation of `toEncoding`
-- produces the same result regardles of hashable's initial seed.
-- We should be able to remove this after we drop support for aeson-1.
detEncodeJSON :: ToJSON a => a -> Text
detEncodeJSON =
    decodeUtf8
        . toStrict
        . encodingToLazyByteString
        . toEncoding
#else

-- | Deterministic `ToJSON` instances, i.e. instances whose implementation of `toEncoding`
-- produces the same result regardles of hashable's initial seed.
-- We should be able to remove this after we drop support for aeson-1.
detEncodeJSON :: ToJSON a => a -> Text
detEncodeJSON =
    decodeUtf8
        . toStrict
        . encodingToLazyByteString
        . toEncoding
        . Json
        . toJSON


newtype Json = Json Value
instance ToJSON Json where
    toJSON (Json v) = v
    toEncoding (Json val) = case val of
        Object m   -> toEncoding $ fmap Json $ Map.fromList $ HM.toList m
        Array  arr -> toEncoding $ Json <$> Vector.toList arr
        _          -> toEncoding val
#endif

data DbRep = DbRep Value (Map ObjName SchemaRep) (Map ObjName RoleRep)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data SchemaRep = SchemaRep ObjName
                           Value
                           (Map ObjName TableRep)
                           (Map ObjName ViewRep)
                           (Map ObjName RoutineRep)
                           (Map ObjName SequenceRep)
                           (Map ObjName CollationRep)
                           (Map ObjName TypeRep)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TableRep = TableRep ObjName
                         Value
                         (Map ObjName TableColumnRep)
                         (Map ObjName TableConstraintRep)
                         (Map ObjName TableTriggerRep)
                         (Map ObjName TablePolicyRep)
                         (Map ObjName TableIndexRep)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ViewRep = ViewRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data RoutineRep = RoutineRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data SequenceRep = SequenceRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data CollationRep = CollationRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableColumnRep = TableColumnRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableConstraintRep = TableConstraintRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableTriggerRep = TableTriggerRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TablePolicyRep = TablePolicyRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TableIndexRep = TableIndexRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data RoleRep = RoleRep ObjName Value
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
data TypeRep = TypeRep ObjName Value
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

instance HasName RoleRep where
    objName (RoleRep n _) = n
instance HasName SchemaRep where
    objName (SchemaRep n _ _ _ _ _ _ _) = n

instance HasName TableRep where
    objName (TableRep n _ _ _ _ _ _) = n

instance HasName TableColumnRep where
    objName (TableColumnRep n _) = n
instance HasName TableConstraintRep where
    objName (TableConstraintRep n _) = n
instance HasName TableTriggerRep where
    objName (TableTriggerRep n _) = n
instance HasName TablePolicyRep where
    objName (TablePolicyRep n _) = n
instance HasName TableIndexRep where
    objName (TableIndexRep n _) = n
instance HasName ViewRep where
    objName (ViewRep n _) = n
instance HasName RoutineRep where
    objName (RoutineRep n _) = n
instance HasName SequenceRep where
    objName (SequenceRep n _) = n
instance HasName CollationRep where
    objName (CollationRep n _) = n

instance HasName TypeRep where
    objName (TypeRep n _) = n

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
            toJSON ("different-schemas" :: Text, foundInDB)
