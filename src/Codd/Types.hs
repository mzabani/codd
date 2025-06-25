module Codd.Types
  ( SqlFilePath (..),
    SchemaAlgo (..),
    SqlRole (..),
    PgMajorVersion (..),
    SqlSchema (..),
    ConnectionString (..),
    RetryPolicy (..),
    RetryBackoffPolicy (..),
    SchemaSelection (..),
    TxnIsolationLvl (..),
    defaultRetryPolicy,
    retryPolicyIterate,
    singleTryPolicy,
    libpqConnString,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time
  ( DiffTime,
    secondsToDiffTime,
  )
import Data.Word (Word16)
import Database.PostgreSQL.Simple.ToField
  ( ToField,
  )

newtype PgMajorVersion = PgMajorVersion Int
  deriving newtype (Eq, Ord)

instance Show PgMajorVersion where
  show (PgMajorVersion v) = 'v' : show v

newtype SqlFilePath = SqlFilePath {unSqlFilePath :: FilePath} deriving newtype (Eq, Ord, Show)

data RetryBackoffPolicy = ExponentialBackoff DiffTime | ConstantBackoff DiffTime deriving stock (Show, Eq)

data RetryPolicy = RetryPolicy Int RetryBackoffPolicy
  deriving stock (Show, Eq)

data TxnIsolationLvl = DbDefault | Serializable | RepeatableRead | ReadCommitted | ReadUncommitted deriving stock (Show, Eq)

data ConnectionString = ConnectionString
  { hostname :: String,
    port :: Word16,
    user :: String,
    password :: String,
    database :: String,
    options :: Maybe String
  }
  deriving stock (Eq)

libpqConnString :: ConnectionString -> ByteString
libpqConnString ConnectionString {..} =
  ByteString.intercalate " " $
    map (\(kw, v) -> kw <> "=" <> v) mixedKwvps
  where
    mixedKwvps =
      catMaybes
        [ Just ("user", quote user),
          Just
            ("host", quote hostname),
          Just
            ("dbname", quote database),
          Just ("password", quote password),
          Just ("port", quote (show port)),
          options >>= (Just . ("options",)) . quote
        ]
    quote (Text.pack -> un) =
      encodeUtf8 $
        "'"
          <> Text.replace "'" "\\'" (Text.replace "\\" "\\\\" un)
          <> "'"

instance Show ConnectionString where
  show _ = "ConnectionString"

data SchemaAlgo = SchemaAlgo
  { strictCollations :: Bool,
    strictRangeCtorPrivs :: Bool,
    ignoreColumnOrder :: Bool
  }

-- | A Retry Policy that tries at most 3 times (at most 2 retries) with an exponential backoff with
-- base time of 1 second. It needs not be a reasonable policy for any workload,
-- it just needs not be absurd.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy 2 (ExponentialBackoff $ secondsToDiffTime 1)

-- | A Policy that only tries once - never retries.
singleTryPolicy :: RetryPolicy
singleTryPolicy = RetryPolicy 0 (ConstantBackoff 0)

-- | When trying an action that fails, this returns how much time to wait until the next try,
-- or Nothing if there are no more retries.
-- It also returns an updated RetryPolicy for the next attempt.
retryPolicyIterate :: RetryPolicy -> Maybe (DiffTime, RetryPolicy)
retryPolicyIterate (RetryPolicy maxRetries backoff)
  | maxRetries <= 0 = Nothing
  | otherwise = case backoff of
      ConstantBackoff t ->
        Just (t, RetryPolicy (maxRetries - 1) (ConstantBackoff t))
      ExponentialBackoff t ->
        Just (t, RetryPolicy (maxRetries - 1) (ExponentialBackoff (t * 2)))

newtype SqlRole = SqlRole {unSqlRole :: Text} deriving newtype (Show, ToField, IsString)

newtype SqlSchema = SqlSchema {unSqlSchema :: Text} deriving newtype (Show, ToField, IsString)

data SchemaSelection = IncludeSchemas [SqlSchema] | AllNonInternalSchemas
