module Codd.Types
  ( SqlFilePath (..),
    SchemaAlgo (..),
    SqlRole (..),
    PgMajorVersion (..),
    SqlSchema (..),
    RetryPolicy (..),
    RetryBackoffPolicy (..),
    SchemaSelection (..),
    TxnIsolationLvl (..),
    defaultRetryPolicy,
    retryPolicyIterate,
    singleTryPolicy,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Data.Time
  ( DiffTime,
    secondsToDiffTime,
  )
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
