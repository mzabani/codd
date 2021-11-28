module Codd.Types
    ( SqlFilePath(..)
    , ChecksumAlgo(..)
    , DeploymentWorkflow(..)
    , SqlRole(..)
    , SqlSchema(..)
    , Include(..)
    , RetryPolicy(..)
    , RetryBackoffPolicy(..)
    , TxnIsolationLvl(..)
    , alsoInclude
    , defaultRetryPolicy
    , retryPolicyIterate
    , singleTryPolicy
    ) where

import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           Data.Time                      ( NominalDiffTime
                                                , secondsToNominalDiffTime
                                                )
import           Database.PostgreSQL.Simple.Time
                                                ( UTCTimestamp )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField )

newtype SqlFilePath = SqlFilePath { unSqlFilePath :: FilePath } deriving newtype (Eq, Ord, Show)

data DeploymentWorkflow = SimpleDeployment | BlueGreenSafeDeploymentUpToAndIncluding UTCTimestamp deriving stock Show

data RetryBackoffPolicy = ExponentialBackoff NominalDiffTime | ConstantBackoff NominalDiffTime deriving stock (Show, Eq)
data RetryPolicy = RetryPolicy Int RetryBackoffPolicy
    deriving stock (Show, Eq)

data TxnIsolationLvl = DbDefault | Serializable | RepeatableRead | ReadCommitted | ReadUncommitted deriving stock (Show, Eq)

data ChecksumAlgo = StrictCollations | LaxCollations

-- | A Retry Policy that tries at most 3 times (at most 2 retries) with an exponential backoff with
-- base time of 1 second. It needs not be a reasonable policy for any workload,
-- it just needs not be absurd.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
    RetryPolicy 2 (ExponentialBackoff $ secondsToNominalDiffTime 1)

-- | A Policy that only tries once - never retries.
singleTryPolicy :: RetryPolicy
singleTryPolicy = RetryPolicy 0 (ConstantBackoff 0)

-- | When trying an action that fails, this returns how much time to wait until the next try,
-- or Nothing if there are no more retries.
-- It also returns an updated RetryPolicy for the next attempt.
retryPolicyIterate :: RetryPolicy -> Maybe (NominalDiffTime, RetryPolicy)
retryPolicyIterate (RetryPolicy maxRetries backoff)
    | maxRetries <= 0 = Nothing
    | otherwise = case backoff of
        ConstantBackoff t ->
            Just (t, RetryPolicy (maxRetries - 1) (ConstantBackoff t))
        ExponentialBackoff t ->
            Just (t, RetryPolicy (maxRetries - 1) (ExponentialBackoff (t * 2)))

newtype SqlRole = SqlRole { unSqlRole :: Text } deriving newtype (Show, ToField, IsString)
newtype SqlSchema = SqlSchema { unSqlSchema :: Text } deriving newtype (Show, ToField, IsString)

data Include a = Include [a] | Exclude [a] | IncludeExclude [a] [a]
alsoInclude :: [a] -> Include a -> Include a
alsoInclude is = \case
    Include xs           -> Include (is ++ xs)
    Exclude xs           -> IncludeExclude is xs
    IncludeExclude as xs -> IncludeExclude (as ++ is) xs
