module Codd.Types (SqlFilePath(..), DeploymentWorkflow(..), SqlRole(..), SqlSchema(..), Include(..), alsoInclude) where

import Data.String (IsString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Time (UTCTimestamp)

newtype SqlFilePath = SqlFilePath { unSqlFilePath :: FilePath } deriving newtype (Eq, Ord, Show)

data DeploymentWorkflow = SimpleDeployment | BlueGreenSafeDeploymentUpToAndIncluding UTCTimestamp deriving stock Show

newtype SqlRole = SqlRole { unSqlRole :: Text } deriving newtype (Show, ToField, IsString)
newtype SqlSchema = SqlSchema { unSqlSchema :: Text } deriving newtype (Show, ToField, IsString)

data Include a = Include [a] | Exclude [a] | IncludeExclude [a] [a]
alsoInclude :: [a] -> Include a -> Include a
alsoInclude is = \case
    Include xs -> Include (is ++ xs)
    Exclude xs -> IncludeExclude is xs
    IncludeExclude as xs -> IncludeExclude (as ++ is) xs