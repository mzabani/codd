module Codd.Types (SqlMigration(..), AddedSqlMigration(..), SqlFilePath(..), CoddSettings(..), DeploymentWorkflow(..), SqlRole(..), SqlSchema(..), Include(..), alsoInclude) where

import Codd.Hashing.Types (DbHashes)
import Data.String (IsString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Time (UTCTimestamp)
import qualified Database.PostgreSQL.Simple as DB

newtype SqlFilePath = SqlFilePath { unSqlFilePath :: FilePath } deriving newtype (Eq, Ord, Show)

data DeploymentWorkflow = SimpleDeployment | BlueGreenSafeDeploymentUpToAndIncluding UTCTimestamp

newtype SqlRole = SqlRole { unSqlRole :: Text } deriving newtype (Show, ToField, IsString)
newtype SqlSchema = SqlSchema { unSqlSchema :: Text } deriving newtype (Show, ToField, IsString)

data Include a = Include [a] | Exclude [a] | IncludeExclude [a] [a]
alsoInclude :: [a] -> Include a -> Include a
alsoInclude is = \case
    Include xs -> Include (is ++ xs)
    Exclude xs -> IncludeExclude is xs
    IncludeExclude as xs -> IncludeExclude (as ++ is) xs

data CoddSettings = CoddSettings {
    dbName :: Text
    -- ^ The name of the Database the Application will connect to
    , appUser :: SqlRole
    -- ^ The name of the User which will be created and authorized to access any assets created for the App's database.
    --   This is usually the App's User.
    , superUserConnString :: DB.ConnectInfo
    -- ^ A Connection String which has the power to create databases, grant privileges and a lot more.
    , sqlMigrations :: Either [FilePath] [AddedSqlMigration]
    -- ^ A list of directories with .sql files or a list of ordered Sql Migrations.
    --   When using Directory Paths, all .sql files from all directories are collected into a single list and then run in alphabetical order. Files whose names don't end in .sql are ignored.
    , onDiskHashes :: Either FilePath DbHashes
    -- ^ The directory where DB hashes are persisted to when SQL migrations are applied. In a valid setup, this should always match the Hashes obtained from the Database,
    -- (perhaps only after applying migrations when deploying).
    , deploymentWorkflow :: DeploymentWorkflow
    -- ^ Simple or Blue-Green-Safe deployment workflow? Simple means no destructive sections are allowed for any migrations and Blue-Green-Safe means
    -- developers have to keep a file which points to the timestamp of the last migration up to which and including it destructive sections must run the next time.
    , schemasToHash :: Include SqlSchema
    -- ^ Selection of Schemas in the DB that we should hash.
    , extraRolesToHash :: Include SqlRole
    -- ^ Selection of Roles to hash. Note that the appUser and the super user from superUserConnString are always included.
}

data SqlMigration = SqlMigration {
    migrationName :: FilePath
    , nonDestructiveSql :: Maybe Text
    , nonDestructiveForce :: Bool
    , nonDestructiveInTxn :: Bool
    , destructiveSql :: Maybe Text
    , destructiveInTxn :: Bool
} deriving stock (Eq, Show)

data AddedSqlMigration = AddedSqlMigration { addedSqlMig :: SqlMigration, addedSqlTimestamp :: UTCTimestamp }