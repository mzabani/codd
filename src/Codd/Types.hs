module Codd.Types (SqlMigration(..), SqlFilePath(..), DbVcsInfo(..), ApplyMigrations(..)) where

import Codd.Hashing.Types (DbHashes)
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as DB

newtype SqlFilePath = SqlFilePath { unSqlFilePath :: FilePath } deriving newtype (Eq, Ord, Show)

data DbVcsInfo = DbVcsInfo {
    dbName :: Text
    -- ^ The name of the Database the Application will connect to
    , appUser :: Text
    -- ^ The name of the User which will be created and authorized to access any assets created for the App's database.
    --   This is usually the App's User.
    , superUserConnString :: DB.ConnectInfo
    -- ^ A Connection String which has the power to create databases, grant privileges and a lot more.
    , sqlMigrations :: Either [FilePath] [SqlMigration]
    -- ^ A list of directories with .sql files or a list of ordered Sql Migrations.
    --   When using Directory Paths, all .sql files from all directories are collected into a single list and then run in alphabetical order. Files whose names don't end in .sql are ignored.
    , onDiskHashes :: Either FilePath DbHashes
    -- ^ The directory where DB hashes are persisted to when SQL migrations are applied. In a valid setup, this should always match the Hashes obtained from the Database,
    -- (perhaps only after applying migrations when deploying).
}

data ApplyMigrations = OnlyNonDestructive | IncludeDestructiveOfAlreadyRun | BothNonAndDestructive

data SqlMigration = SqlMigration {
    migrationName :: FilePath
    , nonDestructiveSql :: Maybe Text
    , nonDestructiveForce :: Bool
    , nonDestructiveInTxn :: Bool
    , destructiveSql :: Maybe Text
    , destructiveInTxn :: Bool
} deriving stock (Eq, Show)