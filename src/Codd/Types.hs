module Codd.Types (SqlMigration(..), DbVcsInfo(..), ApplyMigrations(..)) where

import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as DB

data DbVcsInfo = DbVcsInfo {
    dbName :: String
    -- ^ The name of the Database the Application will connect to
    , appUser :: String
    -- ^ The name of the User which will be created and authorized to access any assets created for the App's database.
    --   This is usually the App's User.
    , superUserConnString :: DB.ConnectInfo
    -- ^ A Connection String which has the power to create databases, grant privileges and a lot more.
    , sqlMigrations :: Either [FilePath] [SqlMigration]
    -- ^ A list of directories with .sql files or a list of ordered Sql Migrations.
    --   When using Directory Paths, all .sql files from all directories are collected into a single list and then run in alphabetical order. Files whose names don't end in .sql are ignored.
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