module Codd.Types (SqlMigration(..)) where

import Data.Text (Text)

data SqlMigration = SqlMigration {
    nonDestructiveSql :: Text
    , nonDestructiveForce :: Bool
    , nonDestructiveInTxn :: Bool
    , destructiveSql :: Text
    , destructiveInTxn :: Bool
} deriving stock (Eq, Show)