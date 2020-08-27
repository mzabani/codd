module Main where

import qualified Codd.Hashing as Codd
import qualified Database.PostgreSQL.Simple as DB

main :: IO ()
main = do
    let dbCfg = DB.defaultConnectInfo
    hashes <- Codd.getDbHashes dbCfg
    Codd.persistHashesToDisk hashes "local/db-schema"