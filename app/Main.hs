module Main where


import qualified Codd.Hashing as Codd
import qualified Codd.Internal as Codd
import qualified Database.PostgreSQL.Simple as DB

main :: IO ()
main = do
    let dbCfg = DB.defaultConnectInfo { DB.connectDatabase = "codd" }
    hashes <- Codd.connectAndDispose dbCfg Codd.getDbHashes
    Codd.persistHashesToDisk hashes "local/db-schema"