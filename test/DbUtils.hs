module DbUtils where

import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)

testConnInfo :: ConnectInfo
testConnInfo = defaultConnectInfo { connectHost = "localhost", connectUser = "postgres", connectDatabase = "postgres", connectPort = 5433 }