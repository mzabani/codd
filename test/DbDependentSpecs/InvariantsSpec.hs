module DbDependentSpecs.InvariantsSpec where

import Codd (withDbAndDrop)
import Codd.Analysis (MigrationCheck(..), NonDestructiveSectionCheck(..), DestructiveSectionCheck(..), checkMigration)
import Codd.Environment (superUserInAppDatabaseConnInfo)
import Codd.Hashing (readHashesFromDatabaseWithSettings)
import Codd.Internal (withConnection)
import Codd.Parsing (toMigrationTimestamp)
import Codd.Types (CoddSettings(..), SqlMigration(..), AddedSqlMigration(..))
import Control.Monad (when, void)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.List (nubBy)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import DbUtils (aroundFreshDatabase, aroundDatabaseWithMigs, getIncreasingTimestamp, aroundTestDbInfo)
import qualified Database.PostgreSQL.Simple as DB
import Database.PostgreSQL.Simple (ConnectInfo(..))
import qualified Database.PostgreSQL.Simple.Time as DB
import Data.Text (unpack)
import Test.Hspec
import UnliftIO.Concurrent (threadDelay)

timestampsMig, lotsOfObjectsMigration :: AddedSqlMigration
timestampsMig = AddedSqlMigration SqlMigration {
                    migrationName = "0000-create-timestamps-table.sql"
                    , nonDestructiveSql = Just "CREATE TABLE timestamps (seq_number int not null, tm1 timestamptz not null, tm2 timestamptz not null, UNIQUE (seq_number), UNIQUE(tm1), UNIQUE(tm2));"
                    , nonDestructiveForce = False
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                } (getIncreasingTimestamp 0)
lotsOfObjectsMigration = AddedSqlMigration SqlMigration {
                    migrationName = "0000-create-lots-of-objects.sql"
                    , nonDestructiveSql = Just $
    "CREATE TABLE anytable (id serial primary key, col1 timestamptz not null, col2 text CHECK (col2 <> ''), UNIQUE (col1), UNIQUE(col1, col2));"
 <> "CREATE EXTENSION intarray; CREATE EXTENSION btree_gist;"
 -- <> "CREATE TABLE other_table (a1 int references timestamps(seq_number), a2 circle, unique(a1, a2), exclude using gist (a1 with =, (a2) with &&));"
 -- <> "CREATE UNIQUE INDEX something_idx ON other_table (a1) WHERE (a1 > 50);"
                    , nonDestructiveForce = False
                    , nonDestructiveInTxn = True
                    , destructiveSql = Nothing
                    , destructiveInTxn = True
                } (getIncreasingTimestamp 0)

shouldBeStrictlySortedOn :: (Show a, Ord b) => [a] -> (a -> b) -> Expectation
shouldBeStrictlySortedOn xs f = zip xs (drop 1 xs) `shouldSatisfy` all (\(a1, a2) -> f a1 < f a2)

spec :: Spec
spec = do
    let
        mkDbInfo baseDbInfo migs = baseDbInfo {
            sqlMigrations = Right migs
            }
    describe "DbDependentSpecs" $ do
        describe "Invariants tests" $ do
            aroundDatabaseWithMigs [ timestampsMig ] $
                it "Our Haskell's timestamping functions and the Database's timestamps behave the same wrt ordering and round-trips" $
                    -- This is much more a test of postgresql-simple than Codd. But it's such an important property to know that holds
                    -- that we test for it here anyway.
                    \dbInfo ->
                        let
                            appConnInfo = superUserInAppDatabaseConnInfo dbInfo
                            veryCloseUtcTimes =
                                zip [0..] $
                                    map (UTCTime (fromGregorian 2020 1 1)) [0, 0.5, 1, 1.5, 2, 3, 4, 5]
                                    ++ map (UTCTime (fromGregorian 2020 1 2)) [0, 0.03, 0.5, 0.84, 1, 1.5, 2, 3, 4, 5, 6, 7]
                            migTimes = nubBy (\(_, t1, _) (_, t2, _) -> t1 == t2) $
                                map (\(n, t) -> let (roundedUtcTime, dbTime) = toMigrationTimestamp t in (n, roundedUtcTime, dbTime)) veryCloseUtcTimes
                        in
                        withConnection appConnInfo $ \conn -> do
                            void $ DB.executeMany conn "INSERT INTO timestamps (seq_number, tm1, tm2) VALUES (?, ?, ?)" migTimes
                            timesFromDb1 :: [(Int, UTCTime, UTCTime)] <- DB.query conn "SELECT seq_number, tm1, tm2 FROM timestamps ORDER BY tm1" ()
                            timesFromDb2 :: [(Int, UTCTime, DB.UTCTimestamp)] <- DB.query conn "SELECT seq_number, tm1, tm2 FROM timestamps ORDER BY tm1" ()
                            timesFromDb2 `shouldBe` migTimes
                            map (\(n, t1, t2) -> (n, t1, DB.Finite t2)) timesFromDb1 `shouldBe` timesFromDb2
                            timesFromDb1 `shouldBeStrictlySortedOn` (\(v, _, _) -> v)
                            timesFromDb1 `shouldBeStrictlySortedOn` (\(_, t1, _) -> t1)
                            timesFromDb1 `shouldBeStrictlySortedOn` (\(_, _, t2) -> t2)
                            timesFromDb2 `shouldBeStrictlySortedOn` (\(_, _, t2) -> t2)

            aroundTestDbInfo $
                it "We can't let impure properties affect our DB Hashing" $ \intactDbInfo -> do
                    -- One possible impurity is the time certain objects are added to the Database. So we apply our migrations with a few seconds
                    -- in between and check the hashes match
                    let dbInfo = intactDbInfo {
                        sqlMigrations = Right [ lotsOfObjectsMigration ]
                    }
                    dbHashes1 <- runStdoutLoggingT $ withDbAndDrop dbInfo (flip withConnection (readHashesFromDatabaseWithSettings dbInfo))
                    threadDelay (5 * 1000 * 1000)
                    dbHashes2 <- runStdoutLoggingT $ withDbAndDrop dbInfo (flip withConnection (readHashesFromDatabaseWithSettings dbInfo))
                    dbHashes1 `shouldBe` dbHashes2