module DbDependentSpecs.InvariantsSpec where

import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( readHashesFromDatabaseWithSettings
                                                )
import           Codd.Internal                  ( withConnection )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                , toMigrationTimestamp
                                                )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Data.List                      ( nubBy )
import           Data.Text                      ( unpack )
import           Data.Time.Calendar             ( fromGregorian )
import           Data.Time.Clock                ( UTCTime(..) )
import qualified Database.PostgreSQL.Simple    as DB
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import qualified Database.PostgreSQL.Simple.Time
                                               as DB
import           DbUtils                        ( aroundDatabaseWithMigs
                                                , aroundFreshDatabase
                                                , aroundTestDbInfo
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                , shouldBeStrictlySortedOn
                                                , testCoddSettings
                                                , testConnTimeout
                                                , withCoddDbAndDrop
                                                )
import           Test.Hspec
import           UnliftIO.Concurrent            ( threadDelay )

timestampsMig, lotsOfObjectsMigration :: Monad m => AddedSqlMigration m
timestampsMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0000-create-timestamps-table.sql"
        , migrationSql            =
            Just
                $ mkValidSql
                      "CREATE TABLE timestamps (seq_number int not null, tm1 timestamptz not null, tm2 timestamptz not null, UNIQUE (seq_number), UNIQUE(tm1), UNIQUE(tm2));"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp 0)
lotsOfObjectsMigration = AddedSqlMigration
    SqlMigration
        { migrationName           = "0000-create-lots-of-objects.sql"
        , migrationSql            =
            Just
            $ mkValidSql
            $ "CREATE TABLE anytable (id serial primary key, col1 timestamptz not null, col2 text CHECK (col2 <> ''), UNIQUE (col1), UNIQUE(col1, col2));"
            <> "CREATE EXTENSION intarray; CREATE EXTENSION btree_gist;"
 -- <> "CREATE TABLE other_table (a1 int references timestamps(seq_number), a2 circle, unique(a1, a2), exclude using gist (a1 with =, (a2) with &&));"
 -- <> "CREATE UNIQUE INDEX something_idx ON other_table (a1) WHERE (a1 > 50);"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp 0)

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Invariants tests" $ do
            aroundDatabaseWithMigs [timestampsMig]
                $ it
                      "Our Haskell's timestamping functions and the Database's timestamps behave the same wrt ordering and round-trips"
                $
                    -- This is much more a test of postgresql-simple than Codd. But it's such an important property to know that holds
                    -- that we test for it here anyway.
                  \dbInfo@CoddSettings { migsConnString } ->
                      let
                          veryCloseUtcTimes =
                              zip [0 ..]
                                  $  map (UTCTime (fromGregorian 2020 1 1))
                                         [0, 0.5, 1, 1.5, 2, 3, 4, 5]
                                  ++ map
                                         (UTCTime (fromGregorian 2020 1 2))
                                         [ 0
                                         , 0.03
                                         , 0.5
                                         , 0.84
                                         , 1
                                         , 1.5
                                         , 2
                                         , 3
                                         , 4
                                         , 5
                                         , 6
                                         , 7
                                         ]
                          migTimes =
                              nubBy (\(_, t1, _) (_, t2, _) -> t1 == t2) $ map
                                  (\(n, t) ->
                                      let (roundedUtcTime, dbTime) =
                                              toMigrationTimestamp t
                                      in  (n, roundedUtcTime, dbTime)
                                  )
                                  veryCloseUtcTimes
                      in
                          do
                              withConnection migsConnString testConnTimeout
                                  $ \conn -> do
                                        void $ DB.executeMany
                                            conn
                                            "INSERT INTO timestamps (seq_number, tm1, tm2) VALUES (?, ?, ?)"
                                            migTimes
                                        timesFromDb1 :: [ ( Int
                                              , UTCTime
                                              , UTCTime
                                              )
                                            ]                       <-
                                            DB.query
                                                conn
                                                "SELECT seq_number, tm1, tm2 FROM timestamps ORDER BY tm1"
                                                ()
                                        timesFromDb2 :: [ ( Int
                                              , UTCTime
                                              , DB.UTCTimestamp
                                              )
                                            ]                               <-
                                            DB.query
                                                conn
                                                "SELECT seq_number, tm1, tm2 FROM timestamps ORDER BY tm1"
                                                ()
                                        timesFromDb2 `shouldBe` migTimes
                                        map
                                                (\(n, t1, t2) ->
                                                    (n, t1, DB.Finite t2)
                                                )
                                                timesFromDb1
                                            `shouldBe` timesFromDb2
                                        timesFromDb1
                                            `shouldBeStrictlySortedOn` (\(v, _, _) ->
                                                                           v
                                                                       )
                                        timesFromDb1
                                            `shouldBeStrictlySortedOn` (\(_, t1, _) ->
                                                                           t1
                                                                       )
                                        timesFromDb1
                                            `shouldBeStrictlySortedOn` (\(_, _, t2) ->
                                                                           t2
                                                                       )
                                        timesFromDb2
                                            `shouldBeStrictlySortedOn` (\(_, _, t2) ->
                                                                           t2
                                                                       )
                                        putStrLn "All tests passed!"

            it "Timing does not affect hashing" $ do
                    -- One possible impurity is the time certain objects are added to the Database. So we apply our migrations with a few seconds
                    -- in between and check the hashes match
                dbInfo    <- testCoddSettings
                dbHashes1 <- runStdoutLoggingT $ withCoddDbAndDrop
                    [lotsOfObjectsMigration]
                    (\cinfo -> withConnection cinfo testConnTimeout
                        $ readHashesFromDatabaseWithSettings dbInfo
                    )
                threadDelay (5 * 1000 * 1000)
                dbHashes2 <- runStdoutLoggingT $ withCoddDbAndDrop
                    [lotsOfObjectsMigration]
                    (\cinfo -> withConnection cinfo testConnTimeout
                        $ readHashesFromDatabaseWithSettings dbInfo
                    )
                dbHashes1 `shouldBe` dbHashes2
