module DbDependentSpecs.ApplicationSpec where

import           Codd                           ( CheckHashes(..)
                                                , applyMigrations
                                                , applyMigrationsNoCheck
                                                )
import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( readHashesFromDatabaseWithSettings
                                                )
import           Codd.Hashing.Types             ( DbHashes(..)
                                                , ObjHash(..)
                                                )
import           Codd.Internal                  ( CanUpdateCoddSchema(..)
                                                , applyMigrationsInternal
                                                , baseApplyMigsBlock
                                                , beginCommitTxnBracket
                                                , withConnection
                                                )
import           Codd.Internal.MultiQueryStatement
                                                ( SqlStatementException )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                , mapSqlMigration
                                                )
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Types                     ( RetryBackoffPolicy(..)
                                                , RetryPolicy(..)
                                                , TxnIsolationLvl(..)
                                                )
import           Control.Monad                  ( forM_
                                                , void
                                                , when
                                                )
import           Control.Monad.Logger           ( LogStr
                                                , LoggingT(runLoggingT)
                                                , fromLogStr
                                                , runStdoutLoggingT
                                                )
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Time                      ( UTCTime
                                                , diffUTCTime
                                                , secondsToNominalDiffTime
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           DbUtils                        ( aroundFreshDatabase
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                , testCoddSettings
                                                , testConnInfo
                                                )
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.QuickCheck
import           UnliftIO                       ( MonadIO
                                                , liftIO
                                                )
import           UnliftIO.Concurrent            ( MVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )

placeHoldersMig, selectMig, copyMig :: AddedSqlMigration
placeHoldersMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0000-placeholders.sql"
        , migrationSql            = Just
            $ mkValidSql "CREATE TABLE any_table();\n-- ? $1 $2 ? ? ?"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp 0)
selectMig = AddedSqlMigration
    SqlMigration { migrationName           = "0001-select-mig.sql"
                 , migrationSql            = Just $ mkValidSql "SELECT 1, 3"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 }
    (getIncreasingTimestamp 1)
copyMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0002-copy-mig.sql"
        , migrationSql            =
            Just
                $ mkValidSql
                      "CREATE TABLE x(name TEXT); COPY x (name) FROM STDIN WITH (FORMAT CSV);\nSome name\n\\.\n COPY x FROM STDIN WITH (FORMAT CSV);\n\\.\n "
        , migrationInTxn          = False
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp 2)
divideBy0Mig = AddedSqlMigration
    SqlMigration { migrationName           = "0003-divide-by-0-mig.sql"
                 , migrationSql = Just $ mkValidSql "SELECT 2; SELECT 7/0"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 }
    (getIncreasingTimestamp 3)

createTableNewTableMig :: String -> Bool -> Int -> AddedSqlMigration
createTableNewTableMig tableName inTxn migOrder = AddedSqlMigration
    SqlMigration
        { migrationName           = "000"
                                    <> show migOrder
                                    <> "-create-table-newtable-mig.sql"
        , migrationSql            = Just
                                    $  mkValidSql
                                    $  "CREATE TABLE "
                                    <> Text.pack tableName
                                    <> "()"
        , migrationInTxn          = inTxn
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp (fromIntegral migOrder))

createDatabaseMig
    :: DB.ConnectInfo -> String -> Int -> Int -> AddedSqlMigration
createDatabaseMig customConnInfo dbName sleepInSeconds migOrder =
    AddedSqlMigration
        SqlMigration
            { migrationName           = "000"
                                        <> show migOrder
                                        <> "-create-database-mig.sql"
            , migrationSql            = Just
                                        $  mkValidSql
                                        $  "CREATE DATABASE "
                                        <> Text.pack dbName
                                        <> "; SELECT pg_sleep("
                                        <> Text.pack (show sleepInSeconds)
                                        <> ");"
            , migrationInTxn          = False
            , migrationCustomConnInfo = Just customConnInfo
            }
        -- Make sure these are really old, we want them to run before codd-test-db is created
        (getIncreasingTimestamp (fromIntegral migOrder - 10000))

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Application tests" $ do
            describe "With the default database available"
                $ aroundFreshDatabase
                $ do
                      it
                              "SQL containing characters typical to placeholders does not throw"
                          $ \emptyTestDbInfo -> do
                                void @IO
                                    $ runStdoutLoggingT
                                    $ applyMigrationsNoCheck
                                          (emptyTestDbInfo
                                              { sqlMigrations = Right
                                                  [placeHoldersMig]
                                              }
                                          )
                                          (const $ pure ())

                      it "Rows-returning function works for no-txn migrations"
                          $ \emptyTestDbInfo -> do
                                void @IO
                                    $ runStdoutLoggingT
                                    $ applyMigrationsNoCheck
                                          (emptyTestDbInfo
                                              { sqlMigrations = Right
                                                                    [selectMig]
                                              }
                                          )
                                          (const $ pure ())

                      it "Rows-returning function works for in-txn migrations"
                          $ \emptyTestDbInfo -> do
                                let (AddedSqlMigration mig t) = selectMig
                                    inTxnMig = AddedSqlMigration
                                        mig { migrationInTxn = True }
                                        t
                                void @IO
                                    $ runStdoutLoggingT
                                    $ applyMigrationsNoCheck
                                          (emptyTestDbInfo
                                              { sqlMigrations = Right [inTxnMig]
                                              }
                                          )
                                          (const $ pure ())

                      it "COPY FROM STDIN works" $ \emptyTestDbInfo -> do
                          void @IO $ runStdoutLoggingT $ applyMigrationsNoCheck
                              (emptyTestDbInfo { sqlMigrations = Right [copyMig]
                                               }
                              )
                              (const $ pure ())

                      forM_
                              [ DbDefault
                              , Serializable
                              , RepeatableRead
                              , ReadCommitted
                              , ReadUncommitted
                              ]
                          $ \isolLvl ->
                                it
                                        ("Transaction Isolation Level is properly applied - "
                                        <> show isolLvl
                                        )
                                    $ \emptyTestDbInfo -> do
                                          let
                                              modifiedSettings =
                                                  emptyTestDbInfo
                                                      { txnIsolationLvl =
                                                          isolLvl
                                                      , sqlMigrations   = Right
                                                          [selectMig] -- One in-txn migrations is exactly what we need to make the last action
                                                                                      -- run in the same transaction as it
                                                      }
                                          -- This pretty much copies Codd.hs's applyMigrations, but it allows
                                          -- us to run an after-migrations action that queries the transaction isolation level
                                          (actualTxnIsol :: DB.Only String, actualTxnReadOnly :: DB.Only
                                                  String) <-
                                              runStdoutLoggingT @IO
                                                  $ applyMigrationsInternal
                                                        (baseApplyMigsBlock
                                                            (migsConnString
                                                                modifiedSettings
                                                            )
                                                            (retryPolicy
                                                                modifiedSettings
                                                            )
                                                            (\_blocksOfMigs conn ->
                                                                (,)
                                                                    <$> unsafeQuery1
                                                                            conn
                                                                            "SHOW transaction_isolation"
                                                                            ()
                                                                    <*> unsafeQuery1
                                                                            conn
                                                                            "SHOW transaction_read_only"
                                                                            ()
                                                            )
                                                            isolLvl
                                                        )
                                                        modifiedSettings

                                          DB.fromOnly actualTxnReadOnly
                                              `shouldBe` "off"
                                          DB.fromOnly actualTxnIsol
                                              `shouldBe` case isolLvl of
                                                             DbDefault
                                                                 -> "read committed"
                                                             Serializable
                                                                 -> "serializable"
                                                             RepeatableRead
                                                                 -> "repeatable read"
                                                             ReadCommitted
                                                                 -> "read committed"
                                                             ReadUncommitted
                                                                 -> "read uncommitted"

                      it
                              "Hard checking and soft checking behaviour on mismatched checksums"
                          $ \emptyTestDbInfo -> do
                                let
                                    bogusDbHashes = DbHashes (ObjHash "")
                                                             Map.empty
                                                             Map.empty
                                void @IO
                                    $ withConnection
                                          (migsConnString emptyTestDbInfo)
                                    $ \conn -> do

                                      -- Hard checking will not apply the migration and therefore will not
                                      -- create "newtable"
                                          DB.query_
                                                  conn
                                                  "SELECT 1 FROM pg_catalog.pg_tables WHERE tablename='newtable'"
                                              `shouldReturn` ([] :: [ DB.Only
                                                                     Int
                                                               ]
                                                             )
                                          runStdoutLoggingT
                                                  (applyMigrations
                                                      (emptyTestDbInfo
                                                          { sqlMigrations =
                                                              Right
                                                                  [ createTableNewTableMig
                                                                        "newtable"
                                                                        True
                                                                        1
                                                                  ]
                                                          , onDiskHashes = Right
                                                              bogusDbHashes
                                                          }
                                                      )
                                                      HardCheck
                                                  )
                                              `shouldThrow` anyIOException
                                          DB.query_
                                                  conn
                                                  "SELECT 1 FROM pg_catalog.pg_tables WHERE tablename='newtable'"
                                              `shouldReturn` ([] :: [ DB.Only
                                                                     Int
                                                               ]
                                                             )

                                          -- Soft checking will apply the migration and will not throw an exception
                                          runStdoutLoggingT
                                              (applyMigrations
                                                  (emptyTestDbInfo
                                                      { sqlMigrations =
                                                          Right
                                                              [ createTableNewTableMig
                                                                    "newtable"
                                                                    True
                                                                    1
                                                              ]
                                                      , onDiskHashes  = Right
                                                          bogusDbHashes
                                                      }
                                                  )
                                                  SoftCheck
                                              )
                                          DB.query_
                                                  conn
                                                  "SELECT 1 FROM pg_catalog.pg_tables WHERE tablename='newtable'"
                                              `shouldReturn` ([DB.Only 1] :: [ DB.Only
                                                                     Int
                                                               ]
                                                             )

                      forM_ [True, False] $ \firstInTxn ->
                          forM_
                                  [ ("newtable" , "sometable")
                                  , ("sometable", "newtable")
                                  ]
                              $ \(t1, t2) ->
                                    it
                                            ("Hard checking falls back to soft checking in the presence of no-txn migrations - "
                                            ++ show firstInTxn
                                            ++ " - "
                                            ++ t1
                                            )
                                        $ \emptyTestDbInfo -> do
                                              let
                                                  bogusDbHashes = DbHashes
                                                      (ObjHash "")
                                                      Map.empty
                                                      Map.empty
                                              void @IO
                                                  $ withConnection
                                                        (migsConnString
                                                            emptyTestDbInfo
                                                        )
                                                  $ \conn -> do
                                                        runStdoutLoggingT
                                                                (applyMigrations
                                                                    (emptyTestDbInfo
                                                                        { sqlMigrations =
                                                                            Right
                                                                                [ createTableNewTableMig
                                                                                    t1
                                                                                    firstInTxn
                                                                                    1
                                                                                , createTableNewTableMig
                                                                                    t2
                                                                                    (not
                                                                                        firstInTxn
                                                                                    )
                                                                                    2
                                                                                ]
                                                                        , onDiskHashes =
                                                                            Right
                                                                                bogusDbHashes
                                                                        }
                                                                    )
                                                                    HardCheck
                                                                )
                                                            `shouldThrow` anyIOException
                                                        DB.query_
                                                                conn
                                                                "SELECT COUNT(*) FROM pg_catalog.pg_tables WHERE tablename='newtable' OR tablename='sometable'"
                                                            `shouldReturn` [ DB.Only
                                                                                 (2 :: Int
                                                                                 )
                                                                           ]

                      it
                              "no-txn migrations and in-txn migrations run in intertwined blocks"
                          $ \emptyTestDbInfo -> do
                                let
                                    migs =
                                        [ AddedSqlMigration
                                            SqlMigration
                                                { migrationName           =
                                                    "0000-first-in-txn-mig.sql"
                                                , migrationSql            =
                                                    Just
                                                    $ mkValidSql
                                                    $ "CREATE TABLE any_table (txid bigint not null);"
                                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                                -- One unique txid from this migration, two rows
                                                , migrationInTxn          = True
                                                , migrationCustomConnInfo =
                                                    Nothing
                                                }
                                            (getIncreasingTimestamp 0)
                                        , AddedSqlMigration
                                            SqlMigration
                                                { migrationName           =
                                                    "0001-second-in-txn-mig.sql"
                                                , migrationSql            =
                                                    Just
                                                    $ mkValidSql
                                                    $ "INSERT INTO any_table (txid) VALUES (txid_current());"
                                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                                -- No txids from this migration because it runs in the same transaction as the last one, two more rows
                                                , migrationInTxn          = True
                                                , migrationCustomConnInfo =
                                                    Nothing
                                                }
                                            (getIncreasingTimestamp 1)
                                        , AddedSqlMigration
                                            SqlMigration
                                                { migrationName           =
                                                    "0002-no-txn-mig.sql"
                                                , migrationSql            =
                                                    Just
                                                    $ mkValidSql
                                                    $ "CREATE TYPE experience AS ENUM ('junior', 'senior');"
                                                    <> "\nALTER TABLE any_table ADD COLUMN experience experience;"
                                                    <> "\nALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';"
                                                    <> "\nUPDATE any_table SET experience='intern';"
                                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                                -- Two distinct txids because this one doesn't run in a migration and two more rows
                                                , migrationInTxn = False
                                                , migrationCustomConnInfo =
                                                    Nothing
                                                }
                                            (getIncreasingTimestamp 2)
                                        , AddedSqlMigration
                                            SqlMigration
                                                { migrationName           =
                                                    "0003-second-in-txn-mig.sql"
                                                , migrationSql            =
                                                    Just
                                                    $ mkValidSql
                                                    $ "INSERT INTO any_table (txid) VALUES (txid_current());"
                                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                                -- One unique txid from this migration because it runs in a new transaction, two more rows
                                                , migrationInTxn          = True
                                                , migrationCustomConnInfo =
                                                    Nothing
                                                }
                                            (getIncreasingTimestamp 3)
                                        , AddedSqlMigration
                                            SqlMigration
                                                { migrationName           =
                                                    "0004-second-in-txn-mig.sql"
                                                , migrationSql            =
                                                    Just
                                                    $ mkValidSql
                                                    $ "INSERT INTO any_table (txid) VALUES (txid_current());"
                                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                                -- No txids from this migration because it runs in the same transaction as the last one, two more rows
                                                , migrationInTxn          = True
                                                , migrationCustomConnInfo =
                                                    Nothing
                                                }
                                            (getIncreasingTimestamp 4)
                                        ]

                                void @IO
                                    $ runStdoutLoggingT
                                    $ applyMigrationsNoCheck
                                          (emptyTestDbInfo
                                              { sqlMigrations = Right migs
                                              }
                                          )
                                          (const $ pure ())
                                withConnection (migsConnString emptyTestDbInfo)
                                    $ \conn -> do
                                          (countTxIds :: Int, countInterns :: Int, totalRows :: Int) <-
                                              unsafeQuery1
                                                  conn
                                                  "SELECT (SELECT COUNT(DISTINCT txid) FROM any_table), (SELECT COUNT(*) FROM any_table WHERE experience='intern'), (SELECT COUNT(*) FROM any_table);"
                                                  ()
                                          countTxIds `shouldBe` 4
                                          countInterns `shouldBe` 4
                                          totalRows `shouldBe` 10
                      context
                              "Retry Policy retries and backoff work as expected"
                          $ do
                                let
                                    testRetries inTxn emptyTestDbInfo = do
                                        -- Kind of ugly.. we test behaviour by analyzing logs and
                                        -- trust that threadDelay is called.
                                        logsmv <- newMVar []
                                        runMVarLogger
                                                logsmv
                                                (applyMigrationsNoCheck
                                                    (emptyTestDbInfo
                                                        { sqlMigrations =
                                                            Right
                                                                [ mapSqlMigration
                                                                      (\m -> m
                                                                          { migrationInTxn =
                                                                              inTxn
                                                                          }
                                                                      )
                                                                      divideBy0Mig
                                                                ]
                                                        , retryPolicy   =
                                                            RetryPolicy
                                                                7
                                                                (ExponentialBackoff
                                                                    (secondsToNominalDiffTime
                                                                        0.001
                                                                    )
                                                                )
                                                        }
                                                    )
                                                    (const $ pure ())
                                                )
                                            `shouldThrow` (\(e :: SqlStatementException) ->
                                                              "division by zero"
                                                                  `List.isInfixOf` show
                                                                                       e

                                                                  &&

                                                            -- For no-txn migrations, each statement is retried individually
                                                                     (inTxn
                                                                     && "SELECT 2;"
                                                                     `List.isInfixOf` show
                                                                                          e
                                                                     || not
                                                                            inTxn
                                                                     && not
                                                                            ("SELECT 2;"
                                                                            `List.isInfixOf` show
                                                                                                 e
                                                                            )
                                                                     && "SELECT 7/0"
                                                                     `List.isInfixOf` show
                                                                                          e
                                                                     )
                                                          )
                                        logs <- readMVar logsmv
                                        length
                                                (filter
                                                    ("Retrying" `Text.isInfixOf`
                                                    )
                                                    logs
                                                )
                                            `shouldBe` 7
                                        -- The last attempt isn't logged because we don't catch exceptions for it
                                        length
                                                (filter
                                                    ("division by zero" `Text.isInfixOf`
                                                    )
                                                    logs
                                                )
                                            `shouldBe` 7
                                        forM_ [1, 2, 4, 8, 16, 32, 64]
                                            $ \delay ->
                                                  length
                                                          (filter
                                                              ((  "Waiting "
                                                               <> Text.pack
                                                                      (show
                                                                          delay
                                                                      )
                                                               <> "ms"
                                                               ) `Text.isInfixOf`
                                                              )
                                                              logs
                                                          )
                                                      `shouldBe` 1
                                it "For in-txn migrations" $ testRetries True
                                it "For no-txn migrations" $ testRetries True

            describe "Custom connection-string migrations" $ do
                it "Bootstrapping works and connections are lazily opened" $ do
                    defaultConnInfo <- testConnInfo
                    testSettings    <- testCoddSettings []
                    let Right createCoddTestDbMigs = sqlMigrations testSettings
                    let postgresCinfo = defaultConnInfo
                            { DB.connectDatabase = "postgres"
                            , DB.connectUser     = "postgres"
                            }
                    let customConnMigs =
                            [ createDatabaseMig
                                  postgresCinfo
                                      { DB.connectDatabase = previousDbName
                                      }
                                  ("new_database_" <> show i)
                                  1 {- 1 sec pg_sleep -}
                                  i
                            | i              <- [0 .. 3]
                            , previousDbName <- if i == 0
                                then ["postgres"]
                                else ["new_database_" <> show (i - 1)]
                            ]
                    void @IO $ do
                        runStdoutLoggingT $ applyMigrationsNoCheck
                            (testSettings
                                { sqlMigrations = Right
                                                  $  customConnMigs
                                                  ++ createCoddTestDbMigs
                                }
                            )
                            (const $ pure ())
                        withConnection defaultConnInfo $ \conn -> do
                            -- 1. Check all databases were created
                            map DB.fromOnly
                                <$>            DB.query
                                                   conn
                                                   "SELECT datname FROM pg_database WHERE datname LIKE 'new_database_%' ORDER BY datname"
                                                   ()

                                `shouldReturn` [ "new_database_0" :: String
                                               , "new_database_1"
                                               , "new_database_2"
                                               , "new_database_3"
                                               ]

                            -- 2. Check order of migrations and time between their applied_at is adequate
                            bootstrapMigs <- DB.query
                                conn
                                "SELECT name, applied_at FROM codd_schema.sql_migrations ORDER BY applied_at LIMIT 4"
                                ()
                            map fst bootstrapMigs
                                `shouldBe` map
                                               (migrationName . addedSqlMig)
                                               customConnMigs
                            -- Half a second is a conservative minimum given pg_sleep(1) in each migration
                            let minTimeBetweenMigs =
                                    secondsToNominalDiffTime 0.5
                            zipWith
                                    (\(_, time1 :: UTCTime) (_, time2) ->
                                        diffUTCTime time2 time1
                                    )
                                    bootstrapMigs
                                    (drop 1 bootstrapMigs)
                                `shouldSatisfy` all (> minTimeBetweenMigs)

                            -- 3. Check every migration was reported as applied
                            map DB.fromOnly
                                <$>            DB.query
                                                   conn
                                                   "SELECT name FROM codd_schema.sql_migrations ORDER BY applied_at"
                                                   ()
                                `shouldReturn` map
                                                   (migrationName . addedSqlMig)
                                                   (  customConnMigs
                                                   ++ createCoddTestDbMigs
                                                   )


runMVarLogger :: MonadIO m => MVar [Text] -> LoggingT m a -> m a
runMVarLogger logsmv m = runLoggingT
    m
    (\_loc _source _level str ->
        modifyMVar_ logsmv (\l -> pure $ l ++ [decodeUtf8 $ fromLogStr str])
    )
