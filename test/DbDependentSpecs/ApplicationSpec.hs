module DbDependentSpecs.ApplicationSpec where

import           Codd                           ( VerifySchemas(..)
                                                , applyMigrations
                                                , applyMigrationsNoCheck
                                                )
import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( baseApplyMigsBlock
                                                , beginCommitTxnBracket
                                                , collectAndApplyMigrations
                                                , withConnection
                                                )
import           Codd.Internal.MultiQueryStatement
                                                ( SqlStatementException )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                , hoistAddedSqlMigration
                                                , mapSqlMigration
                                                )
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Representations           ( readRepresentationsFromDbWithSettings
                                                )
import           Codd.Representations.Types     ( DbRep(..)
                                                , Json(..)
                                                )
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
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import qualified Data.Aeson                    as Aeson
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( UTCTime
                                                , diffUTCTime
                                                , secondsToDiffTime
                                                , secondsToNominalDiffTime
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           DbUtils                        ( aroundFreshDatabase
                                                , createTestUserMig
                                                , createTestUserMigPol
                                                , finallyDrop
                                                , fixMigsOrder
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                , shouldBeStrictlySortedOn
                                                , testCoddSettings
                                                , testConnInfo
                                                , testConnTimeout
                                                )
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.QuickCheck
import qualified Test.QuickCheck               as QC
import           UnliftIO                       ( MonadIO
                                                , liftIO
                                                , stdout
                                                )
import           UnliftIO.Concurrent            ( MVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )

placeHoldersMig, selectMig, copyMig :: MonadThrow m => AddedSqlMigration m
placeHoldersMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0000-placeholders.sql"
        , migrationSql            = mkValidSql
                                        "CREATE TABLE any_table();\n-- ? $1 $2 ? ? ?"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp 0)
selectMig = AddedSqlMigration
    SqlMigration { migrationName           = "0001-select-mig.sql"
                 , migrationSql            = mkValidSql "SELECT 1, 3"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 }
    (getIncreasingTimestamp 1)
copyMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0002-copy-mig.sql"
        , migrationSql            =
            -- CSV and Text formats' escaping rules aren't obvious.
            -- We test those here. See https://www.postgresql.org/docs/13/sql-copy.html
            -- TODO:
            -- Specifying custom delimiters, escape chars, NULL specifier, BINARY copy.
            -- Always compare to what psql does. Hopefully all the complexity is server-side.
            mkValidSql
                "CREATE TABLE x(name TEXT); COPY x (name) FROM STDIN WITH (FORMAT CSV);\nSome name\n\\.\n COPY x FROM STDIN WITH (FORMAT CSV);\n\\.\n COPY x FROM stdin;\nLine\\nbreak\\r\n\\.\n"
        , migrationInTxn          = False
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp 2)

createTableNewTableMig
    :: MonadThrow m => String -> Bool -> Int -> AddedSqlMigration m
createTableNewTableMig tableName inTxn migOrder = AddedSqlMigration
    SqlMigration
        { migrationName           = "000"
                                    <> show migOrder
                                    <> "-create-table-newtable-mig.sql"
        , migrationSql            = mkValidSql
                                    $  "CREATE TABLE "
                                    <> Text.pack tableName
                                    <> "()"
        , migrationInTxn          = inTxn
        , migrationCustomConnInfo = Nothing
        }
    (getIncreasingTimestamp (fromIntegral migOrder))

createDatabaseMig
    :: MonadThrow m => DB.ConnectInfo -> String -> Int -> Int -> SqlMigration m
createDatabaseMig customConnInfo dbName sleepInSeconds migOrder = SqlMigration
    { migrationName = "000" <> show migOrder <> "-create-database-mig.sql"
    , migrationSql            = mkValidSql
                                $  "CREATE DATABASE "
                                <> Text.pack dbName
                                <> "; SELECT pg_sleep("
                                <> Text.pack (show sleepInSeconds)
                                <> ");"
    , migrationInTxn          = False
    , migrationCustomConnInfo = Just customConnInfo
    }

createCountCheckingMig :: MonadThrow m => Int -> String -> SqlMigration m
createCountCheckingMig expectedCount migName = SqlMigration
    { migrationName = "000" <> show expectedCount <> "-" <> migName <> ".sql"
    , migrationSql            =
        mkValidSql
        $ "DO\
\\n$do$\
\\nBEGIN\
\\n   IF (SELECT COUNT(*) <> "
        <> Text.pack (show expectedCount)
        <> " FROM codd_schema.sql_migrations) THEN\
\\n      RAISE 'Not the right count';\
\\n   END IF;\
\\nEND\
\\n$do$;"
    , migrationInTxn          = False
    , migrationCustomConnInfo = Nothing
    }

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
                                          emptyTestDbInfo
                                          (Just [placeHoldersMig])
                                          testConnTimeout
                                          (const $ pure ())

                      it "Rows-returning function works for no-txn migrations"
                          $ \emptyTestDbInfo -> do
                                void @IO
                                    $ runStdoutLoggingT
                                    $ applyMigrationsNoCheck
                                          emptyTestDbInfo
                                          (Just [selectMig])
                                          testConnTimeout
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
                                          emptyTestDbInfo
                                          (Just [inTxnMig])
                                          testConnTimeout
                                          (const $ pure ())

                      it "COPY FROM STDIN works" $ \emptyTestDbInfo ->
                          runStdoutLoggingT
                                  (applyMigrationsNoCheck
                                      emptyTestDbInfo
                                      (Just [copyMig])
                                      testConnTimeout
                                      (\conn -> liftIO $ DB.query
                                          conn
                                          "SELECT name FROM x ORDER BY name"
                                          ()
                                      )
                                  )
                              `shouldReturn` [ DB.Only @String "Line\nbreak\r"
                                             , DB.Only "Some name"
                                             ]



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
                                                      }
                                          -- This pretty much copies Codd.hs's applyMigrations, but it allows
                                          -- us to run an after-migrations action that queries the transaction isolation level
                                          (actualTxnIsol :: DB.Only String, actualTxnReadOnly :: DB.Only
                                                  String) <-
                                              runStdoutLoggingT @IO
                                                  $ applyMigrationsNoCheck
                                                        modifiedSettings
                                                        -- One in-txn migration is just what we need to make the last action
                                                        -- run in the same transaction as it
                                                        (Just [selectMig])
                                                        testConnTimeout
                                                        (\conn ->
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
                              "Strict checking and lax checking behaviour on mismatched schemas"
                          $ \emptyTestDbInfo -> do
                                let bogusDbHashes =
                                        DbRep Aeson.Null Map.empty Map.empty
                                void @IO
                                    $ withConnection
                                          (migsConnString emptyTestDbInfo)
                                          testConnTimeout
                                    $ \conn -> do

                                      -- Strict checking will not apply the migration and therefore will not
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
                                                          { onDiskReps = Right
                                                              bogusDbHashes
                                                          }
                                                      )
                                                      (Just
                                                          [ createTableNewTableMig
                                                                "newtable"
                                                                True
                                                                1
                                                          ]
                                                      )
                                                      testConnTimeout
                                                      StrictCheck
                                                  )
                                              `shouldThrow` anyIOException
                                          DB.query_
                                                  conn
                                                  "SELECT 1 FROM pg_catalog.pg_tables WHERE tablename='newtable'"
                                              `shouldReturn` ([] :: [ DB.Only
                                                                     Int
                                                               ]
                                                             )

                                          -- Lax checking will apply the migration and will not throw an exception
                                          runStdoutLoggingT
                                              (applyMigrations
                                                  (emptyTestDbInfo
                                                      { onDiskReps = Right
                                                          bogusDbHashes
                                                      }
                                                  )
                                                  (Just
                                                      [ createTableNewTableMig
                                                            "newtable"
                                                            True
                                                            1
                                                      ]
                                                  )
                                                  testConnTimeout
                                                  LaxCheck
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
                                            ("Strict checking commits before checking in the presence of no-txn migrations - "
                                            ++ show firstInTxn
                                            ++ " - "
                                            ++ t1
                                            )
                                        $ \emptyTestDbInfo -> do
                                              let
                                                  bogusDbHashes = DbRep
                                                      Aeson.Null
                                                      Map.empty
                                                      Map.empty
                                              void @IO
                                                  $ withConnection
                                                        (migsConnString
                                                            emptyTestDbInfo
                                                        )
                                                        testConnTimeout
                                                  $ \conn -> do
                                                        runStdoutLoggingT
                                                                (applyMigrations
                                                                    (emptyTestDbInfo
                                                                        { onDiskReps =
                                                                            Right
                                                                                bogusDbHashes
                                                                        }
                                                                    )
                                                                    (Just
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
                                                                    )
                                                                    testConnTimeout
                                                                    StrictCheck
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
                                                    mkValidSql
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
                                                    mkValidSql
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
                                                    mkValidSql
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
                                                    mkValidSql
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
                                                    mkValidSql
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
                                          emptyTestDbInfo
                                          (Just migs)
                                          testConnTimeout
                                          (const $ pure ())
                                withConnection
                                        (migsConnString emptyTestDbInfo)
                                        testConnTimeout
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
                                                        { retryPolicy   =
                                                            RetryPolicy
                                                                7
                                                                (ExponentialBackoff
                                                                    (realToFrac
                                                                        0.001
                                                                    )
                                                                )
                                                        , sqlMigrations =
                                                            [ if inTxn
                                                                  then
                                                                      "test/migrations/retry-policy-test-in-txn/"
                                                                  else
                                                                      "test/migrations/retry-policy-test-no-txn/"
                                                            ]
                                                        }
                                                    )
                                                    Nothing
                                                    testConnTimeout
                                                    (const $ pure ())
                                                )
                                            `shouldThrow` (\(e :: SqlStatementException) ->
                                                              "division by zero"
                                                                  `List.isInfixOf` show
                                                                                       e

                                                                  && "SELECT 7/0"
                                                                  `List.isInfixOf` show
                                                                                       e
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
                                it "For no-txn migrations" $ testRetries False

            describe "Custom connection-string migrations" $ do
                it
                        "applied_at registered properly for migrations running before codd_schema is available"
                    $ do
                          defaultConnInfo      <- testConnInfo
                          testSettings         <- testCoddSettings
                          createCoddTestDbMigs <- (: []) <$> createTestUserMig

                          let postgresCinfo = defaultConnInfo
                                  { DB.connectDatabase = "postgres"
                                  , DB.connectUser     = "postgres"
                                  }

                              allMigs =
                                  map (hoistAddedSqlMigration lift)
                                      $  fixMigsOrder
                                      $  [ AddedSqlMigration
                                               (createDatabaseMig
                                                   postgresCinfo
                                                       { DB.connectDatabase =
                                                           previousDbName
                                                       }
                                                   ("new_database_" <> show i)
                                                   1 {- 1 sec pg_sleep -}
                                                   i
                                               )
                                               (getIncreasingTimestamp 0)
                                         | i              <- [0 .. 3]
                                         , previousDbName <- if i == 0
                                             then ["postgres"]
                                             else
                                                 [ "new_database_"
                                                       <> show (i - 1)
                                                 ]
                                         ]
                                      ++ createCoddTestDbMigs

                          finallyDrop "codd-test-db"
                              $ finallyDrop "new_database_0"
                              $ finallyDrop "new_database_1"
                              $ finallyDrop "new_database_2"
                              $ finallyDrop "new_database_3"
                              $ void @IO
                              $ do
                                    runStdoutLoggingT $ applyMigrationsNoCheck
                                        testSettings
                                        (Just allMigs)
                                        testConnTimeout
                                        (const $ pure ())
                                    withConnection defaultConnInfo
                                                   testConnTimeout
                                        $ \conn -> do
                                        -- 1. Check that migrations ran
                                              map DB.fromOnly
                                                  <$> DB.query
                                                          conn
                                                          "SELECT datname FROM pg_database WHERE datname LIKE 'new_database_%' ORDER BY datname"
                                                          ()

                                                  `shouldReturn` [ "new_database_0" :: String
                                                                 , "new_database_1"
                                                                 , "new_database_2"
                                                                 , "new_database_3"
                                                                 ]

                                              -- 2. Check applied_at is not the time we insert into codd_schema.sql_migrations,
                                              -- but the time when migrations are effectively applied.
                                              runMigs :: [(FilePath, UTCTime)] <-
                                                  DB.query
                                                      conn
                                                      "SELECT name, applied_at FROM codd_schema.sql_migrations ORDER BY applied_at, id"
                                                      ()
                                              map fst runMigs
                                                  `shouldBe` map
                                                                 ( migrationName
                                                                 . addedSqlMig
                                                                 )
                                                                 allMigs

                                              -- Half a second is a conservative minimum given pg_sleep(1) in each migration
                                              let minTimeBetweenMigs =
                                                      secondsToNominalDiffTime
                                                          0.5
                                                  migsWithSleep = filter
                                                      (\(n, _) ->
                                                          "-create-database-mig.sql"
                                                              `Text.isSuffixOf` Text.pack
                                                                                    n
                                                      )
                                                      runMigs
                                              zipWith
                                                      (\(_, time1 :: UTCTime) (_, time2) ->
                                                          diffUTCTime time2
                                                                      time1
                                                      )
                                                      migsWithSleep
                                                      (drop 1 migsWithSleep)
                                                  `shouldSatisfy` all
                                                                      (> minTimeBetweenMigs
                                                                      )

                it "Diverse order of different types of migrations"
                    $ ioProperty
                    $ do
                          defaultConnInfo <- testConnInfo
                          testSettings    <- testCoddSettings
                          createCoddTestDbMigs :: AddedSqlMigration IO <-
                              createTestUserMigPol @IO
                          pure
                              $ forAll
                                    (diversifyAppCheckMigs
                                        defaultConnInfo
                                        testSettings
                                        createCoddTestDbMigs
                                    )
                              $ \(DiverseMigrationOrder allMigs) ->
                                    finallyDrop "codd-test-db"
                                        $ finallyDrop "new_database_0"
                                        $ finallyDrop "new_database_1"
                                        $ finallyDrop "new_database_2"
                                        $ finallyDrop "new_database_3"
                                        $ void
                                        $ do
                                              runStdoutLoggingT
                                                  $ applyMigrationsNoCheck
                                                        testSettings
                                                        (Just $ map
                                                            (hoistAddedSqlMigration
                                                                lift
                                                            )
                                                            allMigs
                                                        )
                                                        testConnTimeout
                                                        (const $ pure ())
                                              withConnection defaultConnInfo
                                                             testConnTimeout
                                                  $ \conn -> do
                                                  -- Check all migrations were applied in order
                                                        runMigs :: [ ( Int
                                                              , FilePath
                                                              )
                                                            ] <-
                                                            DB.query
                                                                conn
                                                                "SELECT id, name FROM codd_schema.sql_migrations ORDER BY applied_at, id"
                                                                ()
                                                        map snd runMigs
                                                            `shouldBe` map
                                                                           (migrationName
                                                                           . addedSqlMig
                                                                           )
                                                                           allMigs
                                                        runMigs
                                                            `shouldBeStrictlySortedOn` fst


-- | Concatenates two lists, generates a shuffle of that
-- that does not change relative order of elements when compared
-- to their original lists. The supplied function is called with 
-- the final 0-based index of each element in the list and the
-- element itself to form the final generated list.
mergeShuffle :: [a] -> [a] -> (Int -> a -> b) -> Gen [b]
mergeShuffle l1 l2 f = go l1 l2 f (0 :: Int)
  where
    go []          l2          f i = pure $ zipWith f [i ..] l2
    go l1          []          f i = pure $ zipWith f [i ..] l1
    go l1@(x : xs) l2@(y : ys) f i = do
        yieldFirst <- arbitrary @Bool
        if yieldFirst
            then (f i x :) <$> go xs l2 f (i + 1)
            else (f i y :) <$> go l1 ys f (i + 1)

data MigToCreate = CreateCoddTestDb | CreateCountCheckingMig Bool | CreateCountCheckingMigDifferentUser Bool | CreateDbCreationMig Int

-- | Holds migrations that test codd_schema's internal management while migrations are applied.
-- Look at the `diversifyAppCheckMigs` function, which generates migrations that explore a combination space
-- with the intent of checking codd's migration application internals are robust.
newtype DiverseMigrationOrder m = DiverseMigrationOrder {
    migrationsInOrder :: [AddedSqlMigration m]
}

instance Show (DiverseMigrationOrder m) where
    show (DiverseMigrationOrder migs) =
        concatMap (show . migrationName . addedSqlMig) migs


diversifyAppCheckMigs
    :: MonadThrow m
    => DB.ConnectInfo
    -> CoddSettings
    -> AddedSqlMigration m
    -> Gen (DiverseMigrationOrder m)
diversifyAppCheckMigs defaultConnInfo testSettings createCoddTestDbMigs = do
    -- We want to diversify the order of migrations we apply.
    -- Meaning we want to test regexp-like sequences
    -- like (custom-connection-mig | default-connection-mig)+
    -- However, some custom-connection migrations need to run
    -- in the right relative order between them, and any default-connection
    -- migrations need to run after the mig that creates "codd-test-db".
    -- So the format is actually approximately:
    -- 1. migs <- [create-codd-test-db] ++ Perm{default-conn-migs}
    -- 2. Merge [create-database-migs] and `migs` into another list
    --    preserving relative order of migs from each list randomly.

    -- Also, we want - to the extent that is possible - to ensure
    -- codd_schema.sql_migrations is up-to-date and with rows in
    -- the correct order.
    -- We do that by having default-conn-migs check COUNT(*)
    -- for that table. We also test same-database-migs with different
    -- users wrt that.

    -- Finally we check the order of migrations in codd_schema.sql_migrations
    -- after having all migrations applied.
    let postgresCinfo = defaultConnInfo { DB.connectDatabase = "postgres"
                                        , DB.connectUser     = "postgres"
                                        }

    -- A small number of count-checking migrations is *important*
    -- to ensure we have statistical diversity in terms of relative
    -- position between different types of migrations.
    -- Otherwise, we'd have count-checking migs as the last migrations
    -- and at the earliest position almost every time, when we could have
    -- custom-connection-string ones there.
    createDbAndFirstDefaultConnMig <-
        sequenceA
            [pure CreateCoddTestDb, CreateCountCheckingMig <$> arbitrary @Bool]

    countCheckingMigs <- QC.resize 5 $ QC.listOf $ QC.elements
        [ CreateCountCheckingMig True
        , CreateCountCheckingMig False
        , CreateCountCheckingMigDifferentUser True
        , CreateCountCheckingMigDifferentUser False
        ]

    migsInOrder <-
        fmap (fixMigsOrder . concat)
        $ mergeShuffle (createDbAndFirstDefaultConnMig ++ countCheckingMigs)
                       (map CreateDbCreationMig [0 .. 3])
        $ \migOrder migType -> case migType of
              CreateCoddTestDb -> [createCoddTestDbMigs]
              CreateDbCreationMig i ->
                  [ AddedSqlMigration
                        (createDatabaseMig
                            postgresCinfo { DB.connectDatabase = previousDbName
                                          }
                            ("new_database_" <> show i)
                            0 {- no pg_sleep, another test already tests this -}
                            i
                        )
                        (getIncreasingTimestamp 0)
                  | previousDbName <- if i == 0
                      then ["postgres"]
                      else ["new_database_" <> show (i - 1)]
                  ]
              CreateCountCheckingMig inTxn ->
                  [ AddedSqlMigration
                        (createCountCheckingMig migOrder "count-checking-mig")
                            { migrationInTxn = inTxn
                            }
                        (getIncreasingTimestamp 0)
                  ]

              CreateCountCheckingMigDifferentUser inTxn ->
                  [ AddedSqlMigration
                            (createCountCheckingMig
                                    migOrder
                                    "count-checking-custom-user-mig"
                                )
                                { migrationCustomConnInfo = Just defaultConnInfo
                                    { DB.connectUser = "codd-test-user"
                                    }
                                , migrationInTxn          = inTxn
                                }
                        $ getIncreasingTimestamp 0
                  ]
    pure $ DiverseMigrationOrder migsInOrder


runMVarLogger :: MonadIO m => MVar [Text] -> LoggingT m a -> m a
runMVarLogger logsmv m = runLoggingT
    m
    (\_loc _source _level str -> modifyMVar_
        logsmv
        (\l -> do
            let s = decodeUtf8 $ fromLogStr str
            liftIO $ Text.hPutStrLn stdout s
            pure $ l ++ [s]
        )
    )
