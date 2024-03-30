module DbDependentSpecs.ApplicationSpec where

import           Codd                           ( VerifySchemas(..)
                                                , applyMigrations
                                                , applyMigrationsNoCheck
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Internal                  ( CoddSchemaVersion(..)
                                                , createCoddSchema
                                                , detectCoddSchema
                                                , withConnection
                                                )
import           Codd.Logging                   ( runCoddLogger )
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                , hoistAddedSqlMigration
                                                )
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Representations.Types     ( DbRep(..) )
import           Codd.Types                     ( TxnIsolationLvl(..) )
import           Control.Monad                  ( forM_
                                                , void
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import qualified Data.Aeson                    as Aeson
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( CalendarDiffTime(ctTime)
                                                , UTCTime
                                                , diffUTCTime
                                                , secondsToNominalDiffTime
                                                )
import qualified Database.PostgreSQL.Simple    as DB
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
import           Test.QuickCheck
import qualified Test.QuickCheck               as QC
import           UnliftIO                       ( SomeException
                                                , liftIO
                                                )

placeHoldersMig, selectMig, copyMig :: MonadThrow m => AddedSqlMigration m
placeHoldersMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0000-placeholders.sql"
        , migrationSql            = mkValidSql
                                        "CREATE TABLE any_table();\n-- ? $1 $2 ? ? ?"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 0)
selectMig = AddedSqlMigration
    SqlMigration { migrationName           = "0001-select-mig.sql"
                 , migrationSql            = mkValidSql "SELECT 1, 3"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
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
        , migrationEnvVars        = mempty
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
        , migrationEnvVars        = mempty
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
    , migrationEnvVars        = mempty
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
    , migrationEnvVars        = mempty
    }

alwaysPassingMig, createTableMig, addColumnMig, alwaysFailingMig
    :: MonadThrow m => AddedSqlMigration m
alwaysPassingMig = AddedSqlMigration
    SqlMigration { migrationName           = "0001-always-passing.sql"
                 , migrationSql            = mkValidSql "SELECT 99"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 1)
createTableMig = AddedSqlMigration
    SqlMigration { migrationName           = "0002-create-table.sql"
                 , migrationSql = mkValidSql "CREATE TABLE anytable ();"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 2)
addColumnMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0003-add-column.sql"
        , migrationSql            = mkValidSql
                                        "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 3)
alwaysFailingMig = AddedSqlMigration
    SqlMigration { migrationName           = "0004-always-failing.sql"
                 , migrationSql            = mkValidSql "SELECT 5/0"
                 , migrationInTxn          = True
                 , migrationCustomConnInfo = Nothing
                 , migrationEnvVars        = mempty
                 }
    (getIncreasingTimestamp 4)

changeConnUser
    :: CoddSettings -> String -> AddedSqlMigration m -> AddedSqlMigration m
changeConnUser dbInfo newUser mig = mig
    { addedSqlMig = (addedSqlMig mig)
                        { migrationCustomConnInfo =
                            let cinfo = fromMaybe
                                    (migsConnString dbInfo)
                                    (migrationCustomConnInfo (addedSqlMig mig))
                            in  Just cinfo { DB.connectUser = newUser }
                        }
    }

changeConnDb
    :: CoddSettings -> String -> AddedSqlMigration m -> AddedSqlMigration m
changeConnDb dbInfo newDb mig = mig
    { addedSqlMig = (addedSqlMig mig)
                        { migrationCustomConnInfo =
                            let cinfo = fromMaybe
                                    (migsConnString dbInfo)
                                    (migrationCustomConnInfo (addedSqlMig mig))
                            in  Just cinfo { DB.connectDatabase = newDb }
                        }
    }


-- | A migration that uses many different ways of inputting strings in postgres. In theory we'd only need to
-- test the parser, but we do this to sleep well at night too.
-- This migration only makes sense with standard_conforming_strings=on.
stdConfStringsMig :: MonadThrow m => AddedSqlMigration m
stdConfStringsMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0001-string-escaping.sql"
        , migrationSql            =
            mkValidSql
                "create table string_escape_tests (id int not null, t text not null);\n\

\insert into string_escape_tests (id, t) values \n\
\    (1, 'bc\\def')\n\
\    -- ^ With standard_confirming_strings=on, the value inserted above should be the Haskell string \"bc\\def\"\n\

\    , (2, E'abc\\def')\n\
\    -- ^ The value above should _not_ contain the slash, it should be the Haskell string \"abcdef\"\n\

\    , (3, E'abc\\\\def')\n\
\    -- ^ The value above should be the Haskell string \"abc\\def\"\n\

\    , (4, U&'d\\0061t\\+000061')\n\
\    -- ^ The value above should be the Haskell string \"data\"\n\

\    , (5, U&'d!0061t!+000061' UESCAPE '!')\n\
\    -- ^ The value above should also be the Haskell string \"data\"\n\

\    , (6, U&'d;0061t;+000061' UESCAPE ';')\n\
\    -- ^ The value above should also be the Haskell string \"data\"\n\

\    , (7, U&'d\\0061t\\+000061\\\\''')\n\
\    -- ^ The value above should also be the Haskell string \"data\\'\"\n\

\    , (8, U&'\\0441\\043B\\043E\\043D')\n\
\    -- ^ The value above should be the Haskell string \"слон\"\n\

\    , (9, $$Dianne's horse$$)\n\
\    -- ^ Haskell string \"Dianne's horse\"\n\

\    , (10, $SomeTag$Dianne's horse$SomeTag$)\n\
\    -- ^ Same as above\n\
\;"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 0)

-- | A migration that uses many different ways of inputting strings in postgres. In theory we'd only need to
-- test the parser, but we do this to sleep well at night too.
-- This migration only makes sense with standard_conforming_strings=off.
notStdConfStringsMig :: MonadThrow m => AddedSqlMigration m
notStdConfStringsMig = AddedSqlMigration
    SqlMigration
        { migrationName           = "0001-string-escaping.sql"
        , migrationSql            =
            mkValidSql
                "set standard_conforming_strings=off; create table string_escape_tests (id int not null, t text not null);\n\

\insert into string_escape_tests (id, t) values \n\
\    (1, 'bc\\def')\n\
\    -- ^ With standard_confirming_strings=off, the value inserted above should be the Haskell string \"bcdef\"\n\

\    , (2, 'abc\\\\de''f')\n\
\    -- ^ The value above should _not_ contain the slash, it should be the Haskell string \"abc\\de'f\"\n\
\;"
        , migrationInTxn          = True
        , migrationCustomConnInfo = Nothing
        , migrationEnvVars        = mempty
        }
    (getIncreasingTimestamp 0)

spec :: Spec
spec = do
    describe "DbDependentSpecs" $ do
        describe "Application tests" $ do
            describe "codd_schema version migrations"
                $ aroundFreshDatabase
                $ forM_ [CoddSchemaDoesNotExist .. maxBound]
                $ \vIntermediary ->
                      it
                              (  "codd_schema version migration succeeds from "
                              ++ show CoddSchemaDoesNotExist
                              ++ " to "
                              ++ show vIntermediary
                              ++ " and then to "
                              ++ show (maxBound @CoddSchemaVersion)
                              )
                          $ \emptyTestDbInfo ->
                                void @IO
                                    $ withConnection
                                          (migsConnString emptyTestDbInfo)
                                          testConnTimeout
                                    $ \conn -> do
                                          -- Drop the codd_schema that was created by `aroundFreshDatabase`
                                          void $ DB.execute_
                                              conn
                                              "DROP SCHEMA codd_schema CASCADE"
                                          createCoddSchema vIntermediary
                                                           Serializable
                                                           conn
                                          detectCoddSchema conn
                                              `shouldReturn` vIntermediary
                                          createCoddSchema maxBound
                                                           Serializable
                                                           conn
                                          detectCoddSchema conn
                                              `shouldReturn` maxBound

            describe "With the default database available"
                $ aroundFreshDatabase
                $ do
                      it
                              "SQL containing characters typical to placeholders does not throw"
                          $ \emptyTestDbInfo -> do
                                void @IO
                                    $ runCoddLogger
                                    $ applyMigrationsNoCheck
                                          emptyTestDbInfo
                                          (Just [placeHoldersMig])
                                          testConnTimeout
                                          (const $ pure ())

                      it "Rows-returning function works for no-txn migrations"
                          $ \emptyTestDbInfo -> do
                                void @IO
                                    $ runCoddLogger
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
                                    $ runCoddLogger
                                    $ applyMigrationsNoCheck
                                          emptyTestDbInfo
                                          (Just [inTxnMig])
                                          testConnTimeout
                                          (const $ pure ())

                      it
                              "String escaping works in all its forms with standard_conforming_strings=on"
                          $ \emptyTestDbInfo -> void @IO $ do
                                stringsAndIds :: [(Int, Text)] <-
                                    runCoddLogger $ applyMigrationsNoCheck
                                        emptyTestDbInfo
                                        (Just [stdConfStringsMig])
                                        testConnTimeout
                                        (\conn -> liftIO $ DB.query
                                            conn
                                            "SELECT id, t FROM string_escape_tests ORDER BY id"
                                            ()
                                        )

                                map snd stringsAndIds
                                    `shouldBe` [ "bc\\def"
                                               , "abcdef"
                                               , "abc\\def"
                                               , "data"
                                               , "data"
                                               , "data"
                                               , "data\\'"
                                               , "слон"
                                               , "Dianne's horse"
                                               , "Dianne's horse"
                                               ]
                      it
                              "String escaping works in all its forms with standard_conforming_strings=off"
                          $ \emptyTestDbInfo -> void @IO $ do
                                stringsAndIds :: [(Int, Text)] <-
                                    runCoddLogger $ applyMigrationsNoCheck
                                        emptyTestDbInfo
                                        (Just [notStdConfStringsMig])
                                        testConnTimeout
                                        (\conn -> liftIO $ DB.query
                                            conn
                                            "SELECT id, t FROM string_escape_tests ORDER BY id"
                                            ()
                                        )

                                map snd stringsAndIds
                                    `shouldBe` ["bcdef", "abc\\de'f"]

                      it "COPY FROM STDIN works" $ \emptyTestDbInfo ->
                          runCoddLogger

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
                                              runCoddLogger @IO
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
                                          runCoddLogger

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
                                          void $ runCoddLogger

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
                                                        runCoddLogger

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
                                                , migrationEnvVars = mempty
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
                                                , migrationEnvVars = mempty
                                                }
                                            (getIncreasingTimestamp 1)
                                        , AddedSqlMigration
                                            SqlMigration
                                                { migrationName =
                                                    "0002-no-txn-mig.sql"
                                                , migrationSql =
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
                                                , migrationEnvVars = mempty
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
                                                , migrationEnvVars = mempty
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
                                                , migrationEnvVars = mempty
                                                }
                                            (getIncreasingTimestamp 4)
                                        ]

                                void @IO
                                    $ runCoddLogger
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

            describe "Custom connection-string migrations" $ do
                aroundFreshDatabase
                    $ forM_ [True, False]
                    $ \addDefaultConnMig ->
                          it
                                  ("In-txn migrations in non-default database get registered after all said migrations are committed, not after each one is applied inside a transaction. Default-conn mig first: "
                                  ++ show addDefaultConnMig
                                  )
                              $ \dbInfo -> do
                                      -- To test this we put three consecutive in-txn migrations that run on a non-default database, where the last migration always fails.
                                      -- Neither of the three migrations should be registered in this scenario, as they were all rolled back. In a second time, we run only the first two migrations,
                                      -- and check they were registered.
                                    runCoddLogger
                                            (applyMigrationsNoCheck
                                                dbInfo
                                                (  Just
                                                $
                                                   -- A default-conn mig is interesting because it makes the default connection available if it runs first, but we don't want that default connection to be used too soon
                                                   [ alwaysPassingMig
                                                   | addDefaultConnMig
                                                   ]
                                                ++ [ changeConnDb
                                                       dbInfo
                                                       "postgres"
                                                       createTableMig
                                                   , changeConnDb
                                                       dbInfo
                                                       "postgres"
                                                       addColumnMig
                                                   , changeConnDb
                                                       dbInfo
                                                       "postgres"
                                                       alwaysFailingMig
                                                   ]
                                                )
                                                testConnTimeout
                                                (const $ pure ())
                                            )
                                        `shouldThrow` (\(_ :: SomeException) ->
                                                          True
                                                      )
                                    allRegisteredMigs :: [String] <-
                                        map DB.fromOnly <$> withConnection
                                            (migsConnString dbInfo)
                                            testConnTimeout
                                            (\conn -> DB.query
                                                conn
                                                "SELECT name from codd_schema.sql_migrations"
                                                ()
                                            )
                                    allRegisteredMigs
                                        `shouldNotContain` [ migrationName
                                                                 (addedSqlMig
                                                                     @IO
                                                                     createTableMig
                                                                 )
                                                           ]
                                    allRegisteredMigs
                                        `shouldNotContain` [ migrationName
                                                                 (addedSqlMig
                                                                     @IO
                                                                     addColumnMig
                                                                 )
                                                           ]
                                    allRegisteredMigs
                                        `shouldNotContain` [ migrationName
                                                                 (addedSqlMig
                                                                     @IO
                                                                     alwaysFailingMig
                                                                 )
                                                           ]

                                    -- If we don't include the third migration, the first two should be applied
                                    runCoddLogger
                                        (applyMigrationsNoCheck
                                            dbInfo
                                            (  Just
                                            $
                                                   -- A default-conn mig is interesting because it makes the default connection available if it runs first, but we don't want that default connection to be used too soon
                                               [ alwaysPassingMig
                                               | addDefaultConnMig
                                               ]
                                            ++ [ changeConnDb
                                                   dbInfo
                                                   "postgres"
                                                   createTableMig
                                               , changeConnDb dbInfo
                                                              "postgres"
                                                              addColumnMig
                                               ]
                                            )
                                            testConnTimeout
                                            (const $ pure ())
                                        )
                                    allRegisteredMigs2 :: [String] <-
                                        map DB.fromOnly <$> withConnection
                                            (migsConnString dbInfo)
                                            testConnTimeout
                                            (\conn -> DB.query
                                                conn
                                                "SELECT name from codd_schema.sql_migrations"
                                                ()
                                            )
                                    allRegisteredMigs2
                                        `shouldContain` [ migrationName
                                                              (addedSqlMig @IO
                                                                  createTableMig
                                                              )
                                                        ]
                                    allRegisteredMigs2
                                        `shouldContain` [ migrationName
                                                              (addedSqlMig @IO
                                                                  addColumnMig
                                                              )
                                                        ]
                                    allRegisteredMigs2
                                        `shouldNotContain` [ migrationName
                                                                 (addedSqlMig
                                                                     @IO
                                                                     alwaysFailingMig
                                                                 )
                                                           ]
                aroundFreshDatabase
                    $ forM_ [True, False]
                    $ \addDefaultConnMig ->
                          it
                                  ("In-txn migrations in same database as the default connection string get registered in the same transaction even for a different user. Default-conn mig first: "
                                  ++ show addDefaultConnMig
                                  )
                              $ \dbInfo -> do
                          -- To test this we put three consecutive in-txn migrations on the default database under a different user, where the last migration always fails.
                          -- Neither of the three migrations should be registered in this scenario, as they were all rolled back.
                                    runCoddLogger
                                            (applyMigrationsNoCheck
                                                dbInfo
                                                (  Just
                                                $
                                                   -- A default-conn mig is interesting because it makes the default connection available if it runs first, but we don't want that default connection to be used regardless
                                                   [ alwaysPassingMig
                                                   | addDefaultConnMig
                                                   ]
                                                ++ [ changeConnUser
                                                       dbInfo
                                                       "codd-test-user"
                                                       createTableMig
                                                   , changeConnUser
                                                       dbInfo
                                                       "codd-test-user"
                                                       addColumnMig
                                                   , changeConnUser
                                                       dbInfo
                                                       "codd-test-user"
                                                       alwaysFailingMig
                                                   ]
                                                )
                                                testConnTimeout
                                                (const $ pure ())
                                            )
                                        `shouldThrow` (\(_ :: SomeException) ->
                                                          True
                                                      )
                                    allRegisteredMigs :: [String] <-
                                        map DB.fromOnly <$> withConnection
                                            (migsConnString dbInfo)
                                            testConnTimeout
                                            (\conn -> DB.query
                                                conn
                                                "SELECT name from codd_schema.sql_migrations"
                                                ()
                                            )
                                    allRegisteredMigs
                                        `shouldNotContain` [ migrationName
                                                                 (addedSqlMig
                                                                     @IO
                                                                     createTableMig
                                                                 )
                                                           ]
                                    allRegisteredMigs
                                        `shouldNotContain` [ migrationName
                                                                 (addedSqlMig
                                                                     @IO
                                                                     addColumnMig
                                                                 )
                                                           ]
                                    allRegisteredMigs
                                        `shouldNotContain` [ migrationName
                                                                 (addedSqlMig
                                                                     @IO
                                                                     alwaysFailingMig
                                                                 )
                                                           ]
                it
                        "applied_at and application_duration registered properly for migrations running before codd_schema is available"
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
                                    runCoddLogger $ applyMigrationsNoCheck
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
                                              runMigs :: [ ( FilePath
                                                    , UTCTime
                                                    , CalendarDiffTime
                                                    )
                                                  ]                           <-
                                                  DB.query
                                                      conn
                                                      "SET intervalstyle TO 'iso_8601'; SELECT name, applied_at, application_duration FROM codd_schema.sql_migrations ORDER BY applied_at, id"
                                                      ()
                                              map (\(f, _, _) -> f) runMigs
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
                                                      (\(n, _, _) ->
                                                          "-create-database-mig.sql"
                                                              `Text.isSuffixOf` Text.pack
                                                                                    n
                                                      )
                                                      runMigs
                                              zipWith
                                                      (\(_, time1 :: UTCTime, _) (_, time2, _) ->
                                                          diffUTCTime time2
                                                                      time1
                                                      )
                                                      migsWithSleep
                                                      (drop 1 migsWithSleep)
                                                  `shouldSatisfy` all
                                                                      (> minTimeBetweenMigs
                                                                      )
                                              migsWithSleep `shouldSatisfy` all
                                                  (\(_, _, applicationDuration) ->
                                                      ctTime applicationDuration
                                                          > secondsToNominalDiffTime
                                                                0.7
                                                                 -- 700ms is a conservative value given each duration should be ~ 1 sec
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
                                              runCoddLogger
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
mergeShuffle ll1 ll2 f = go ll1 ll2 (0 :: Int)
  where
    go []          l2          i = pure $ zipWith f [i ..] l2
    go l1          []          i = pure $ zipWith f [i ..] l1
    go l1@(x : xs) l2@(y : ys) i = do
        yieldFirst <- arbitrary @Bool
        if yieldFirst
            then (f i x :) <$> go xs l2 (i + 1)
            else (f i y :) <$> go l1 ys (i + 1)

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
    -> AddedSqlMigration m
    -> Gen (DiverseMigrationOrder m)
diversifyAppCheckMigs defaultConnInfo createCoddTestDbMigs = do
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
