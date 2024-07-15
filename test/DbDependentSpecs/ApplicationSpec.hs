module DbDependentSpecs.ApplicationSpec where

import Codd
  ( VerifySchemas (..),
    applyMigrations,
    applyMigrationsNoCheck,
  )
import Codd.Environment (CoddSettings (..))
import Codd.Internal
  ( CoddSchemaVersion (..),
    collectPendingMigrations,
    createCoddSchema,
    detectCoddSchema,
    withConnection,
  )
import Codd.Logging (runCoddLogger)
import Codd.Parsing
  ( AddedSqlMigration (..),
    SqlMigration (..),
    hoistAddedSqlMigration,
  )
import Codd.Query
  ( execvoid_,
    unsafeQuery1,
  )
import Codd.Representations.Types (DbRep (..))
import Codd.Types (TxnIsolationLvl (..))
import Control.Monad
  ( forM_,
    void,
  )
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadThrow)
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
  ( CalendarDiffTime (ctTime),
    UTCTime,
    diffUTCTime,
    secondsToNominalDiffTime,
  )
import qualified Database.PostgreSQL.Simple as DB
import DbDependentSpecs.RetrySpec (runMVarLogger)
import DbUtils
  ( aroundFreshDatabase,
    cleanupAfterTest,
    createTestUserMig,
    createTestUserMigPol,
    fixMigsOrder,
    getIncreasingTimestamp,
    mkValidSql,
    shouldBeStrictlySortedOn,
    testCoddSettings,
    testConnInfo,
    testConnTimeout,
  )
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import UnliftIO
  ( SomeException,
    liftIO,
    newMVar,
    readMVar,
  )

placeHoldersMig, selectMig, copyMig :: (MonadThrow m) => AddedSqlMigration m
placeHoldersMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0000-placeholders.sql",
        migrationSql =
          mkValidSql
            "CREATE TABLE any_table();\n-- ? $1 $2 ? ? ?",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 0)
selectMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0001-select-mig.sql",
        migrationSql = mkValidSql "SELECT 1, 3",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 1)
copyMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0002-copy-mig.sql",
        migrationSql =
          -- CSV and Text formats' escaping rules aren't obvious.
          -- We test those here. See https://www.postgresql.org/docs/13/sql-copy.html
          -- TODO:
          -- Specifying custom delimiters, escape chars, NULL specifier, BINARY copy.
          -- Always compare to what psql does. Hopefully all the complexity is server-side.
          mkValidSql
            "CREATE TABLE x(name TEXT); COPY x (name) FROM STDIN WITH (FORMAT CSV);\nSome name\n\\.\n COPY x FROM STDIN WITH (FORMAT CSV);\n\\.\n COPY x FROM stdin;\nLine\\nbreak\\r\n\\.\n",
        migrationInTxn = False,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 2)

createTableNewTableMig ::
  (MonadThrow m) => String -> Bool -> Int -> AddedSqlMigration m
createTableNewTableMig tableName inTxn migOrder =
  AddedSqlMigration
    SqlMigration
      { migrationName =
          "000"
            <> show migOrder
            <> "-create-table-newtable-mig.sql",
        migrationSql =
          mkValidSql $
            "CREATE TABLE "
              <> Text.pack tableName
              <> "()",
        migrationInTxn = inTxn,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp (fromIntegral migOrder))

createDatabaseMig ::
  (MonadThrow m) => DB.ConnectInfo -> String -> Int -> Int -> SqlMigration m
createDatabaseMig customConnInfo dbName sleepInSeconds migOrder =
  SqlMigration
    { migrationName = "000" <> show migOrder <> "-create-database-mig.sql",
      migrationSql =
        mkValidSql $
          "CREATE DATABASE "
            <> Text.pack dbName
            <> "; SELECT pg_sleep("
            <> Text.pack (show sleepInSeconds)
            <> ");",
      migrationInTxn = False,
      migrationCustomConnInfo = Just customConnInfo,
      migrationEnvVars = mempty
    }

createAlwaysPassingMig :: (MonadThrow m) => Int -> String -> SqlMigration m
createAlwaysPassingMig migrationIdx migName =
  SqlMigration
    { migrationName = "000" <> show migrationIdx <> "-" <> migName <> ".sql",
      migrationSql = mkValidSql "SELECT 71",
      migrationInTxn = False,
      migrationCustomConnInfo = Nothing,
      migrationEnvVars = mempty
    }

alwaysPassingMig,
  createTableMig,
  addColumnMig,
  alwaysFailingMig,
  alwaysFailingMigNoTxn ::
    (MonadThrow m) => AddedSqlMigration m
alwaysPassingMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0001-always-passing.sql",
        migrationSql = mkValidSql "SELECT 99",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 1)
createTableMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0002-create-table.sql",
        migrationSql = mkValidSql "CREATE TABLE anytable ();",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 2)
addColumnMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0003-add-column.sql",
        migrationSql =
          mkValidSql
            "ALTER TABLE anytable ADD COLUMN anycolumn TEXT;",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 3)
alwaysFailingMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0004-always-failing.sql",
        migrationSql = mkValidSql "SELECT 5/0",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 4)
alwaysFailingMigNoTxn =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0005-always-failing-no-txn.sql",
        migrationSql = mkValidSql "SELECT 5/0",
        migrationInTxn = False,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 5)

-- | A migration that creates the codd_schema itself (and an old version at that). This is like what a pg dump would have.
pgDumpEmulatingMig :: (MonadThrow m) => AddedSqlMigration m
pgDumpEmulatingMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0010-pg_dump-emulating-mig.sql",
        migrationSql =
          mkValidSql
            "CREATE SCHEMA codd_schema; GRANT USAGE ON SCHEMA codd_schema TO PUBLIC;\
            \CREATE TABLE codd_schema.sql_migrations ( \
            \  id SERIAL PRIMARY KEY\
            \, migration_timestamp timestamptz not null\
            \, applied_at timestamptz not null \
            \, name text not null \
            \, unique (name), unique (migration_timestamp));\
            \GRANT INSERT,SELECT ON TABLE codd_schema.sql_migrations TO PUBLIC;\
            \GRANT USAGE ,SELECT ON SEQUENCE codd_schema.sql_migrations_id_seq TO PUBLIC;\
            \ -- Pretend a migration that always fails was applied. We'll be able to add this migration in our test as codd should skip it \n\
            \INSERT INTO codd_schema.sql_migrations (migration_timestamp, applied_at, name) VALUES ('2000-01-01', '2000-01-01', '0004-always-failing.sql'), ('2000-01-02', '2000-01-01 00:00:01', '0005-always-failing-no-txn.sql')",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 10)

changeConnUser ::
  CoddSettings -> String -> AddedSqlMigration m -> AddedSqlMigration m
changeConnUser dbInfo newUser mig =
  mig
    { addedSqlMig =
        (addedSqlMig mig)
          { migrationCustomConnInfo =
              let cinfo =
                    fromMaybe
                      (migsConnString dbInfo)
                      (migrationCustomConnInfo (addedSqlMig mig))
               in Just cinfo {DB.connectUser = newUser}
          }
    }

changeConnDb ::
  CoddSettings -> String -> AddedSqlMigration m -> AddedSqlMigration m
changeConnDb dbInfo newDb mig =
  mig
    { addedSqlMig =
        (addedSqlMig mig)
          { migrationCustomConnInfo =
              let cinfo =
                    fromMaybe
                      (migsConnString dbInfo)
                      (migrationCustomConnInfo (addedSqlMig mig))
               in Just cinfo {DB.connectDatabase = newDb}
          }
    }

-- | A migration that uses many different ways of inputting strings in postgres. In theory we'd only need to
-- test the parser, but we do this to sleep well at night too.
-- This migration only makes sense with standard_conforming_strings=on.
stdConfStringsMig :: (MonadThrow m) => AddedSqlMigration m
stdConfStringsMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0001-string-escaping.sql",
        migrationSql =
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
            \;",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 0)

-- | A migration that uses many different ways of inputting strings in postgres. In theory we'd only need to
-- test the parser, but we do this to sleep well at night too.
-- This migration only makes sense with standard_conforming_strings=off.
notStdConfStringsMig :: (MonadThrow m) => AddedSqlMigration m
notStdConfStringsMig =
  AddedSqlMigration
    SqlMigration
      { migrationName = "0001-string-escaping.sql",
        migrationSql =
          mkValidSql
            "set standard_conforming_strings=off; create table string_escape_tests (id int not null, t text not null);\n\
            \insert into string_escape_tests (id, t) values \n\
            \    (1, 'bc\\def')\n\
            \    -- ^ With standard_confirming_strings=off, the value inserted above should be the Haskell string \"bcdef\"\n\
            \    , (2, 'abc\\\\de''f')\n\
            \    -- ^ The value above should _not_ contain the slash, it should be the Haskell string \"abc\\de'f\"\n\
            \;",
        migrationInTxn = True,
        migrationCustomConnInfo = Nothing,
        migrationEnvVars = mempty
      }
    (getIncreasingTimestamp 0)

downgradeCoddSchema :: DB.Connection -> CoddSchemaVersion -> IO ()
downgradeCoddSchema conn targetVersion = go maxBound
  where
    go currentSchemaVersion
      | targetVersion == currentSchemaVersion = pure ()
      | otherwise = do
          case currentSchemaVersion of
            CoddSchemaDoesNotExist -> pure ()
            CoddSchemaV1 -> do
              execvoid_ conn "DROP SCHEMA codd_schema CASCADE"
            CoddSchemaV2 ->
              execvoid_
                conn
                "ALTER TABLE codd_schema.sql_migrations DROP COLUMN application_duration"
            CoddSchemaV3 ->
              execvoid_
                conn
                "ALTER TABLE codd_schema.sql_migrations DROP COLUMN num_applied_statements, DROP COLUMN no_txn_failed_at, ALTER COLUMN applied_at SET NOT NULL; \n\
                \REVOKE UPDATE ON TABLE codd_schema.sql_migrations FROM PUBLIC;"
            CoddSchemaV4 ->
              execvoid_
                conn
                "ALTER TABLE codd_schema.sql_migrations DROP COLUMN txnid, DROP COLUMN connid"

          go (pred currentSchemaVersion)

spec :: Spec
spec = do
  describe "DbDependentSpecs" $ do
    describe "Application tests" $ do
      describe "codd_schema version migrations" $
        forM_ [CoddSchemaDoesNotExist .. maxBound] $
          \vIntermediary ->
            aroundFreshDatabase
              $ it
                ( "codd_schema version migration succeeds from "
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
                    -- Downgrade the schema created by `aroundFreshDatabase`. We want migrations applied with different versions to exist.
                    downgradeCoddSchema conn vIntermediary
                    detectCoddSchema conn
                      `shouldReturn` vIntermediary

                    -- Test that we can apply migrations in an older state, and that it updates the schema to the latest
                    runCoddLogger $
                      applyMigrationsNoCheck
                        emptyTestDbInfo
                        -- At least one pending migration so codd updates the schema
                        (Just [alwaysPassingMig])
                        testConnTimeout
                        (const $ pure ())
                    detectCoddSchema conn
                      `shouldReturn` maxBound

                    -- Run `codd up` one more time to ensure test that fetching migrations applied with different schema versions does not fail
                    -- This can happen if we e.g. assume `num_applied_statements` is not nullable for V3, but migrations applied with earlier versions
                    -- will have that with a NULL value.
                    runCoddLogger $
                      applyMigrationsNoCheck
                        emptyTestDbInfo
                        (Just [alwaysPassingMig])
                        testConnTimeout
                        (const $ pure ())

      describe "With the default database available" $
        aroundFreshDatabase $
          do
            it
              "SQL containing characters typical to placeholders does not throw"
              $ \emptyTestDbInfo -> do
                void @IO $
                  runCoddLogger $
                    applyMigrationsNoCheck
                      emptyTestDbInfo
                      (Just [placeHoldersMig])
                      testConnTimeout
                      (const $ pure ())

            it "Rows-returning function works for no-txn migrations" $
              \emptyTestDbInfo -> do
                void @IO $
                  runCoddLogger $
                    applyMigrationsNoCheck
                      emptyTestDbInfo
                      (Just [selectMig])
                      testConnTimeout
                      (const $ pure ())

            it "Rows-returning function works for in-txn migrations" $
              \emptyTestDbInfo -> do
                let (AddedSqlMigration mig t) = selectMig
                    inTxnMig =
                      AddedSqlMigration
                        mig {migrationInTxn = True}
                        t
                void @IO $
                  runCoddLogger $
                    applyMigrationsNoCheck
                      emptyTestDbInfo
                      (Just [inTxnMig])
                      testConnTimeout
                      (const $ pure ())

            it
              "String escaping works in all its forms with standard_conforming_strings=on"
              $ \emptyTestDbInfo -> void @IO $ do
                stringsAndIds :: [(Int, Text)] <-
                  runCoddLogger $
                    applyMigrationsNoCheck
                      emptyTestDbInfo
                      (Just [stdConfStringsMig])
                      testConnTimeout
                      ( \conn ->
                          liftIO $
                            DB.query
                              conn
                              "SELECT id, t FROM string_escape_tests ORDER BY id"
                              ()
                      )

                map snd stringsAndIds
                  `shouldBe` [ "bc\\def",
                               "abcdef",
                               "abc\\def",
                               "data",
                               "data",
                               "data",
                               "data\\'",
                               "слон",
                               "Dianne's horse",
                               "Dianne's horse"
                             ]
            it
              "String escaping works in all its forms with standard_conforming_strings=off"
              $ \emptyTestDbInfo -> void @IO $ do
                stringsAndIds :: [(Int, Text)] <-
                  runCoddLogger $
                    applyMigrationsNoCheck
                      emptyTestDbInfo
                      (Just [notStdConfStringsMig])
                      testConnTimeout
                      ( \conn ->
                          liftIO $
                            DB.query
                              conn
                              "SELECT id, t FROM string_escape_tests ORDER BY id"
                              ()
                      )

                map snd stringsAndIds
                  `shouldBe` ["bcdef", "abc\\de'f"]

            it "COPY FROM STDIN works" $ \emptyTestDbInfo ->
              runCoddLogger
                ( applyMigrationsNoCheck
                    emptyTestDbInfo
                    (Just [copyMig])
                    testConnTimeout
                    ( \conn ->
                        liftIO $
                          DB.query
                            conn
                            "SELECT name FROM x ORDER BY name"
                            ()
                    )
                )
                `shouldReturn` [ DB.Only @String "Line\nbreak\r",
                                 DB.Only "Some name"
                               ]

            forM_
              [ DbDefault,
                Serializable,
                RepeatableRead,
                ReadCommitted,
                ReadUncommitted
              ]
              $ \isolLvl ->
                it
                  ( "Transaction Isolation Level is properly applied - "
                      <> show isolLvl
                  )
                  $ \emptyTestDbInfo -> do
                    let modifiedSettings =
                          emptyTestDbInfo
                            { txnIsolationLvl =
                                isolLvl
                            }
                    -- This pretty much copies Codd.hs's applyMigrations, but it allows
                    -- us to run an after-migrations action that queries the transaction isolation level
                    ( actualTxnIsol :: DB.Only String,
                      actualTxnReadOnly ::
                        DB.Only
                          String
                      ) <-
                      runCoddLogger @IO $
                        applyMigrationsNoCheck
                          modifiedSettings
                          -- One in-txn migration is just what we need to make the last action
                          -- run in the same transaction as it
                          (Just [selectMig])
                          testConnTimeout
                          ( \conn ->
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
                        DbDefault ->
                          "read committed"
                        Serializable ->
                          "serializable"
                        RepeatableRead ->
                          "repeatable read"
                        ReadCommitted ->
                          "read committed"
                        ReadUncommitted ->
                          "read uncommitted"

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
                      `shouldReturn` ( [] ::
                                         [ DB.Only
                                             Int
                                         ]
                                     )
                    runCoddLogger
                      ( applyMigrations
                          ( emptyTestDbInfo
                              { onDiskReps =
                                  Right
                                    bogusDbHashes
                              }
                          )
                          ( Just
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
                      `shouldReturn` ( [] ::
                                         [ DB.Only
                                             Int
                                         ]
                                     )

                    -- Lax checking will apply the migration and will not throw an exception
                    void $
                      runCoddLogger
                        ( applyMigrations
                            ( emptyTestDbInfo
                                { onDiskReps =
                                    Right
                                      bogusDbHashes
                                }
                            )
                            ( Just
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
                      `shouldReturn` ( [DB.Only 1] ::
                                         [ DB.Only
                                             Int
                                         ]
                                     )

            forM_ [True, False] $ \firstInTxn ->
              forM_
                [ ("newtable", "sometable"),
                  ("sometable", "newtable")
                ]
                $ \(t1, t2) ->
                  it
                    ( "Strict checking commits before checking in the presence of no-txn migrations - "
                        ++ show firstInTxn
                        ++ " - "
                        ++ t1
                    )
                    $ \emptyTestDbInfo -> do
                      let bogusDbHashes =
                            DbRep
                              Aeson.Null
                              Map.empty
                              Map.empty
                      void @IO
                        $ withConnection
                          ( migsConnString
                              emptyTestDbInfo
                          )
                          testConnTimeout
                        $ \conn -> do
                          runCoddLogger
                            ( applyMigrations
                                ( emptyTestDbInfo
                                    { onDiskReps =
                                        Right
                                          bogusDbHashes
                                    }
                                )
                                ( Just
                                    [ createTableNewTableMig
                                        t1
                                        firstInTxn
                                        1,
                                      createTableNewTableMig
                                        t2
                                        ( not
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
                                               ( 2 :: Int
                                               )
                                           ]

            it
              "no-txn migrations and in-txn migrations run in intertwined blocks"
              $ \emptyTestDbInfo -> do
                let migs =
                      [ AddedSqlMigration
                          SqlMigration
                            { migrationName =
                                "0000-first-in-txn-mig.sql",
                              migrationSql =
                                mkValidSql $
                                  "CREATE TABLE any_table (txid bigint not null);"
                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());",
                              -- One unique txid from this migration, two rows
                              migrationInTxn = True,
                              migrationCustomConnInfo =
                                Nothing,
                              migrationEnvVars = mempty
                            }
                          (getIncreasingTimestamp 0),
                        AddedSqlMigration
                          SqlMigration
                            { migrationName =
                                "0001-second-in-txn-mig.sql",
                              migrationSql =
                                mkValidSql $
                                  "INSERT INTO any_table (txid) VALUES (txid_current());"
                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());",
                              -- No txids from this migration because it runs in the same transaction as the last one, two more rows
                              migrationInTxn = True,
                              migrationCustomConnInfo =
                                Nothing,
                              migrationEnvVars = mempty
                            }
                          (getIncreasingTimestamp 1),
                        AddedSqlMigration
                          SqlMigration
                            { migrationName =
                                "0002-no-txn-mig.sql",
                              migrationSql =
                                mkValidSql $
                                  "CREATE TYPE experience AS ENUM ('junior', 'senior');"
                                    <> "\nALTER TABLE any_table ADD COLUMN experience experience;"
                                    <> "\nALTER TYPE experience ADD VALUE 'intern' BEFORE 'junior';"
                                    <> "\nUPDATE any_table SET experience='intern';"
                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());"
                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());",
                              -- Two distinct txids because this one doesn't run in a migration and two more rows
                              migrationInTxn = False,
                              migrationCustomConnInfo =
                                Nothing,
                              migrationEnvVars = mempty
                            }
                          (getIncreasingTimestamp 2),
                        AddedSqlMigration
                          SqlMigration
                            { migrationName =
                                "0003-second-in-txn-mig.sql",
                              migrationSql =
                                mkValidSql $
                                  "INSERT INTO any_table (txid) VALUES (txid_current());"
                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());",
                              -- One unique txid from this migration because it runs in a new transaction, two more rows
                              migrationInTxn = True,
                              migrationCustomConnInfo =
                                Nothing,
                              migrationEnvVars = mempty
                            }
                          (getIncreasingTimestamp 3),
                        AddedSqlMigration
                          SqlMigration
                            { migrationName =
                                "0004-second-in-txn-mig.sql",
                              migrationSql =
                                mkValidSql $
                                  "INSERT INTO any_table (txid) VALUES (txid_current());"
                                    <> "\nINSERT INTO any_table (txid) VALUES (txid_current());",
                              -- No txids from this migration because it runs in the same transaction as the last one, two more rows
                              migrationInTxn = True,
                              migrationCustomConnInfo =
                                Nothing,
                              migrationEnvVars = mempty
                            }
                          (getIncreasingTimestamp 4)
                      ]

                void @IO $
                  runCoddLogger $
                    applyMigrationsNoCheck
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
        aroundFreshDatabase $
          forM_ [True, False] $
            \addDefaultConnMig ->
              it
                ( "In-txn migrations in non-default database get registered after all said migrations are committed, not after each one is applied inside a transaction. Default-conn mig first: "
                    ++ show addDefaultConnMig
                )
                $ \dbInfo -> do
                  -- To test this we put three consecutive in-txn migrations that run on a non-default database, where the last migration always fails.
                  -- Neither of the three migrations should be registered in this scenario, as they were all rolled back. In a second time, we run only the first two migrations,
                  -- and check they were registered.
                  runCoddLogger
                    ( applyMigrationsNoCheck
                        dbInfo
                        ( Just $
                            -- A default-conn mig is interesting because it makes the default connection available if it runs first, but we don't want that default connection to be used too soon
                            [ alwaysPassingMig
                              | addDefaultConnMig
                            ]
                              ++ [ changeConnDb
                                     dbInfo
                                     "postgres"
                                     createTableMig,
                                   changeConnDb
                                     dbInfo
                                     "postgres"
                                     addColumnMig,
                                   changeConnDb
                                     dbInfo
                                     "postgres"
                                     alwaysFailingMig
                                 ]
                        )
                        testConnTimeout
                        (const $ pure ())
                    )
                    `shouldThrow` ( \(_ :: SomeException) ->
                                      True
                                  )
                  allRegisteredMigs :: [String] <-
                    map DB.fromOnly
                      <$> withConnection
                        (migsConnString dbInfo)
                        testConnTimeout
                        ( \conn ->
                            DB.query
                              conn
                              "SELECT name from codd_schema.sql_migrations"
                              ()
                        )
                  allRegisteredMigs
                    `shouldNotContain` [ migrationName
                                           ( addedSqlMig
                                               @IO
                                               createTableMig
                                           )
                                       ]
                  allRegisteredMigs
                    `shouldNotContain` [ migrationName
                                           ( addedSqlMig
                                               @IO
                                               addColumnMig
                                           )
                                       ]
                  allRegisteredMigs
                    `shouldNotContain` [ migrationName
                                           ( addedSqlMig
                                               @IO
                                               alwaysFailingMig
                                           )
                                       ]

                  -- If we don't include the third migration, the first two should be applied
                  runCoddLogger
                    ( applyMigrationsNoCheck
                        dbInfo
                        ( Just $
                            -- A default-conn mig is interesting because it makes the default connection available if it runs first, but we don't want that default connection to be used too soon
                            [ alwaysPassingMig
                              | addDefaultConnMig
                            ]
                              ++ [ changeConnDb
                                     dbInfo
                                     "postgres"
                                     createTableMig,
                                   changeConnDb
                                     dbInfo
                                     "postgres"
                                     addColumnMig
                                 ]
                        )
                        testConnTimeout
                        (const $ pure ())
                    )
                  allRegisteredMigs2 :: [String] <-
                    map DB.fromOnly
                      <$> withConnection
                        (migsConnString dbInfo)
                        testConnTimeout
                        ( \conn ->
                            DB.query
                              conn
                              "SELECT name from codd_schema.sql_migrations"
                              ()
                        )
                  allRegisteredMigs2
                    `shouldContain` [ migrationName
                                        ( addedSqlMig @IO
                                            createTableMig
                                        )
                                    ]
                  allRegisteredMigs2
                    `shouldContain` [ migrationName
                                        ( addedSqlMig @IO
                                            addColumnMig
                                        )
                                    ]
                  allRegisteredMigs2
                    `shouldNotContain` [ migrationName
                                           ( addedSqlMig
                                               @IO
                                               alwaysFailingMig
                                           )
                                       ]
        aroundFreshDatabase $
          forM_ [True, False] $
            \addDefaultConnMig ->
              it
                ( "In-txn migrations in same database as the default connection string get registered in the same transaction even for a different user. Default-conn mig first: "
                    ++ show addDefaultConnMig
                )
                $ \dbInfo -> do
                  -- To test this we put three consecutive in-txn migrations on the default database under a different user, where the last migration always fails.
                  -- Neither of the three migrations should be registered in this scenario, as they were all rolled back.
                  runCoddLogger
                    ( applyMigrationsNoCheck
                        dbInfo
                        ( Just $
                            -- A default-conn mig is interesting because it makes the default connection available if it runs first, but we don't want that default connection to be used regardless
                            [ alwaysPassingMig
                              | addDefaultConnMig
                            ]
                              ++ [ changeConnUser
                                     dbInfo
                                     "codd-test-user"
                                     createTableMig,
                                   changeConnUser
                                     dbInfo
                                     "codd-test-user"
                                     addColumnMig,
                                   changeConnUser
                                     dbInfo
                                     "codd-test-user"
                                     alwaysFailingMig
                                 ]
                        )
                        testConnTimeout
                        (const $ pure ())
                    )
                    `shouldThrow` ( \(_ :: SomeException) ->
                                      True
                                  )
                  allRegisteredMigs :: [String] <-
                    map DB.fromOnly
                      <$> withConnection
                        (migsConnString dbInfo)
                        testConnTimeout
                        ( \conn ->
                            DB.query
                              conn
                              "SELECT name from codd_schema.sql_migrations"
                              ()
                        )
                  allRegisteredMigs
                    `shouldNotContain` [ migrationName
                                           ( addedSqlMig
                                               @IO
                                               createTableMig
                                           )
                                       ]
                  allRegisteredMigs
                    `shouldNotContain` [ migrationName
                                           ( addedSqlMig
                                               @IO
                                               addColumnMig
                                           )
                                       ]
                  allRegisteredMigs
                    `shouldNotContain` [ migrationName
                                           ( addedSqlMig
                                               @IO
                                               alwaysFailingMig
                                           )
                                       ]
        after (const cleanupAfterTest)
          $ it
            "applied_at and application_duration registered properly for migrations running before codd_schema is available"
          $ do
            defaultConnInfo <- testConnInfo
            testSettings <- testCoddSettings
            createCoddTestDbMigs <- (: []) <$> createTestUserMig

            let postgresCinfo =
                  defaultConnInfo
                    { DB.connectDatabase = "postgres",
                      DB.connectUser = "postgres"
                    }

                allMigs =
                  map (hoistAddedSqlMigration lift) $
                    fixMigsOrder $
                      [ AddedSqlMigration
                          ( createDatabaseMig
                              postgresCinfo
                                { DB.connectDatabase =
                                    previousDbName
                                }
                              ("new_database_" <> show i)
                              1 {- 1 sec pg_sleep -}
                              i
                          )
                          (getIncreasingTimestamp 0)
                        | i <- [0 .. 3],
                          previousDbName <-
                            if i == 0
                              then ["postgres"]
                              else
                                [ "new_database_"
                                    <> show (i - 1)
                                ]
                      ]
                        ++ createCoddTestDbMigs

            void @IO $
              do
                runCoddLogger $
                  applyMigrationsNoCheck
                    testSettings
                    (Just allMigs)
                    testConnTimeout
                    (const $ pure ())
                withConnection
                  defaultConnInfo
                  testConnTimeout
                  $ \conn -> do
                    -- 1. Check that migrations ran
                    map DB.fromOnly
                      <$> DB.query
                        conn
                        "SELECT datname FROM pg_database WHERE datname LIKE 'new_database_%' ORDER BY datname"
                        ()
                      `shouldReturn` [ "new_database_0" :: String,
                                       "new_database_1",
                                       "new_database_2",
                                       "new_database_3"
                                     ]

                    -- 2. Check applied_at is not the time we insert into codd_schema.sql_migrations,
                    -- but the time when migrations are effectively applied.
                    runMigs ::
                      [ ( FilePath,
                          UTCTime,
                          CalendarDiffTime
                        )
                      ] <-
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
                        migsWithSleep =
                          filter
                            ( \(n, _, _) ->
                                "-create-database-mig.sql"
                                  `Text.isSuffixOf` Text.pack
                                    n
                            )
                            runMigs
                    zipWith
                      ( \(_, time1 :: UTCTime, _) (_, time2, _) ->
                          diffUTCTime
                            time2
                            time1
                      )
                      migsWithSleep
                      (drop 1 migsWithSleep)
                      `shouldSatisfy` all
                        ( > minTimeBetweenMigs
                        )
                    migsWithSleep
                      `shouldSatisfy` all
                        ( \(_, _, applicationDuration) ->
                            ctTime applicationDuration
                              > secondsToNominalDiffTime
                                0.7
                                -- 700ms is a conservative value given each duration should be ~ 1 sec
                        )

        after (const cleanupAfterTest)
          $ it
            "Allow migrations that create codd_schema themselves if they run sufficiently early in some cases"
          $ do
            defaultConnInfo <- testConnInfo
            testSettings <- testCoddSettings
            bootstrapMig <- createTestUserMig
            logsmv <- newMVar []

            let postgresCinfo =
                  defaultConnInfo
                    { DB.connectDatabase = "postgres",
                      DB.connectUser = "postgres"
                    }

                allMigs =
                  map (hoistAddedSqlMigration lift) $
                    fixMigsOrder
                      [ bootstrapMig,
                        alwaysPassingMig,
                        pgDumpEmulatingMig,
                        alwaysFailingMig, -- The previous migration pretends this was applied so codd should skip this
                        alwaysFailingMigNoTxn -- The previous migration pretends this was applied so codd should skip this
                      ]

            finalCoddSchemaVersion <-
              runMVarLogger logsmv $
                applyMigrationsNoCheck
                  testSettings
                  (Just allMigs)
                  testConnTimeout
                  detectCoddSchema
            finalCoddSchemaVersion `shouldBe` maxBound
            withConnection defaultConnInfo testConnTimeout $
              \conn -> do
                -- 1. Check that all migrations are registered. Note they might _NOT_ be in the same order of `allMigs` as the dump is registered as applied after skipped migrations
                runMigs :: Set FilePath <-
                  Set.fromList
                    . map DB.fromOnly
                    <$> DB.query
                      conn
                      "SELECT name FROM codd_schema.sql_migrations"
                      ()
                runMigs
                  `shouldBe` Set.fromList
                    ( map
                        ( migrationName
                            . addedSqlMig
                        )
                        allMigs
                    )
            logs <- readMVar logsmv
            logs
              `shouldSatisfy` any
                ( \l ->
                    "Skipping"
                      `Text.isInfixOf` l
                      && Text.pack
                        ( migrationName
                            ( addedSqlMig @IO
                                alwaysFailingMig
                            )
                        )
                        `Text.isInfixOf` l
                )
            logs
              `shouldSatisfy` any
                ( \l ->
                    "Skipping"
                      `Text.isInfixOf` l
                      && Text.pack
                        ( migrationName
                            ( addedSqlMig @IO
                                alwaysFailingMigNoTxn
                            )
                        )
                        `Text.isInfixOf` l
                )

        after (const cleanupAfterTest) $
          it "Diverse order of different types of migrations" $
            ioProperty $
              do
                defaultConnInfo <- testConnInfo
                testSettings <- testCoddSettings
                createCoddTestDbMigs :: AddedSqlMigration IO <-
                  createTestUserMigPol @IO
                pure
                  $ forAll
                    ( diversifyAppCheckMigs
                        defaultConnInfo
                        createCoddTestDbMigs
                    )
                  $ \(DiverseMigrationOrder allMigs) -> void $ do
                    runMigs :: [(Int, FilePath, Int64, Int)] <-
                      runCoddLogger $
                        applyMigrationsNoCheck
                          testSettings
                          ( Just $
                              map
                                (hoistAddedSqlMigration lift)
                                allMigs
                          )
                          testConnTimeout
                          ( \conn ->
                              liftIO $
                                DB.query
                                  conn
                                  "SELECT id, name, txnid, connid FROM codd_schema.sql_migrations ORDER BY applied_at, id"
                                  ()
                          )
                    -- Check all migrations were applied in order, and that ordering only by their Ids gives the same order
                    map (\(_, name, _, _) -> name) runMigs
                      `shouldBe` map
                        ( migrationName
                            . addedSqlMig
                        )
                        allMigs
                    runMigs
                      `shouldBeStrictlySortedOn` \(migid, _, _, _) ->
                        migid

                    map (\(_, name, _, _) -> name) runMigs
                      `shouldBe` map
                        ( migrationName
                            . addedSqlMig
                        )
                        allMigs
                    let migsForTests =
                          zipWith
                            ( \(migId, _, txnId, connId) mig ->
                                ( migId,
                                  fromMaybe defaultConnInfo $
                                    migrationCustomConnInfo $
                                      addedSqlMig mig,
                                  migrationInTxn $
                                    addedSqlMig mig,
                                  txnId,
                                  connId
                                )
                            )
                            runMigs
                            allMigs
                        everyPairOfMigs =
                          [ ( conStr1,
                              conStr2,
                              intxn1,
                              intxn2,
                              t1,
                              t2,
                              cid1,
                              cid2
                            )
                            | (migId1, conStr1, intxn1, t1, cid1) <-
                                migsForTests,
                              (migId2, conStr2, intxn2, t2, cid2) <-
                                migsForTests,
                              migId1 < migId2
                          ]
                        consecutiveMigs =
                          zipWith
                            ( \(migId1, conStr1, intxn1, t1, cid1) (migId2, conStr2, intxn2, t2, cid2) ->
                                ( conStr1,
                                  conStr2,
                                  intxn1,
                                  intxn2,
                                  t1,
                                  t2,
                                  cid1,
                                  cid2
                                )
                            )
                            migsForTests
                            (drop 1 migsForTests)

                    -- 1. Test that no two migrations with the same connection string use different connIds
                    everyPairOfMigs
                      `shouldSatisfy` all
                        ( \(conStr1, conStr2, _, _, _, _, cid1, cid2) ->
                            conStr1 /= conStr2 || cid1 == cid2
                        )
                    -- 2. Test that no two migrations with different connection strings use the same connIds
                    everyPairOfMigs
                      `shouldSatisfy` all
                        ( \(conStr1, conStr2, _, _, _, _, cid1, cid2) ->
                            conStr1 == conStr2 || cid1 /= cid2
                        )
                    -- 3. Test that no two consecutive in-txn migrations with the same connection string have different txnIds
                    consecutiveMigs
                      `shouldSatisfy` all
                        ( \(conStr1, conStr2, intxn1, intxn2, txnid1, txnid2, _, _) ->
                            conStr1
                              /= conStr2
                              || not intxn1
                              || not intxn2
                              || txnid1
                                == txnid2
                        )
                    -- 4. Test that no two no-txn migrations have the same txnId
                    consecutiveMigs
                      `shouldSatisfy` all
                        ( \(conStr1, conStr2, intxn1, intxn2, txnid1, txnid2, _, _) ->
                            intxn1 || intxn2 || txnid1 /= txnid2
                        )

-- | Concatenates two lists, generates a shuffle of that
-- that does not change relative order of elements when compared
-- to their original lists. The supplied function is called with
-- the final 0-based index of each element in the list and the
-- element itself to form the final generated list.
mergeShuffle :: [a] -> [a] -> (Int -> a -> b) -> Gen [b]
mergeShuffle ll1 ll2 f = go ll1 ll2 (0 :: Int)
  where
    go [] l2 i = pure $ zipWith f [i ..] l2
    go l1 [] i = pure $ zipWith f [i ..] l1
    go l1@(x : xs) l2@(y : ys) i = do
      yieldFirst <- arbitrary @Bool
      if yieldFirst
        then (f i x :) <$> go xs l2 (i + 1)
        else (f i y :) <$> go l1 ys (i + 1)

data MigToCreate = CreateCoddTestDb | CreatePassingMig Bool | CreatePassingMigDifferentUser Bool | CreateDbCreationMig Int

-- | Holds migrations that test codd_schema's internal management while migrations are applied.
-- Look at the `diversifyAppCheckMigs` function, which generates migrations that explore a combination space
-- with the intent of checking codd's migration application internals are robust.
newtype DiverseMigrationOrder m = DiverseMigrationOrder
  { migrationsInOrder :: [AddedSqlMigration m]
  }

instance Show (DiverseMigrationOrder m) where
  show (DiverseMigrationOrder migs) =
    concatMap (show . migrationName . addedSqlMig) migs

diversifyAppCheckMigs ::
  (MonadThrow m) =>
  DB.ConnectInfo ->
  AddedSqlMigration m ->
  Gen (DiverseMigrationOrder m)
diversifyAppCheckMigs defaultConnInfo createCoddTestDbMigs = do
  let postgresCinfo =
        defaultConnInfo
          { DB.connectDatabase = "postgres",
            DB.connectUser = "postgres"
          }

  numPassingMigs <- chooseInt (0, 5)
  numCreateOtherDbMigs <- chooseInt (0, 3)
  passingMigs <-
    QC.resize numPassingMigs $
      QC.listOf $
        QC.elements
          [ CreatePassingMig True,
            CreatePassingMig False,
            CreatePassingMigDifferentUser True,
            CreatePassingMigDifferentUser False
          ]

  migsInOrder <-
    fmap (fixMigsOrder . concat)
      $ mergeShuffle
        (CreateCoddTestDb : passingMigs)
        (map CreateDbCreationMig [0 .. numCreateOtherDbMigs])
      $ \migOrder migType -> case migType of
        CreateCoddTestDb -> [createCoddTestDbMigs]
        CreateDbCreationMig i ->
          [ AddedSqlMigration
              ( createDatabaseMig
                  postgresCinfo
                    { DB.connectDatabase = previousDbName
                    }
                  ("new_database_" <> show i)
                  0 {- no pg_sleep, another test already tests this -}
                  migOrder
              )
              (getIncreasingTimestamp 0)
            | previousDbName <-
                if i == 0
                  then ["postgres"]
                  else ["new_database_" <> show (i - 1)]
          ]
        CreatePassingMig inTxn ->
          [ AddedSqlMigration
              (createAlwaysPassingMig migOrder "passing-mig")
                { migrationInTxn = inTxn
                }
              (getIncreasingTimestamp 0)
          ]
        CreatePassingMigDifferentUser inTxn ->
          [ AddedSqlMigration
              ( createAlwaysPassingMig
                  migOrder
                  "passing-custom-user-mig"
              )
                { migrationCustomConnInfo =
                    Just
                      defaultConnInfo
                        { DB.connectUser = "codd-test-user"
                        },
                  migrationInTxn = inTxn
                }
              $ getIncreasingTimestamp 0
          ]
  pure $ DiverseMigrationOrder migsInOrder
