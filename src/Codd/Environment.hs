module Codd.Environment
    ( CoddSettings(..)
    , getAdminConnInfo
    , getCoddSettings
    , parseEnv
    , retryPolicyParser
    ) where

import           Codd.Parsing                   ( connStringParser
                                                , parseWithEscapeCharProper
                                                )
import           Codd.Representations.Types     ( DbRep )
import           Codd.Types                     ( RetryBackoffPolicy(..)
                                                , RetryPolicy(..)
                                                , SchemaAlgo(..)
                                                , SchemaSelection
                                                    ( AllNonInternalSchemas
                                                    , IncludeSchemas
                                                    )
                                                , SqlRole(..)
                                                , SqlSchema(..)
                                                , TxnIsolationLvl(..)
                                                , defaultRetryPolicy
                                                )
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , char
                                                , endOfInput
                                                , parseOnly
                                                , skipSpace
                                                , string
                                                )
import qualified Data.Attoparsec.Text          as Parsec
import           Data.Bifunctor                 ( first )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           UnliftIO                       ( MonadIO(..) )
import           UnliftIO.Environment           ( lookupEnv )

data CoddSettings = CoddSettings
    { migsConnString    :: ConnectInfo
    -- ^ The Connection String which will be used to run migrations.
    , sqlMigrations     :: [FilePath]
    -- ^ A list of directories with .sql files.
    --   All .sql files from all directories are collected into a single list and then run in alphabetical order. Files whose names don't end in .sql are ignored.
    , onDiskReps        :: Either FilePath DbRep
    -- ^ The directory with the expected DB schema representations, or the expected representations themselves.
    , namespacesToCheck :: SchemaSelection
    -- ^ Selection of Schemas in the DB that codd should check.
    , extraRolesToCheck :: [SqlRole]
    -- ^ Selection of Roles to check. The super user from migsConnString is automatically checked and needs not be added here.
    , retryPolicy       :: RetryPolicy
    -- ^ The Retry Policy to be used when applying failing migrations.
    , txnIsolationLvl   :: TxnIsolationLvl
    -- ^ Transaction isolation level to be used when applying migrations.
    , schemaAlgoOpts    :: SchemaAlgo
    -- ^ Fine tuning that changes the schema extraction algorithm.
    }

-- | Considers backslash as an espace character for space.
spaceSeparatedObjNameParser :: Parser [Text]
spaceSeparatedObjNameParser = do
    v <- parseWithEscapeCharProper (== ' ')
    skipSpace
    (endOfInput *> pure [v]) <|> fmap (v :) spaceSeparatedObjNameParser

-- | Parses a retry policy in the format "max MAXRETRIES backoff (constant|exponential) TIME(s|ms)"
retryPolicyParser :: Parser RetryPolicy
retryPolicyParser = do
    void $ string "max"
    skipSpace
    maxRetries <- Parsec.decimal
    when (maxRetries < 0) $ fail "Max retries must be at least 0 (zero)"
    skipSpace
    void $ string "backoff"
    skipSpace
    backoff <- backoffParser
    skipSpace
    interval <- timeParser
    pure $ RetryPolicy maxRetries (backoff interval)
  where
    backoffParser =
        string "constant"
            *>  pure ConstantBackoff
            <|> string "exponential"
            *>  pure ExponentialBackoff
            <|> fail "Backoff must be constant or exponential"
    timeParser =
        realToFrac
            <$> (Parsec.rational @Double <* char 's')
            <|> realToFrac
            .   (/ 1000)
            <$> (Parsec.rational @Double <* string "ms")

txnIsolationLvlParser :: Parser TxnIsolationLvl
txnIsolationLvlParser =
    string "db-default"
        *>  pure DbDefault
        <|> string "serializable"
        *>  pure Serializable
        <|> string "repeatable read"
        *>  pure RepeatableRead
        <|> string "read committed"
        *>  pure ReadCommitted
        <|> string "read uncommitted"
        *>  pure ReadUncommitted

schemaAlgoOptsParser :: Parser SchemaAlgo
schemaAlgoOptsParser = do
    modifiers <- validModifier `Parsec.sepBy'` char ' '
    pure SchemaAlgo { strictCollations     = collations `elem` modifiers
                    , strictRangeCtorPrivs = rangeCtorPrivs `elem` modifiers
                    , ignoreColumnOrder    = ignoreColOrder `elem` modifiers
                    }

  where
    collations     = "strict-collations"
    rangeCtorPrivs = "strict-range-ctor-privs"
    ignoreColOrder = "ignore-column-order"
    validModifier =
        string collations <|> string rangeCtorPrivs <|> string ignoreColOrder

readEnv :: MonadIO m => String -> m Text
readEnv var =
    maybe (error $ "Could not find environment variable '" ++ var ++ "'")
          Text.pack
        <$> lookupEnv var

parseEnv :: MonadIO m => a -> (String -> Either Text a) -> String -> m a
parseEnv defaultValue parser var = do
    e <- lookupEnv var
    case e of
        Nothing -> pure defaultValue
        Just v  -> case parser v of
            Left err ->
                error
                    $  "Error parsing environment variable '"
                    ++ var
                    ++ "': "
                    ++ Text.unpack err
            Right x -> pure x



getAdminConnInfo :: MonadIO m => m ConnectInfo
getAdminConnInfo = do
    adminConnStr <- readEnv "CODD_CONNECTION"
    let connInfoE = parseOnly (connStringParser <* endOfInput) adminConnStr
    case connInfoE of
        Left err ->
            error
                $ "Error parsing the connection string in environment variable CODD_CONNECTION: "
                ++ err
        Right connInfo -> pure connInfo

getCoddSettings :: MonadIO m => m CoddSettings
getCoddSettings = do
    adminConnInfo     <- getAdminConnInfo
    sqlMigrationPaths <- map Text.unpack . Text.splitOn ":" <$> readEnv
        "CODD_MIGRATION_DIRS" -- No escaping colons in PATH (really?) so no escaping here either

    -- Only temporarily accept the old env var name for this to smooth transition for current users
    onDiskRepsDir :: String <-
        ((,) <$> lookupEnv "CODD_EXPECTED_SCHEMA_DIR" <*> lookupEnv
                "CODD_CHECKSUM_DIR"
            )
            <&> \case
                    (Nothing, Nothing) ->
                        error
                            "Could not find \"CODD_EXPECTED_SCHEMA_DIR\" environment variable"
                    (Just _, Just _) ->
                        error
                            "Found both \"CODD_EXPECTED_SCHEMA_DIR\" and \"CODD_CHECKSUM_DIR\" but can there only be one"
                    (Just d , Nothing) -> d
                    (Nothing, Just d ) -> d
    namespacesToCheck <- parseEnv
        AllNonInternalSchemas
        ( fmap (IncludeSchemas . map SqlSchema)
        . parseVar spaceSeparatedObjNameParser
        )
        "CODD_SCHEMAS"
    extraRolesToCheck <- parseEnv
        []
        (fmap (map SqlRole) . parseVar spaceSeparatedObjNameParser)
        "CODD_EXTRA_ROLES"
    retryPolicy <- parseEnv defaultRetryPolicy
                            (parseVar retryPolicyParser)
                            "CODD_RETRY_POLICY"
    txnIsolationLvl <- parseEnv DbDefault
                                (parseVar txnIsolationLvlParser)
                                "CODD_TXN_ISOLATION"
    schemaAlgoOpts <- parseEnv (SchemaAlgo False False False)
                               (parseVar schemaAlgoOptsParser)
                               "CODD_SCHEMA_ALGO"
    pure CoddSettings { migsConnString    = adminConnInfo
                      , sqlMigrations     = sqlMigrationPaths
                      , onDiskReps        = Left onDiskRepsDir
                      , namespacesToCheck = namespacesToCheck
                      , extraRolesToCheck = extraRolesToCheck
                      , retryPolicy       = retryPolicy
                      , txnIsolationLvl   = txnIsolationLvl
                      , schemaAlgoOpts    = schemaAlgoOpts
                      }

  where
    parseVar parser =
        first Text.pack . parseOnly (parser <* endOfInput) . Text.pack

