module Codd.Environment
    ( CoddSettings(..)
    , getAdminConnInfo
    , getCoddSettings
    , retryPolicyParser
    ) where

import           Codd.Hashing.Types             ( DbHashes )
import           Codd.Parsing                   ( AddedSqlMigration
                                                , connStringParser
                                                , parseWithEscapeCharProper
                                                )
import           Codd.Types                     ( ChecksumAlgo(..)
                                                , Include(..)
                                                , RetryBackoffPolicy(..)
                                                , RetryPolicy(..)
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
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           UnliftIO                       ( MonadIO(..) )
import           UnliftIO.Environment           ( lookupEnv )

data CoddSettings = CoddSettings
    { migsConnString   :: ConnectInfo
    -- ^ The Connection String which will be used to run migrations.
    , sqlMigrations    :: forall m . Either [FilePath] [AddedSqlMigration m]
    -- ^ A list of directories with .sql files or a list of ordered Sql Migrations.
    --   When using Directory Paths, all .sql files from all directories are collected into a single list and then run in alphabetical order. Files whose names don't end in .sql are ignored.
    , onDiskHashes     :: Either FilePath DbHashes
    -- ^ The directory where DB hashes are persisted to when SQL migrations are applied. In a valid setup, this should always match the Hashes obtained from the Database,
    -- (perhaps only after applying migrations when deploying).
    , schemasToHash    :: Include SqlSchema
    -- ^ Selection of Schemas in the DB that we should hash.
    , extraRolesToHash :: Include SqlRole
    -- ^ Selection of Roles to hash. You usually need to include at least the App User. The super user from migsConnString is always included in hashing automatically and needs not be added here.
    , retryPolicy      :: RetryPolicy
    -- ^ The Retry Policy to be used when applying failing migrations.
    , txnIsolationLvl  :: TxnIsolationLvl
    -- ^ Transaction isolation level to be used when applying migrations.
    , checksumAlgo     :: ChecksumAlgo
    -- ^ Fine tuning that changes the checksum algorithm.
    , hashedChecksums  :: Bool
    -- ^ Instead of computing MD5 hashes of DB objects, you can store/use the string composed by Codd without hashing it
    -- by setting this to False.
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

checksumAlgoParser :: Parser ChecksumAlgo
checksumAlgoParser = do
    modifiers <- validModifier `Parsec.sepBy'` char ' '
    pure ChecksumAlgo
        { strictCollations         = collations `elem` modifiers
        , strictRangeCtorOwnership = rangeCtorOwnership `elem` modifiers
        , ignoreColumnOrder        = ignoreColOrder `elem` modifiers
        }

  where
    collations         = "strict-collations"
    rangeCtorOwnership = "strict-range-ctor-ownership"
    ignoreColOrder     = "ignore-column-order"
    validModifier =
        string collations
            <|> string rangeCtorOwnership
            <|> string ignoreColOrder

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
    onDiskHashesDir <- Text.unpack <$> readEnv "CODD_CHECKSUM_DIR"
    schemasToHash   <- parseEnv
        (error
            "Please define the CODD_SCHEMAS environment variable with a space separated list of schema names"
        )
        (fmap (Include . map SqlSchema) . parseVar spaceSeparatedObjNameParser)
        "CODD_SCHEMAS"
    extraRolesToHash <- parseEnv
        (Include [])
        (fmap (Include . map SqlRole) . parseVar spaceSeparatedObjNameParser)
        "CODD_EXTRA_ROLES"
    retryPolicy <- parseEnv defaultRetryPolicy
                            (parseVar retryPolicyParser)
                            "CODD_RETRY_POLICY"
    txnIsolationLvl <- parseEnv DbDefault
                                (parseVar txnIsolationLvlParser)
                                "CODD_TXN_ISOLATION"
    checksumAlgo <- parseEnv (ChecksumAlgo False False False)
                             (parseVar checksumAlgoParser)
                             "CODD_CHECKSUM_ALGO"
    pure CoddSettings { migsConnString   = adminConnInfo
                      , sqlMigrations    = Left sqlMigrationPaths
                      , onDiskHashes     = Left onDiskHashesDir
                      , schemasToHash    = schemasToHash
                      , extraRolesToHash = extraRolesToHash
                      , retryPolicy      = retryPolicy
                      , txnIsolationLvl  = txnIsolationLvl
                      , checksumAlgo     = checksumAlgo
                      , hashedChecksums  = True
                      }

  where
    parseVar parser =
        first Text.pack . parseOnly (parser <* endOfInput) . Text.pack

