module Codd.Environment
    ( CoddSettings(..)
    , connStringParser
    , getAdminConnInfo
    , getCoddSettings
    , retryPolicyParser
    , superUserInAppDatabaseConnInfo
    ) where

import           Codd.Hashing.Types             ( DbHashes )
import           Codd.Parsing                   ( AddedSqlMigration
                                                , parseMigrationTimestamp
                                                )
import           Codd.Types                     ( DeploymentWorkflow(..)
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
                                                , decimal
                                                , endOfInput
                                                , parseOnly
                                                , peekChar
                                                , skipSpace
                                                , string
                                                )
import qualified Data.Attoparsec.Text          as Parsec
import           Data.Bifunctor                 ( first )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( secondsToNominalDiffTime )
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           UnliftIO                       ( MonadIO(..) )
import           UnliftIO.Environment           ( lookupEnv )

data CoddSettings = CoddSettings
    { dbName              :: Text
    -- ^ The name of the Database the Application will connect to
    , superUserConnString :: ConnectInfo
    -- ^ A Connection String which has the power to create databases, grant privileges and a lot more.
    , sqlMigrations       :: Either [FilePath] [AddedSqlMigration]
    -- ^ A list of directories with .sql files or a list of ordered Sql Migrations.
    --   When using Directory Paths, all .sql files from all directories are collected into a single list and then run in alphabetical order. Files whose names don't end in .sql are ignored.
    , onDiskHashes        :: Either FilePath DbHashes
    -- ^ The directory where DB hashes are persisted to when SQL migrations are applied. In a valid setup, this should always match the Hashes obtained from the Database,
    -- (perhaps only after applying migrations when deploying).
    , deploymentWorkflow  :: DeploymentWorkflow
    -- ^ Simple or Blue-Green-Safe deployment workflow? Simple means no destructive sections are allowed for any migrations and Blue-Green-Safe means
    -- developers have to keep a file which points to the timestamp of the last migration up to which and including it destructive sections must run the next time.
    , schemasToHash       :: Include SqlSchema
    -- ^ Selection of Schemas in the DB that we should hash.
    , extraRolesToHash    :: Include SqlRole
    -- ^ Selection of Roles to hash. You usually need to include at least the App User. The super user from superUserConnString is always included in hashing automatically and needs not be added here.
    , retryPolicy         :: RetryPolicy
    -- ^ The Retry Policy to be used when applying failing migrations.
    , txnIsolationLvl     :: TxnIsolationLvl
    -- ^ Transaction isolation level to be used when applying migrations.
    }

-- | Parses a value using backslash as an escape char for any char that matches
-- the supplied predicate. Stops at and does not consume the first predicate-passing
-- char.
parseWithEscapeChar :: (Char -> Bool) -> Parser Text
parseWithEscapeChar untilc = do
    cs       <- Parsec.takeWhile (\c -> c /= '\\' && not (untilc c))
    nextChar <- peekChar
    case nextChar of
        Nothing   -> pure cs
        Just '\\' -> do
            void $ char '\\'
            c    <- Parsec.take 1
            rest <- parseWithEscapeChar untilc
            pure $ cs <> c <> rest
        Just _ -> pure cs

-- | Parses a string in the format protocol://username[:password]@host:port/database_name
connStringParser :: Parser ConnectInfo
connStringParser = do
    void $ string "postgres://"
    usr <- idParser "username"
    pwd <- (char ':' *> idParser "password") <|> pure ""
    void $ char '@'
    host <- idParser "host" -- TODO: IPv6 addresses such as ::1 ??
    void $ char ':'
    port <- decimal <|> fail "Could not find a port in the connection string."
    void $ char '/'
    adminDb <- idParser "database"
    pure ConnectInfo { connectHost     = host
                     , connectPort     = port
                     , connectUser     = usr
                     , connectPassword = pwd
                     , connectDatabase = adminDb
                     }
  where
    idParser :: String -> Parser String
    idParser idName = do
        x <- Text.unpack <$> parseWithEscapeChar (\c -> c == ':' || c == '@')
        when (x == "")
            $  fail
            $  "Could not find a "
            <> idName
            <> " in the connection string."
        pure x

-- | Considers backslash as an espace character for space.
spaceSeparatedObjNameParser :: Parser [Text]
spaceSeparatedObjNameParser = do
    v <- parseWithEscapeChar (== ' ')
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
        secondsToNominalDiffTime
            <$> (Parsec.rational <* char 's')
            <|> secondsToNominalDiffTime
            .   (/ 1000)
            <$> (Parsec.rational <* string "ms")

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
    adminConnStr <- readEnv "CODD_ADMIN_CONNECTION"
    let connInfoE = parseOnly (connStringParser <* endOfInput) adminConnStr
    case connInfoE of
        Left err ->
            error
                $ "Error parsing the connection string in environment variable CODD_ADMIN_CONNECTION: "
                ++ err
        Right connInfo -> pure connInfo

getCoddSettings :: MonadIO m => m CoddSettings
getCoddSettings = do
    adminConnInfo     <- getAdminConnInfo
    appDbName         <- readEnv "CODD_APPDB"
    sqlMigrationPaths <- map Text.unpack . Text.splitOn ":" <$> readEnv
        "CODD_MIGRATION_DIRS" -- No escaping colons in PATH (really?) so no escaping here either
    onDiskHashesDir <- Text.unpack <$> readEnv "CODD_CHECKSUM_DIR"
    destructiveUpTo <- parseEnv
        SimpleDeployment
        (fmap BlueGreenSafeDeploymentUpToAndIncluding . parseMigrationTimestamp)
        "CODD_DESTROY_UP_TO_AND_INCLUDING"
    schemasToHash <- parseEnv
        (error
            "Please define the CODD_SCHEMAS environment variable with a space separated list of schema names"
        )
        (fmap (Include . map SqlSchema) . parseVar spaceSeparatedObjNameParser)
        "CODD_SCHEMAS"
    extraRolesToHash <- parseEnv
        (error
            "Please define the CODD_EXTRA_ROLES environment variable with a space separated list of extra roles"
        )
        (fmap (Include . map SqlRole) . parseVar spaceSeparatedObjNameParser)
        "CODD_EXTRA_ROLES"
    retryPolicy <- parseEnv defaultRetryPolicy
                            (parseVar retryPolicyParser)
                            "CODD_RETRY_POLICY"
    txnIsolationLvl <- parseEnv DbDefault
                                (parseVar txnIsolationLvlParser)
                                "CODD_TXN_ISOLATION"
    pure CoddSettings { dbName              = appDbName
                      , superUserConnString = adminConnInfo
                      , sqlMigrations       = Left sqlMigrationPaths
                      , onDiskHashes        = Left onDiskHashesDir
                      , deploymentWorkflow  = destructiveUpTo
                      , schemasToHash       = schemasToHash
                      , extraRolesToHash    = extraRolesToHash
                      , retryPolicy         = retryPolicy
                      , txnIsolationLvl     = txnIsolationLvl
                      }

  where
    parseVar parser =
        first Text.pack . parseOnly (parser <* endOfInput) . Text.pack

-- | Returns a `ConnectInfo` that will connect to the App's Database with the Super User's credentials.
superUserInAppDatabaseConnInfo :: CoddSettings -> ConnectInfo
superUserInAppDatabaseConnInfo CoddSettings { superUserConnString, dbName } =
    superUserConnString { connectDatabase = Text.unpack dbName }
