module Codd.Query
  ( InTxnT(..) -- TODO: we probably don't want to export the constructor as it can break the transaction sandbox if misused
  , InTxn
  , beginCommitTxnBracket
  , execvoid_
  , hoistInTxn
  , query
  , unsafeQuery1
  ) where

import           Codd.Parsing                   ( AddedSqlMigration
                                                , hoistAddedSqlMigration
                                                )
import           Codd.Types                     ( TxnIsolationLvl(..) )
import           Control.Monad                  ( void )
import           Control.Monad.Logger           ( MonadLogger )
import           Data.Kind                      ( Type )
import qualified Database.PostgreSQL.Simple    as DB
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
                                                , onException
                                                , toIO
                                                )

execvoid_ :: MonadIO m => DB.Connection -> DB.Query -> m ()
execvoid_ conn q = liftIO $ void $ DB.execute_ conn q

query
  :: (DB.FromRow b, MonadIO m, DB.ToRow a)
  => DB.Connection
  -> DB.Query
  -> a
  -> m [b]
query conn q r = liftIO $ DB.query conn q r

-- | Throws an exception if 0 or more than 1 results are returned.
unsafeQuery1
  :: (DB.FromRow b, MonadIO m, DB.ToRow a)
  => DB.Connection
  -> DB.Query
  -> a
  -> m b
unsafeQuery1 conn q r = liftIO $ do
  res <- DB.query conn q r
  case res of
    []  -> error "No results for query1"
    [x] -> return x
    _   -> error "More than one result for query1"


-- | Returns a Query with a valid "BEGIN" statement that is READ WRITE and has
-- the desired isolation level.
beginStatement :: TxnIsolationLvl -> DB.Query
beginStatement = \case
  DbDefault       -> "BEGIN READ WRITE"
  Serializable    -> "BEGIN READ WRITE,ISOLATION LEVEL SERIALIZABLE"
  RepeatableRead  -> "BEGIN READ WRITE,ISOLATION LEVEL REPEATABLE READ"
  ReadCommitted   -> "BEGIN READ WRITE,ISOLATION LEVEL READ COMMITTED"
  ReadUncommitted -> "BEGIN READ WRITE,ISOLATION LEVEL READ UNCOMMITTED"

class Monad m => InTxn (m :: Type -> Type)
newtype InTxnT m a = InTxnT { unTxnT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadUnliftIO, InTxn)

beginCommitTxnBracket
  :: (MonadUnliftIO m, MonadIO m)
  => TxnIsolationLvl
  -> DB.Connection
  -> InTxnT m a
  -> m a
beginCommitTxnBracket isolLvl conn (unTxnT -> f) = do
  iof <- toIO f
  liftIO $ do
    execvoid_ conn $ beginStatement isolLvl
    v <- iof `onException` DB.rollback conn -- TODO: Running `rollback` or any queries in generic exception handlers is not right!
    DB.commit conn
    pure v

hoistInTxn :: Monad m => AddedSqlMigration m -> AddedSqlMigration (InTxnT m)
hoistInTxn = hoistAddedSqlMigration InTxnT
