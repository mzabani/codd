module Codd.Query
  ( InTxnT(..) -- TODO: we probably don't want to export the constructor as it can break the transaction sandbox if misused
  , CanStartTxn
  , InTxn
  , NotInTxn
  , beginCommitTxnBracket
  , execvoid_
  , hoistInTxn
  , query
  , unsafeQuery1
  , withTxnIfNecessary
  ) where

import           Codd.Parsing                   ( AddedSqlMigration
                                                , hoistAddedSqlMigration
                                                )
import           Codd.Types                     ( TxnIsolationLvl(..) )
import           Control.Monad                  ( void )
import           Control.Monad.Logger           ( MonadLogger, LoggingT )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Data.Kind                      ( Type )
import qualified Database.PostgreSQL.Simple    as DB
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
                                                , onException
                                                , toIO
                                                )
import UnliftIO.Resource (ResourceT)

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

{-|
We want to allow functions to specify the following constraints:
1. That they must be called from within an already open transaction.
2. That they must be called not inside a transaction.
3. That they don't care if they're in a transaction, but they want to be able to open one if they're not in one.

Of course, any type level constraints we devise must assume the user will not call `execute conn "BEGIN"` or `execute conn "COMMIT"`
or similar statements, but rather that they'll use functions exposed by this module to do that.
With that little bit of discipline, we should be able to achieve our goal.
-}

class Monad m => InTxn (m :: Type -> Type)
class Monad m => NotInTxn (m :: Type -> Type)
newtype InTxnT m a = InTxnT { unTxnT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadUnliftIO, InTxn)

instance MonadTrans InTxnT where
  lift = InTxnT

-- 1. First we start with our basic assumptions: `IO` has no open transactions, and `SomeMonadTransformerT IO` also don't.
-- Ideally we wouldn't add each MonadTransformerT instance here manually, but rather something `MonadTrans` based?
-- Need to look into `MonadTrans` again.
instance NotInTxn IO
instance NotInTxn m => NotInTxn (LoggingT m)
instance NotInTxn m => NotInTxn (ResourceT m)

-- 2. Next, if some monad `m` is inside a transaction, `SomeMonadTransformerT m` also is.
-- Here too the comment from `1` about `MonadTrans` might apply.
instance InTxn m => InTxn (LoggingT m)
instance InTxn m => InTxn (ResourceT m)

-- 3. Now we want functions running in some monad `m` to specify another type parameter `txn` that is either equal to `m`
-- if `m` is in a transaction, or `InTxnT m` otherwise, and allow these functions to start a transaction in the latter.
-- We also want type inference to work well.
-- There are possibly many ways to go about this with GHC. I'm not well versed in them.
-- My ~first~ second attempt is to use we use multi-parameter classes to avoid duplicate instances (since instance heads are ignored).
-- However, type inference with those is bad (GHC doesn't know which of the two `CanStartTxn m` instances to select) and thus requires
-- explicit type applications in _all_ cases with `withTxnIfNecessary`, or enabling AllowAmbiguousTypes, which is required
-- for polymorphic functions anyway.

data CheckTxnFancy m txn where
  AlreadyInTxn :: CheckTxnFancy m m -- Proof that `m ~ txn`
  NotInTxn :: CheckTxnFancy m (InTxnT m) -- Proof that `InTxnT m ~ txn`

-- | We maybe would be able to better guide type inference by adding a functional dependency from m -> txn.
-- However, that creates a conflict between the
class (InTxn txn) => CanStartTxn (m :: Type -> Type) (txn :: Type -> Type) where
  txnCheck :: m (CheckTxnFancy m txn)

-- instance CanStartTxn IO (InTxnT IO) where
--   txnCheck = pure NotInTxn

instance InTxn m => CanStartTxn m m where
  txnCheck = pure AlreadyInTxn

instance NotInTxn m => CanStartTxn m (InTxnT m) where
  txnCheck = pure NotInTxn

-- How do we do other instances?

beginCommitTxnBracket
  :: (MonadUnliftIO m, NotInTxn m)
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

-- simpleExample :: IO ()
-- simpleExample = beginCommitTxnBracket DbDefault undefined $ pure ()

-- simpleExampleLoggingT :: IO ()
-- simpleExampleLoggingT = beginCommitTxnBracket DbDefault undefined $ runStderrLoggingT $ pure ()

-- | Runs a function inside a read-write transaction of the desired isolation level,
-- BEGINning the transaction if not in one, or just running the supplied function otherwise,
-- even if you are in a different isolation level than the one supplied.
-- If not in a transaction, commits after running `f`. Does not commit otherwise.
withTxnIfNecessary
  :: forall txn m a. (MonadUnliftIO m, CanStartTxn m txn)
  => TxnIsolationLvl
  -> DB.Connection
  -> txn a
  -> m a
withTxnIfNecessary isolLvl conn f = do
  t :: CheckTxnFancy m txn <- txnCheck
  case t of
    AlreadyInTxn -> f -- No `rollback` because it'd be bad and/or because the function that already started the transaction might do that anyway
    NotInTxn -> do
      iof <- toIO $ unTxnT f
      liftIO $ do
        execvoid_ conn $ beginStatement isolLvl
        v <- iof `onException` DB.rollback conn -- TODO: Running `rollback` or any queries in generic exception handlers is not right!
        DB.commit conn
        pure v


-- testFnIO :: IO ()
-- testFnIO = withTxnIfNecessary @(InTxnT IO) DbDefault undefined $ pure ()

-- testFnInTxnT :: InTxnT IO ()
-- testFnInTxnT = withTxnIfNecessary @(InTxnT IO) DbDefault undefined $ pure ()

-- testFnWithLoggerAsBase :: LoggingT IO ()
-- testFnWithLoggerAsBase = withTxnIfNecessary @(InTxnT (LoggingT IO)) DbDefault undefined $ pure ()

-- testFnWithInTxnTAsBase :: InTxnT IO ()
-- testFnWithInTxnTAsBase = runStderrLoggingT $ withTxnIfNecessary @(LoggingT (InTxnT IO)) DbDefault undefined $ pure ()



hoistInTxn :: (Monad m, MonadTrans t, Monad (t m)) => AddedSqlMigration m -> AddedSqlMigration (t m)
hoistInTxn = hoistAddedSqlMigration lift
