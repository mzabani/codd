module Codd.Query
    ( InTxnT
    , CanStartTxn
    , InTxn
    , NotInTxn
    , execvoid_
    , query
    , txnStatus
    , unsafeQuery1
    , queryMay
    , withTransaction
    ) where

import           Codd.Logging                   ( CoddLogger
                                                , LoggingT
                                                )
import           Codd.Types                     ( TxnIsolationLvl(..) )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Monad.Except           ( ExceptT )
import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.State            ( StateT )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Control.Monad.Trans.Writer     ( WriterT )
import           Data.Kind                      ( Type )
import qualified Database.PostgreSQL.LibPQ     as PQ
import qualified Database.PostgreSQL.Simple    as DB
import qualified Database.PostgreSQL.Simple.Internal
                                               as PGInternal
import           UnliftIO                       ( MonadIO(..)
                                                , MonadUnliftIO
                                                , onException
                                                )
import           UnliftIO.Resource              ( ResourceT )

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

-- | Throws an exception if more one result is returned by the query.
queryMay
    :: (DB.FromRow b, MonadIO m, DB.ToRow a)
    => DB.Connection
    -> DB.Query
    -> a
    -> m (Maybe b)
queryMay conn q r = liftIO $ do
    res <- DB.query conn q r
    case res of
        []  -> pure Nothing
        [x] -> pure $ Just x
        _   -> error "More than one result for queryMay"


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

Of course, any type level constraints we devise must assume the user will not call `execute conn "BEGIN"`, `execute conn "COMMIT"`,
or similar statements, but rather that they'll use functions exposed by this module to manage transactions.
And with that little bit of discipline, we should be able to moderately achieve goals 1 to 3, where "moderately" means
this is not necessarily an airtight sandbox, but ways to break out of it should be harder in codd's codebase.

One existing exception to this airtightness is handling multiple connections at once. It's not hard to do something like
`withTransaction @txn isolLvl conn1 $ someInTxnTFunc conn2` when conn2 is in fact not in a transaction.

We want to address this hole eventually.
-}

class Monad m => InTxn (m :: Type -> Type)
class Monad m => NotInTxn (m :: Type -> Type)
newtype InTxnT m a = InTxnT { unTxnT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, CoddLogger, MonadUnliftIO, InTxn)


-- | TODO: Is this instance a way of breaking this module's sandbox? Check!
instance MonadTrans InTxnT where
    lift = InTxnT

-- 1. First we start with our basic assumptions: `IO` has no open transactions, and `SomeMonadTransformerT IO` also doesn't.
-- We do this for some common monad transformers to increase compatibility.
instance NotInTxn IO
instance NotInTxn m => NotInTxn (LoggingT m)
instance NotInTxn m => NotInTxn (ResourceT m)
instance NotInTxn m => NotInTxn (ReaderT r m)
instance NotInTxn m => NotInTxn (StateT s m)
instance NotInTxn m => NotInTxn (ExceptT e m)
instance (NotInTxn m, Monoid w) => NotInTxn (WriterT w m)

-- 2. Next, if some monad `m` is inside a transaction, `SomeMonadTransformerT m` also is.
-- We could add an instance with with `MonadTrans t => InTxn (t m)`, but that would require XUndecidableInstances.
-- I wonder if it's worth it?
-- instance (InTxn m, MonadTrans t, Monad (t m)) => InTxn (t m)
instance InTxn m => InTxn (LoggingT m)
instance InTxn m => InTxn (ResourceT m)
instance InTxn m => InTxn (ReaderT r m)
instance InTxn m => InTxn (StateT s m)
instance InTxn m => InTxn (ExceptT e m)
instance (InTxn m, Monoid w) => InTxn (WriterT w m)

-- 3. Now we want the type-level trickery to let us infer from class constraints if we're inside an `InTxn` monad or not.
-- There are possibly many ways to go about this with GHC. I'm not well versed in them.
-- My ~first~ second attempt is to use multi-parameter classes to avoid duplicate instances (since instance heads are ignored
-- for instance selection).
-- However, in some contexts where the `txn` monad type appears only in class constraints, but not in the function's arguments, using
-- `withTransaction` will require enabling AllowAmbiguousTypes and explicit type applications like `withTransaction @txn`.
-- This seems kind of acceptable. Other common idioms will be adding constraints such as `(NotInTxn m, txn ~ InTxnT (ResourceT m))`,
-- which provides a type argument that can be used for arguments like functions `(Connection -> txn a)`.

data CheckTxnWit m txn where
  AlreadyInTxn :: CheckTxnWit m m -- Proof that `m ~ txn`
  NotInTxn :: CheckTxnWit m (InTxnT m) -- Proof that `InTxnT m ~ txn`

-- We maybe would be able to better guide type inference by adding a functional dependency from m -> txn.
-- However, that creates a conflict in instance resolution for the next two `CanStartTxn` instances, probably
-- because instance heads are disconsidered.
class (InTxn txn) => CanStartTxn (m :: Type -> Type) (txn :: Type -> Type) where
  txnCheck :: m (CheckTxnWit m txn)

instance InTxn m => CanStartTxn m m where
    txnCheck = pure AlreadyInTxn

instance NotInTxn m => CanStartTxn m (InTxnT m) where
    txnCheck = pure NotInTxn

-- | Runs a function inside a read-write transaction of the desired isolation level,
-- BEGINning the transaction if not in one, or just running the supplied function otherwise,
-- even if you are in a different isolation level than the one supplied.
-- If not in a transaction, commits after running `f`, but only if the transaction is still active. Does not commit otherwise.
withTransaction
    :: forall txn m a
     . (MonadUnliftIO m, CanStartTxn m txn)
    => TxnIsolationLvl
    -> DB.Connection
    -> txn a
    -> m a
withTransaction isolLvl conn f = do
    t :: CheckTxnWit m txn <- txnCheck
    case t of
        AlreadyInTxn -> assertTxnStatus PQ.TransInTrans >> f
        NotInTxn     -> do
            assertTxnStatus PQ.TransIdle
            execvoid_ conn $ beginStatement isolLvl
            -- Note: once we stop rolling back on exception here, we can relax this function's `MonadUnliftIO`
            -- constraint to just `MonadIO`
            v           <- unTxnT f `onException` liftIO (DB.rollback conn)
            transStatus <- txnStatus conn
            when (transStatus == PQ.TransInTrans) $ liftIO $ DB.commit conn
            pure v
  where
    assertTxnStatus :: MonadUnliftIO n => PQ.TransactionStatus -> n ()
    assertTxnStatus s = do
        actualStatus <- txnStatus conn
        when (actualStatus /= s)
            $  error
            $  "Internal error in codd. We were expecting txnStatus "
            ++ show s
            ++ " but got "
            ++ show actualStatus
            ++ ". Please report this as a bug"

txnStatus :: MonadUnliftIO m => DB.Connection -> m PQ.TransactionStatus
txnStatus conn = liftIO $ PGInternal.withConnection conn PQ.transactionStatus
