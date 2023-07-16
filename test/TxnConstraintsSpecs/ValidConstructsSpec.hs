module TxnConstraintsSpecs.ValidConstructsSpec
    ( spec
    ) where
import           Codd.Query                     ( InTxn
                                                , InTxnT
                                                , NotInTxn
                                                , withTransaction
                                                )
import           Codd.Types                     ( TxnIsolationLvl(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Logger           ( LoggingT(..)
                                                , runStderrLoggingT
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import           Test.Hspec
import UnliftIO (MonadUnliftIO)

{-|
There is no need to run actual tests in this module, and it should be fine to use
`undefined` for connections and other silly things: we are only interested that
this module compiles, since these are checks that our transaction management
type-level sandbox works as intended.
-}

spec :: Spec
spec = describe "Valid constructs with Txn class constraints" $ pure ()

someConn :: DB.Connection
someConn = undefined

-- | Starting a transaction with `IO` as base monad should be possible.
_withTxnFromIO :: IO ()
_withTxnFromIO = withTransaction @(InTxnT IO) DbDefault someConn $ pure ()

_testFnInTxnT :: InTxnT IO ()
_testFnInTxnT = withTransaction @(InTxnT IO) DbDefault someConn $ pure ()

_testFnWithLoggerAsBase :: LoggingT IO ()
_testFnWithLoggerAsBase =
    withTransaction @(InTxnT (LoggingT IO)) DbDefault someConn $ pure ()

_testFnWithInTxnTAsBase :: InTxnT IO ()
_testFnWithInTxnTAsBase =
    runStderrLoggingT
        $ withTransaction @(LoggingT (InTxnT IO)) DbDefault someConn
        $ pure ()

_canStartFromNoTxnByChangingMonad
    :: forall m . (MonadUnliftIO m, NotInTxn m) => m ()
_canStartFromNoTxnByChangingMonad =
    withTransaction @(InTxnT m) DbDefault someConn $ pure ()

_canStartFromInTxnSameMonad :: forall m . (MonadUnliftIO m, InTxn m) => m ()
_canStartFromInTxnSameMonad = withTransaction @m DbDefault someConn $ pure ()
