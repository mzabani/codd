module Codd.Internal where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO (MonadUnliftIO, throwIO, bracket, tryAny)
import UnliftIO.Concurrent (threadDelay)

-- | Tries to connect until a connection succeeds or until a timeout, executes the supplied action and disposes of the opened Connection.
connectAndDispose :: (MonadUnliftIO m, MonadIO m) => DB.ConnectInfo -> (DB.Connection -> m a) -> m a
connectAndDispose connStr action = go (50 :: Int) -- At most 50 * 100ms = 5 seconds
  where
      wrappedAction n eitherConn = do
          case eitherConn of
              Left e -> if n <= 0 then throwIO e else threadDelay (1000 * 100) >> go (n - 1)
              Right conn -> action conn
      go n = bracket (tryAny $ liftIO $ DB.connect connStr) (either (const $ pure ()) (liftIO . DB.close)) (wrappedAction n)