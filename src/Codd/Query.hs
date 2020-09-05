module Codd.Query where

import Control.Monad (void)
import qualified Database.PostgreSQL.Simple as DB
import UnliftIO (MonadIO(..))

execvoid_ :: MonadIO m => DB.Connection -> DB.Query -> m ()
execvoid_ conn q = liftIO $ void $ DB.execute_ conn q

query :: (MonadIO m, DB.ToRow a, DB.FromRow b) => DB.Connection -> DB.Query -> a -> m [b]
query conn q r = liftIO $ DB.query conn q r

-- | Throws an exception if 0 or more than 1 results are returned.
unsafeQuery1 :: (MonadIO m, DB.ToRow a, DB.FromRow b) => DB.Connection -> DB.Query -> a -> m b
unsafeQuery1 conn q r = liftIO $ do
    res <- DB.query conn q r
    case res of
        [] -> error "No results for query1"
        (x:[]) -> return x
        _ -> error "More than one result for query1"