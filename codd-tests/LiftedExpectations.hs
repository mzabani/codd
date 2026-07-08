module LiftedExpectations
    ( shouldThrow
    ) where

import           Test.Hspec                     ( HasCallStack
                                                , Selector
                                                )
import qualified Test.Hspec                    as Hspec
import           UnliftIO                       ( Exception
                                                , MonadIO(liftIO)
                                                , MonadUnliftIO
                                                , toIO
                                                )

infix 1 `shouldThrow`
shouldThrow
    :: (HasCallStack, MonadUnliftIO m, MonadIO m, Exception e)
    => m a
    -> Selector e
    -> m ()
shouldThrow act sel = do
    ioAct <- toIO act
    liftIO $ ioAct `Hspec.shouldThrow` sel
