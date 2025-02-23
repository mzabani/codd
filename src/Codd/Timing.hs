module Codd.Timing (prettyPrintDuration, timeAction) where

import Control.Monad.IO.Unlift (MonadIO (..))
import Data.Text (Text)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import qualified Formatting as Fmt
import System.Clock (Clock (..), TimeSpec (..), getTime)

pico_1ns :: Integer
pico_1ns = 1_000

pico_1ms :: Double
pico_1ms = 1_000_000_000

pico_1s :: forall a. (Num a) => a
pico_1s = 1_000_000_000_000

prettyPrintDuration :: DiffTime -> Text
prettyPrintDuration (fromIntegral @Integer @Double . diffTimeToPicoseconds -> dps)
  | dps < pico_1ms =
      Fmt.sformat
        (Fmt.fixed @Double 2)
        (fromIntegral @Integer (round (100 * dps / pico_1ms)) / 100)
        <> "ms"
  | -- e.g. 0.23ms
    dps < pico_1s =
      Fmt.sformat (Fmt.int @Integer) (round $ dps / pico_1ms) <> "ms"
  | -- e.g. 671ms
    otherwise =
      Fmt.sformat
        (Fmt.fixed @Double 1)
        (fromIntegral @Integer (round (10 * dps / pico_1s)) / 10)
        <> "s" -- e.g. 10.5s

timeAction :: (MonadIO m) => m a -> m (a, DiffTime)
timeAction f = do
  before <- liftIO $ getTime Monotonic
  ret <- f
  after <- liftIO $ getTime Monotonic
  pure
    ( ret,
      picosecondsToDiffTime $
        (pico_1s :: Integer)
          * fromIntegral (sec after - sec before)
          + (pico_1ns :: Integer)
            * fromIntegral (nsec after - nsec before)
    )
