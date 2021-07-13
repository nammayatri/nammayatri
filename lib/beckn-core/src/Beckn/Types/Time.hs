module Beckn.Types.Time where

import Data.Time (UTCTime)
import qualified Data.Time as Time
import EulerHS.Prelude
import qualified System.Clock as Clock

type Ms = Int

type MeasuringDuration m a = MonadClock m => m a -> m a

class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

class Monad m => MonadClock m where
  getClockTime :: m Clock.TimeSpec

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime

instance MonadClock IO where
  getClockTime = Clock.getTime Clock.Monotonic
