module Mobility.Fixtures.Common where

import Beckn.Types.Time (Seconds)
import Data.Time
import EulerHS.Prelude

timeBetweenLocationUpdates :: Seconds
timeBetweenLocationUpdates = 1

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime
