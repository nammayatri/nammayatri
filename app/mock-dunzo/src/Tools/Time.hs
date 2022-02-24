module Tools.Time where

import Beckn.Prelude
import Data.Aeson.Internal.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX

timeToInt :: UTCTime -> Integer
timeToInt = (`div` 1000000000) . fromPico . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
