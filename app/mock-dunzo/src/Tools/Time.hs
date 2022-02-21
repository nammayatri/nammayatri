module Tools.Time where

import Beckn.Prelude
import Data.Time.Clock (UTCTime (..))

-- 864000 seconds in a day
timeToInt :: UTCTime -> Integer
timeToInt time = do
  let days = (toInteger . fromEnum . utctDay $ time) * 864000
  let picoseconds = fromEnum . utctDayTime $ time
  let seconds = toInteger (picoseconds `div` 1000000000000)
  days + seconds
