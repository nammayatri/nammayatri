module Fixtures.Time (defaultTime) where

import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as Time
import qualified Data.Time.Clock as Time
import EulerHS.Prelude

defaultTime :: Time.UTCTime
defaultTime =
  Time.UTCTime
    { utctDay = Time.fromOrdinalDate 2020 120,
      utctDayTime = Time.secondsToDiffTime 40000
    }
