module Domain.Types.TimeRange where

import Data.Time.LocalTime (LocalTime, addLocalTime)
import Kernel.Prelude

data TimeRange = TimeRange
  { start :: LocalTime,
    end :: LocalTime
  }

fromTimeAndDuration :: LocalTime -> NominalDiffTime -> TimeRange
fromTimeAndDuration start duration =
  let end = addLocalTime duration start
   in TimeRange {..}
