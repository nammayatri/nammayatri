-- | Sunset time from the NOAA solar position approximation. It is accurate to about
-- a minute or two at Indian latitudes.
module Lib.Permit.Picnic.Sunset (sunsetIST) where

import Data.Fixed (mod')
import Data.Time (Day, TimeOfDay (..), toGregorian)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Kernel.Prelude

-- | The approximation can be about a minute late. The SOP requires the group home
-- before sunset, so this much is taken off.
safetyMarginMinutes :: Double
safetyMarginMinutes = 1

-- | A time in Indian Standard Time just before sunset, for a latitude and longitude in
-- degrees: the calculated sunset less the safety margin, rounded down to the minute.
sunsetIST :: Double -> Double -> Day -> TimeOfDay
sunsetIST latitude longitude day = TimeOfDay (minutes `div` 60) (minutes `mod` 60) 0
  where
    (year, _, _) = toGregorian day
    dayOfYear = snd (toOrdinalDate day)
    daysInYear = if year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0) then 366 else 365
    radians degrees = degrees * pi / 180
    g = 2 * pi / daysInYear * fromIntegral (dayOfYear - 1)
    equationOfTime =
      229.18
        * ( 0.000075 + 0.001868 * cos g - 0.032077 * sin g
              - 0.014615 * cos (2 * g)
              - 0.040849 * sin (2 * g)
          )
    declination =
      0.006918 - 0.399912 * cos g + 0.070257 * sin g
        - 0.006758 * cos (2 * g)
        + 0.000907 * sin (2 * g)
        - 0.002697 * cos (3 * g)
        + 0.00148 * sin (3 * g)
    hourAngle =
      acos
        ( cos (radians 90.833) / (cos (radians latitude) * cos declination)
            - tan (radians latitude) * tan declination
        )
        * 180
        / pi
    utcMinutes = 720 - 4 * (longitude - hourAngle) - equationOfTime
    minutes = floor ((utcMinutes + 330 - safetyMarginMinutes) `mod'` 1440) :: Int
