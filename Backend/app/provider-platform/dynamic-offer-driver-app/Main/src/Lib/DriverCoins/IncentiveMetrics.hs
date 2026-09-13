{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverCoins.IncentiveMetrics
  ( IncentiveWindowKey (..),
    RideIncentiveDeltas (..),
    mkIncentiveWindowKey,
    unBoundedWindowKey,
    windowSuffix,
    matchingTimeBoundWindows,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import Data.Time (DiffTime, TimeOfDay (..), timeOfDayToTime, utctDay, utctDayTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Kernel.Prelude
import qualified Kernel.Types.TimeBound as TB

data IncentiveWindowKey
  = DayWindow
  | TimeBoundWindow Text
  deriving stock (Eq, Show, Generic)

data RideIncentiveDeltas = RideIncentiveDeltas
  { ridesDelta :: Int,
    earningsDelta :: Int,
    distanceMetersDelta :: Int,
    rideTimeSecondsDelta :: Int
  }
  deriving stock (Eq, Show, Generic)

unBoundedWindowKey :: IncentiveWindowKey
unBoundedWindowKey = DayWindow

-- | Window key for cohort ride-count Redis. Unbounded -> Day. TimeBound -> weekday + active
-- peak (e.g. "Monday:17:00:00-20:00:00").
mkIncentiveWindowKey :: UTCTime -> TB.TimeBound -> IncentiveWindowKey
mkIncentiveWindowKey _ TB.Unbounded = DayWindow
mkIncentiveWindowKey localTime tb =
  case findActivePeak tb localTime of
    Just (startTod, endTod) -> TimeBoundWindow (peakWindowSuffix localTime startTod endTod)
    Nothing -> DayWindow

peakWindowSuffix :: UTCTime -> TimeOfDay -> TimeOfDay -> Text
peakWindowSuffix localTime startTod endTod =
  localDayName localTime <> ":" <> T.pack (show startTod) <> "-" <> T.pack (show endTod)

localDayName :: UTCTime -> Text
localDayName localTime =
  let (_, _, dow) = toWeekDate (utctDay localTime)
   in case dow of
        1 -> "Monday"
        2 -> "Tuesday"
        3 -> "Wednesday"
        4 -> "Thursday"
        5 -> "Friday"
        6 -> "Saturday"
        7 -> "Sunday"
        _ -> "Monday"

findActivePeak :: TB.TimeBound -> UTCTime -> Maybe (TimeOfDay, TimeOfDay)
findActivePeak TB.Unbounded _ = Nothing
findActivePeak (TB.BoundedByWeekday peaks) localTime =
  let (_, _, dow) = toWeekDate (utctDay localTime)
      dayPeaks = getPeaksForCurrentDay dow peaks
   in findPeakContaining (utctDayTime localTime) (handleTwentyFourHourClockCycle dayPeaks)
findActivePeak (TB.BoundedByDay days) localTime =
  case lookup (utctDay localTime) days of
    Nothing -> Nothing
    Just dayPeaks -> findPeakContaining (utctDayTime localTime) (handleTwentyFourHourClockCycle dayPeaks)

findPeakContaining :: DiffTime -> [(TimeOfDay, TimeOfDay)] -> Maybe (TimeOfDay, TimeOfDay)
findPeakContaining currTime = find (\(startTod, endTod) -> currTime > timeOfDayToTime startTod && currTime < timeOfDayToTime endTod)

handleTwentyFourHourClockCycle :: [(TimeOfDay, TimeOfDay)] -> [(TimeOfDay, TimeOfDay)]
handleTwentyFourHourClockCycle =
  foldl'
    ( \timeBounds (startTime, endTime) ->
        if endTime < startTime
          then timeBounds <> [(startTime, TimeOfDay 23 59 59), (TimeOfDay 00 00 00, endTime)]
          else timeBounds <> [(startTime, endTime)]
    )
    []

getPeaksForCurrentDay :: Int -> TB.BoundedPeaks -> [(TimeOfDay, TimeOfDay)]
getPeaksForCurrentDay currentDayOfWeek peaks =
  case currentDayOfWeek of
    1 -> peaks.monday
    2 -> peaks.tuesday
    3 -> peaks.wednesday
    4 -> peaks.thursday
    5 -> peaks.friday
    6 -> peaks.saturday
    7 -> peaks.sunday
    _ -> peaks.monday

windowSuffix :: IncentiveWindowKey -> Text
windowSuffix DayWindow = "Day"
windowSuffix (TimeBoundWindow key) = "TimeBound:" <> key

-- | TimeBound windows (no Day) whose peaks contain localTime.
matchingTimeBoundWindows :: UTCTime -> [TB.TimeBound] -> [IncentiveWindowKey]
matchingTimeBoundWindows localTime timeBounds =
  nub $
    filter (/= DayWindow) $
      [ mkIncentiveWindowKey localTime tb
        | tb <- timeBoundsMatchingNow
      ]
  where
    timeBoundsMatchingNow =
      let wrapped = TimeBoundHolder <$> filter (/= TB.Unbounded) timeBounds
          matched = TB.findBoundedDomain wrapped localTime
       in map (.timeBounds) matched

data TimeBoundHolder = TimeBoundHolder {timeBounds :: TB.TimeBound}
  deriving stock (Generic)
