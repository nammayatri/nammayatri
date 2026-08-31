{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Window
  ( PeakWindowKey (..),
    mkPeakWindowKey,
    isJourneyWindowActive,
    isWithinTimeBound,
  )
where

import qualified Data.Text as T
import Data.Time (DiffTime, TimeOfDay (..), timeOfDayToTime, utctDay, utctDayTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Kernel.Prelude
import qualified Kernel.Types.TimeBound as TB

data PeakWindowKey
  = DayWindow
  | TimeBoundWindow Text
  deriving (Eq, Show, Generic)

-- | Same peak-key shape as driver-app IncentiveMetrics.mkIncentiveWindowKey.
-- Unbounded / no active peak -> DayWindow.
mkPeakWindowKey :: UTCTime -> TB.TimeBound -> PeakWindowKey
mkPeakWindowKey _ TB.Unbounded = DayWindow
mkPeakWindowKey localTime tb =
  case findActivePeak tb localTime of
    Just (startTod, endTod) -> TimeBoundWindow (peakWindowSuffix localTime startTod endTod)
    Nothing -> DayWindow

-- | Journey active check is date-range only. Peak windows are enforced per milestone.
-- endDate is exclusive: active while startDate <= localTime < endDate.
isJourneyWindowActive :: UTCTime -> UTCTime -> UTCTime -> Bool
isJourneyWindowActive localTime startDate endDate =
  localTime >= startDate && localTime < endDate

-- | Milestone filter: Nothing / Unbounded always counts; Bounded only inside an active peak.
isWithinTimeBound :: UTCTime -> Maybe TB.TimeBound -> Bool
isWithinTimeBound _ Nothing = True
isWithinTimeBound _ (Just TB.Unbounded) = True
isWithinTimeBound localTime (Just tb) =
  case mkPeakWindowKey localTime tb of
    TimeBoundWindow _ -> True
    DayWindow -> False

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
