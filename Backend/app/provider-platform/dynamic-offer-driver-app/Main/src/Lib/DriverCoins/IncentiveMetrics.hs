{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverCoins.IncentiveMetrics
  ( DriverIncentiveMetricsData (..),
    IncentiveWindowKey (..),
    RideIncentiveDeltas (..),
    mkIncentiveWindowKey,
    unBoundedWindowKey,
    windowSuffix,
    matchingTimeBoundWindows,
    incrementIncentiveMetrics,
    getIncentiveMetricsData,
    isMetricThresholdMet,
    areAllConfiguredMetricsMet,
  )
where

import Data.List (nub)
import qualified Data.Text as T
import Data.Time (DiffTime, TimeOfDay (..), timeOfDayToTime, utctDay, utctDayTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.Common

data IncentiveWindowKey
  = DayWindow
  | TimeBoundWindow Text
  deriving stock (Eq, Show, Generic)

-- | All DriverIncentiveCohortMetrics counters for one driver in one window
-- (day or time-bound), stored as a single Redis JSON value.
data DriverIncentiveMetricsData = DriverIncentiveMetricsData
  { ridesCompleted :: Int,
    totalEarnings :: Int,
    totalTripDistanceMeters :: Int,
    totalRideTimeSeconds :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RideIncentiveDeltas = RideIncentiveDeltas
  { ridesDelta :: Int,
    earningsDelta :: Int,
    distanceMetersDelta :: Int,
    rideTimeSecondsDelta :: Int
  }
  deriving stock (Eq, Show, Generic)

unBoundedWindowKey :: IncentiveWindowKey
unBoundedWindowKey = DayWindow

-- | Window key for metrics Redis. Unbounded -> Day. TimeBound -> weekday + active
-- peak (e.g. "Monday:17:00:00-20:00:00"); keys expire at local midnight.
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

-- | Active peak interval at localTime (mirrors Kernel.Types.TimeBound.findBoundedDomain).
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

defaultIncentiveMetricsData :: DriverIncentiveMetricsData
defaultIncentiveMetricsData =
  DriverIncentiveMetricsData
    { ridesCompleted = 0,
      totalEarnings = 0,
      totalTripDistanceMeters = 0,
      totalRideTimeSeconds = 0
    }

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
      let wrapped = (\tb -> TimeBoundHolder tb) <$> filter (/= TB.Unbounded) timeBounds
          matched = TB.findBoundedDomain wrapped localTime
       in map (.timeBounds) matched

mkIncentiveMetricsWindowKey :: Id DP.Person -> IncentiveWindowKey -> Text
mkIncentiveMetricsWindowKey driverId windowKey =
  "DriverIncentiveMetrics:DriverId:"
    <> driverId.getId
    <> ":Window:"
    <> windowSuffix windowKey

applyDeltas :: RideIncentiveDeltas -> DriverIncentiveMetricsData -> DriverIncentiveMetricsData
applyDeltas deltas metrics =
  metrics
    { ridesCompleted = metrics.ridesCompleted + deltas.ridesDelta,
      totalEarnings = metrics.totalEarnings + deltas.earningsDelta,
      totalTripDistanceMeters = metrics.totalTripDistanceMeters + deltas.distanceMetersDelta,
      totalRideTimeSeconds = metrics.totalRideTimeSeconds + deltas.rideTimeSecondsDelta
    }

getIncentiveMetricsData :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> IncentiveWindowKey -> m DriverIncentiveMetricsData
getIncentiveMetricsData driverId windowKey = do
  let key = mkIncentiveMetricsWindowKey driverId windowKey
  mbMetrics <-
    Hedis.runInMasterCloudRedisCellWithCrossAppRedis
      (Hedis.get key)
  let metrics = fromMaybe defaultIncentiveMetricsData mbMetrics
  logDebug $
    "IncentiveMetrics Redis read - driverId: "
      <> driverId.getId
      <> ", key: "
      <> key
      <> ", window: "
      <> show windowKey
      <> ", found: "
      <> show (isJust mbMetrics)
      <> ", metrics: "
      <> show metrics
  pure metrics

setIncentiveMetricsWindowWithExpiry ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  IncentiveWindowKey ->
  DriverIncentiveMetricsData ->
  Int ->
  m ()
setIncentiveMetricsWindowWithExpiry driverId windowKey metrics expirationPeriod = do
  let key = mkIncentiveMetricsWindowKey driverId windowKey
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.setExp key metrics expirationPeriod

incrementIncentiveMetricsWindow ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  IncentiveWindowKey ->
  RideIncentiveDeltas ->
  Int ->
  m ()
incrementIncentiveMetricsWindow driverId windowKey deltas expirationPeriod = do
  let key = mkIncentiveMetricsWindowKey driverId windowKey
  mbExisting <- Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.get key
  let previous = fromMaybe defaultIncentiveMetricsData mbExisting
      updated = applyDeltas deltas previous
  logDebug $
    "IncentiveMetrics Redis write - driverId: "
      <> driverId.getId
      <> ", key: "
      <> key
      <> ", window: "
      <> show windowKey
      <> ", deltas: "
      <> show deltas
      <> ", previous: "
      <> show previous
      <> ", updated: "
      <> show updated
      <> ", ttlSeconds: "
      <> show expirationPeriod
  setIncentiveMetricsWindowWithExpiry driverId windowKey updated expirationPeriod

-- | Redis counters for DriverIncentiveCohortMetrics only (rides / earnings /
-- distance / ride-time). One JSON key per driver per window. Other coin events
-- use their existing keys (e.g. DriverValidRideCount).
incrementIncentiveMetrics ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  RideIncentiveDeltas ->
  Int ->
  [TB.TimeBound] ->
  UTCTime ->
  m ()
incrementIncentiveMetrics driverId deltas expirationPeriod timeBounds localTime = do
  let matchedWindows = matchingTimeBoundWindows localTime timeBounds
      windows = DayWindow : matchedWindows
  logDebug $
    "IncentiveMetrics increment - driverId: "
      <> driverId.getId
      <> ", localTime: "
      <> show localTime
      <> ", configuredTimeBounds: "
      <> show timeBounds
      <> ", matchedTimeBoundWindows: "
      <> show matchedWindows
      <> ", windowsToUpdate: "
      <> show windows
      <> ", deltas: "
      <> show deltas
  forM_ windows $ \windowKey ->
    incrementIncentiveMetricsWindow driverId windowKey deltas expirationPeriod

data TimeBoundHolder = TimeBoundHolder {timeBounds :: TB.TimeBound}
  deriving stock (Generic)

isMetricThresholdMet :: Maybe Int -> Int -> Bool
isMetricThresholdMet Nothing _ = False
isMetricThresholdMet (Just threshold) actual = actual >= threshold

areAllConfiguredMetricsMet ::
  Maybe Int ->
  Int ->
  Maybe Int ->
  Int ->
  Maybe Int ->
  Int ->
  Maybe Int ->
  Int ->
  Bool
areAllConfiguredMetricsMet ridesCompletedThreshold rides totalEarningsThreshold earnings totalTripDistanceMetersThreshold distance totalRideTimeSecondsThreshold rideTime =
  let checks =
        [ (ridesCompletedThreshold, rides),
          (totalEarningsThreshold, earnings),
          (totalTripDistanceMetersThreshold, distance),
          (totalRideTimeSecondsThreshold, rideTime)
        ]
      configured = [(threshold, actual) | (Just threshold, actual) <- checks]
   in not (null configured) && all (\(threshold, actual) -> actual >= threshold) configured
