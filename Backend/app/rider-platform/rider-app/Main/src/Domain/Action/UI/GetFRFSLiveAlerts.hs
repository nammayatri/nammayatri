module Domain.Action.UI.GetFRFSLiveAlerts where

import Data.List (last)
import qualified Data.Map.Strict as Map
import Data.OpenApi (ToSchema)
import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Clickhouse.FRFSLiveAlerts as CHMetrics

data LiveMetricsResponse = LiveMetricsResponse
  { hourlyBookings :: [HourlyBooking],
    liveStatus :: [ModeStatus],
    downtimeInfo :: [VehicleDowntimeInfo]
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

data HourlyBooking = HourlyBooking
  { time :: UTCTime,
    mode :: Text,
    count :: Int
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

data ModeStatus = ModeStatus
  { mode :: Text,
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

data VehicleDowntimeInfo = VehicleDowntimeInfo
  { vehicleType :: Text,
    lastOutageStartTime :: Maybe UTCTime,
    lastRecoveryTime :: Maybe UTCTime,
    isCurrentlyUp :: Int -- 1 = up, 0 = down
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

getLiveMetrics ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  [Text] ->
  m LiveMetricsResponse
getLiveMetrics merchantOpCityId mbFrom mbTo modes = do
  currTime <- getCurrentTime

  let toTime = fromMaybe currTime mbTo
  let fromTime = fromMaybe (addUTCTime (-86400) toTime) mbFrom
  let effectiveModes = if null modes then ["BUS", "METRO", "SUBWAY"] else modes

  hourlyRaw <- CHMetrics.getHourlyBookingCounts merchantOpCityId effectiveModes fromTime toTime
  let hourlyRes =
        [ HourlyBooking
            { time = t,
              mode = vType,
              count = c
            }
          | (t, vType, c) <- hourlyRaw
        ]

  statusRes <- calculateLiveStatus merchantOpCityId currTime effectiveModes
  downtimeRes <- calculateDowntime merchantOpCityId currTime effectiveModes

  pure $
    LiveMetricsResponse
      { hourlyBookings = hourlyRes,
        liveStatus = statusRes,
        downtimeInfo = downtimeRes
      }

calculateLiveStatus :: (MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Id DMOC.MerchantOperatingCity -> UTCTime -> [Text] -> m [ModeStatus]
calculateLiveStatus merchantOpCityId currTime effectiveModes = do
  let oneHourAgo = addUTCTime (-3600) currTime
  currCounts <- CHMetrics.getCurrentHourCounts merchantOpCityId effectiveModes oneHourAgo
  let currMap = Map.fromList currCounts

  let weeks = [1 .. 4] :: [Int]
  let oneHour = 3600 :: NominalDiffTime
  let oneWeek = 7 * 86400 :: NominalDiffTime
  let ranges =
        [ let end = addUTCTime (fromIntegral (- w) * oneWeek) currTime
           in (addUTCTime (fromIntegral (- w) * oneWeek - oneHour) currTime, end)
          | w <- weeks
        ]
  histCounts <- CHMetrics.getHistoricalCountsForRanges merchantOpCityId ranges
  let histMap = Map.fromList histCounts

  pure $ map (\m -> evaluateStatus m (Map.lookup m currMap) (Map.lookup m histMap)) effectiveModes

evaluateStatus :: Text -> Maybe Int -> Maybe Int -> ModeStatus
evaluateStatus mode currMb histSumMb =
  let curr = fromMaybe 0 currMb
      histSum = fromMaybe 0 histSumMb
      baseline = if histSum > 0 then fromIntegral histSum / (4.0 :: Double) else 0.0
      status =
        if (fromIntegral curr :: Double) < baseline * 0.4
          then "UNDER_ISSUE"
          else "OK"
   in ModeStatus {mode = mode, status = status}

calculateDowntime :: (MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Id DMOC.MerchantOperatingCity -> UTCTime -> [Text] -> m [VehicleDowntimeInfo]
calculateDowntime merchantOpCityId currTime effectiveModes = do
  let twoDaysAgo = addUTCTime (-2 * 86400) currTime
  let analysisWindow = 68 * 86400 :: NominalDiffTime
  let startTime = addUTCTime (- analysisWindow) currTime

  allMinutes <- CHMetrics.getMinuteCounts merchantOpCityId startTime twoDaysAgo

  let currentDayOfWeek = getDayOfWeek currTime
  let baseline = calculateBaselineWithDayOfWeek allMinutes currentDayOfWeek

  let statuses = analyzeOutages allMinutes baseline

  -- Get downtime info for each vehicle type
  pure $ map (getVehicleDowntimeInfo statuses) effectiveModes

-- Get downtime info for a specific vehicle type
getVehicleDowntimeInfo :: [MinuteStatus] -> Text -> VehicleDowntimeInfo
getVehicleDowntimeInfo allStatuses vType =
  let -- Filter statuses for this vehicle type
      vehicleStatuses = filter (\ms -> msVehicleType ms == vType) allStatuses
      sorted = sortBy (comparing msTime) vehicleStatuses

      -- Find last outage
      outages = [ms | ms <- sorted, msIsOutage ms]
      mbLastOutage = if null outages then Nothing else Just (last outages)
   in case mbLastOutage of
        Nothing ->
          -- No outage found
          VehicleDowntimeInfo
            { vehicleType = vType,
              lastOutageStartTime = Nothing,
              lastRecoveryTime = Nothing,
              isCurrentlyUp = 1 -- Up
            }
        Just outageStart ->
          let recoveries = [ms | ms <- sorted, msTime ms > msTime outageStart, msIsResolved ms]
           in case recoveries of
                [] ->
                  -- Outage ongoing (no recovery found)
                  VehicleDowntimeInfo
                    { vehicleType = vType,
                      lastOutageStartTime = Just (msTime outageStart),
                      lastRecoveryTime = Nothing,
                      isCurrentlyUp = 0 -- Down
                    }
                (recovery : _) ->
                  -- Outage recovered
                  VehicleDowntimeInfo
                    { vehicleType = vType,
                      lastOutageStartTime = Just (msTime outageStart),
                      lastRecoveryTime = Just (msTime recovery),
                      isCurrentlyUp = 1 -- Up
                    }

-- Get day of week (0-6, where 0 = Sunday)
getDayOfWeek :: UTCTime -> Int
getDayOfWeek utcTime =
  let day = utctDay utcTime
      (_, _, dow) = toWeekDate day
   in dow `mod` 7

-- Calculate baseline filtered by day of week
calculateBaselineWithDayOfWeek :: [CHMetrics.MinuteCount] -> Int -> Map.Map (Text, Int) Double
calculateBaselineWithDayOfWeek minuteCounts currentDayOfWeek =
  let filtered = filter (\mc -> getDayOfWeek (CHMetrics.mcTime mc) == currentDayOfWeek) minuteCounts

      groupedByDay =
        Map.fromListWith
          (+)
          [ ((CHMetrics.mcVehicleType mc, getMinuteOfDay (CHMetrics.mcTime mc), utctDay (CHMetrics.mcTime mc)), CHMetrics.mcCount mc)
            | mc <- filtered
          ]

      groupedForAvg =
        Map.fromListWith
          (++)
          [ ((vType, minuteOfDay), [count])
            | ((vType, minuteOfDay, _day), count) <- Map.toList groupedByDay
          ]

      averaged = Map.map (\counts -> fromIntegral (sum counts) / fromIntegral (length counts)) groupedForAvg
   in averaged

calculateBaseline :: [CHMetrics.MinuteCount] -> Map.Map (Text, Int) Double
calculateBaseline minuteCounts =
  let grouped =
        Map.fromListWith
          (++)
          [ ((CHMetrics.mcVehicleType mc, getMinuteOfDay (CHMetrics.mcTime mc)), [CHMetrics.mcCount mc])
            | mc <- minuteCounts
          ]
      averaged = Map.map (\counts -> fromIntegral (sum counts) / fromIntegral (length counts)) grouped
   in averaged

getMinuteOfDay :: UTCTime -> Int
getMinuteOfDay utcTime =
  let timeOfDay = utctDayTime utcTime
      seconds = floor timeOfDay :: Int
      hours = seconds `div` 3600
      minutes = (seconds `mod` 3600) `div` 60
   in hours * 60 + minutes

data MinuteStatus = MinuteStatus
  { msVehicleType :: Text,
    msTime :: UTCTime,
    msCount :: Int,
    msBaselineCount :: Double,
    msIsOutage :: Bool,
    msIsResolved :: Bool
  }
  deriving (Show)

analyzeOutages :: [CHMetrics.MinuteCount] -> Map.Map (Text, Int) Double -> [MinuteStatus]
analyzeOutages minuteCounts baseline =
  [ MinuteStatus
      { msVehicleType = vType,
        msTime = time,
        msCount = count,
        msBaselineCount = baselineCount,
        msIsOutage = fromIntegral count < baselineCount * 0.4,
        msIsResolved = fromIntegral count >= baselineCount * 0.8
      }
    | mc <- minuteCounts,
      let vType = CHMetrics.mcVehicleType mc
          time = CHMetrics.mcTime mc
          count = CHMetrics.mcCount mc
          minuteOfDay = getMinuteOfDay time
          baselineCount = Map.findWithDefault 10.0 (vType, minuteOfDay) baseline -- default 10 if no baseline
  ]
