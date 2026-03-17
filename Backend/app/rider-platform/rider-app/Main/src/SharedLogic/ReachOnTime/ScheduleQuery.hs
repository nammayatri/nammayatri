module SharedLogic.ReachOnTime.ScheduleQuery where

import qualified Data.Time
import Data.Time (UTCTime, addUTCTime)
import Kernel.Prelude

-- | Time mode for journey planning
data TimeMode = LeaveNow | ArriveBy | DepartAt
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Request to GTFS schedule server for trips between stops
data TripsBetweenRequest = TripsBetweenRequest
  { originStopCode :: Text,
    destinationStopCode :: Text,
    date :: Maybe Text, -- "YYYY-MM-DD"
    departAfter :: Maybe Text, -- "HH:MM:SS" for DepartAt/LeaveNow
    arriveBefore :: Maybe Text, -- "HH:MM:SS" for ArriveBy
    windowMinutes :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Response from GTFS schedule server
data ScheduledTrip = ScheduledTrip
  { tripId :: Text,
    routeId :: Text,
    routeShortName :: Maybe Text,
    originStopCode :: Text,
    originStopName :: Maybe Text,
    destinationStopCode :: Text,
    destinationStopName :: Maybe Text,
    departureTime :: Text,
    arrivalTime :: Text,
    mode :: Maybe Text,
    numStops :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Build a GTFS query request based on time mode
buildScheduleQuery ::
  TimeMode ->
  Text -> -- origin stop code
  Text -> -- destination stop code
  UTCTime -> -- reference time (target or current)
  TripsBetweenRequest
buildScheduleQuery mode originStop destStop refTime =
  let -- Convert to IST for GTFS query
      istOffset = 5 * 3600 + 30 * 60 :: Int
      istTime = addUTCTime (fromIntegral istOffset) refTime
      timeStr = formatTimeHHMMSS istTime
      dateStr = formatDateYMD istTime
   in case mode of
        LeaveNow ->
          TripsBetweenRequest
            { originStopCode = originStop,
              destinationStopCode = destStop,
              date = Just dateStr,
              departAfter = Just timeStr,
              arriveBefore = Nothing,
              windowMinutes = Just 60
            }
        DepartAt ->
          TripsBetweenRequest
            { originStopCode = originStop,
              destinationStopCode = destStop,
              date = Just dateStr,
              departAfter = Just timeStr,
              arriveBefore = Nothing,
              windowMinutes = Just 120
            }
        ArriveBy ->
          TripsBetweenRequest
            { originStopCode = originStop,
              destinationStopCode = destStop,
              date = Just dateStr,
              departAfter = Nothing,
              arriveBefore = Just timeStr,
              windowMinutes = Just 120
            }

-- | Parse time mode from text
parseTimeMode :: Maybe Text -> TimeMode
parseTimeMode Nothing = LeaveNow
parseTimeMode (Just "ArriveBy") = ArriveBy
parseTimeMode (Just "DepartAt") = DepartAt
parseTimeMode _ = LeaveNow

-- | Format time as HH:MM:SS
formatTimeHHMMSS :: UTCTime -> Text
formatTimeHHMMSS utcTime =
  let tod = utctDayTime utcTime
      totalSecs = floor tod :: Int
      h = totalSecs `div` 3600
      m = (totalSecs `mod` 3600) `div` 60
      s = totalSecs `mod` 60
   in padTwo h <> ":" <> padTwo m <> ":" <> padTwo s
  where
    padTwo n = if n < 10 then "0" <> show n else show n
    utctDayTime = Data.Time.utctDayTime

-- | Format date as YYYY-MM-DD
formatDateYMD :: UTCTime -> Text
formatDateYMD utcTime =
  let day = Data.Time.utctDay utcTime
   in show day

-- | Determine the reference time for schedule queries based on time mode
getScheduleReferenceTime :: TimeMode -> Maybe UTCTime -> Maybe UTCTime -> UTCTime -> UTCTime
getScheduleReferenceTime mode mbTargetArrival mbTargetDeparture now =
  case mode of
    LeaveNow -> now
    ArriveBy -> fromMaybe now mbTargetArrival
    DepartAt -> fromMaybe now mbTargetDeparture
