module Storage.CachedQueries.Merchant.MultiModalSuburban where

import Data.Aeson
import qualified Data.Text as T
import Data.Time
import Kernel.Prelude hiding (encodeUtf8, sequence)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB

-- Data type to represent train information from Redis
data TrainInfo = TrainInfo
  { trainNo :: Text,
    trainName :: Text,
    trainStartDate :: UTCTime,
    hasArrived :: Int,
    hasDeparted :: Int,
    sequence :: Int,
    stationCode :: Text,
    platformNo :: Text,
    delayArrival :: Int,
    delayDeparture :: Int,
    schedArrivalTime :: TimeOfDay,
    schedDepartureTime :: TimeOfDay
  }
  deriving (Generic, Show)

instance FromJSON TrainInfo where
  parseJSON = withObject "TrainInfo" $ \v -> do
    trainNo <- v .: "trainNo"
    trainName <- v .: "trainName"
    trainStartDateStr <- v .: "trainStartDate"
    hasArrived <- v .: "hasArrived"
    hasDeparted <- v .: "hasDeparted"
    sequence <- v .: "sequence"
    stationCode <- v .: "stationCode"
    platformNo <- v .: "platformNo"
    delayArrival <- v .: "delayArrival"
    delayDeparture <- v .: "delayDeparture"
    schedArrivalTimeStr <- v .: "schedArrivalTime"
    schedDepartureTimeStr <- v .: "schedDepartureTime"

    -- Parse time strings to TimeOfDay
    let schedArrivalTime = parseTimeOfDay schedArrivalTimeStr
    let schedDepartureTime = parseTimeOfDay schedDepartureTimeStr
    let trainStartDate = parseUTCTime trainStartDateStr

    return $
      TrainInfo
        { trainNo = trainNo,
          trainName = trainName,
          trainStartDate = trainStartDate,
          hasArrived = hasArrived,
          hasDeparted = hasDeparted,
          sequence = sequence,
          stationCode = stationCode,
          platformNo = platformNo,
          delayArrival = delayArrival,
          delayDeparture = delayDeparture,
          schedArrivalTime = schedArrivalTime,
          schedDepartureTime = schedDepartureTime
        }

instance ToJSON TrainInfo

-- Data type to represent a route with its trains
data RouteWithTrains = RouteWithTrains
  { routeCode :: Text,
    trains :: [TrainInfo]
  }
  deriving (Generic, Show)

instance FromJSON RouteWithTrains

instance ToJSON RouteWithTrains

mkRouteKey :: Text -> Text
mkRouteKey routeId = "suburban:" <> routeId

-- Helper function to parse time strings like "23:35:00" to TimeOfDay
parseTimeOfDay :: Text -> TimeOfDay
parseTimeOfDay timeStr =
  case T.splitOn ":" timeStr of
    [hourStr, minuteStr, _] ->
      let hour = fromMaybe 0 $ readMaybe $ T.unpack hourStr
          minute = fromMaybe 0 $ readMaybe $ T.unpack minuteStr
       in TimeOfDay hour minute 0
    _ -> TimeOfDay 0 0 0 -- Default to midnight if parsing fails

-- Helper function to parse date strings like "2025/04/29 20:00:00" to UTCTime
parseUTCTime :: Text -> UTCTime
parseUTCTime dateStr =
  fromMaybe epoch $ parseTimeM False defaultTimeLocale "%Y/%m/%d %H:%M:%S" (T.unpack dateStr)
  where
    epoch = UTCTime (fromGregorian 1970 1 1) 0

-- Get all trains for a single route
getRoutesTrains :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => Text -> m RouteWithTrains
getRoutesTrains routeId = do
  let key = mkRouteKey routeId

  -- Get train data from Redis as Text
  trainDataList :: Maybe [TrainInfo] <- CQMMB.withCrossAppRedisNew $ Hedis.safeGet key
  logDebug $ "Got train data for route " <> routeId <> ": " <> show trainDataList
  case trainDataList of
    Just trainDataList' -> return $ RouteWithTrains routeId trainDataList'
    Nothing -> do
      logDebug $ "No train data found for route " <> routeId
      return $ RouteWithTrains routeId []

-- Get trains for multiple routes
getTrainsForRoutes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => [Text] -> m [RouteWithTrains]
getTrainsForRoutes = mapM getRoutesTrains
