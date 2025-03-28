module Storage.CachedQueries.Merchant.MultiModalBus
  ( BusStopETA (..),
    BusData (..),
    RouteWithBuses (..),
    getRoutesBuses,
    getBusesForRoutes,
    mkRouteKey,
    withCrossAppRedisNew,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Environment
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, id, map)
import Kernel.Prelude hiding (encodeUtf8)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common

-- Type for bus stop ETA information
data BusStopETA = BusStopETA
  { stopId :: Text,
    stopSeq :: Int,
    stopName :: Text,
    arrivalTime :: UTCTime
  }
  deriving (Generic, Show, Eq)

withCrossAppRedisNew ::
  (Hedis.HedisFlow m env, Kernel.Prelude.HasField "ltsHedisEnv" env Hedis.HedisEnv) => m f -> m f
withCrossAppRedisNew f = do
  local (\env -> env{hedisEnv = env.ltsHedisEnv, hedisClusterEnv = env.ltsHedisEnv}) f

instance FromJSON BusStopETA where
  parseJSON = withObject "BusStopETA" $ \v -> do
    stopId <- v .: "stop_id"
    stopSeq <- v .: "stop_seq"
    stopName <- v .: "stop_name"
    arrivalTimestamp <- v .: "arrival_time" :: Parser Integer
    let arrivalTime = posixSecondsToUTCTime $ realToFrac arrivalTimestamp
    return $ BusStopETA {..}

instance ToJSON BusStopETA where
  toJSON BusStopETA {..} =
    object
      [ "stop_id" .= stopId,
        "stop_seq" .= stopSeq,
        "stop_name" .= stopName,
        "arrival_time" .= floor @Double @Integer (realToFrac $ utcTimeToPOSIXSeconds arrivalTime)
      ]

-- Type for bus data including location and ETAs
data BusData = BusData
  { latitude :: Double,
    longitude :: Double,
    timestamp :: Int,
    speed :: Double,
    device_id :: Text,
    etaData :: Maybe [BusStopETA]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data FullBusData = FullBusData
  { vehicleNumber :: Text,
    busData :: BusData
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- Route with multiple buses
data RouteWithBuses = RouteWithBuses
  { routeId :: Text,
    buses :: [FullBusData]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- Create Redis key for a route
mkRouteKey :: Text -> Text
mkRouteKey routeId = "route:" <> routeId

-- Get all buses for a single route
getRoutesBuses :: Text -> Environment.Flow RouteWithBuses
getRoutesBuses routeId = do
  let key = mkRouteKey routeId

  -- Get all bus data from Redis directly

  busDataPairs' :: [(Text, Text)] <- withCrossAppRedisNew $ Hedis.hGetAll key
  logDebug $ "Got bus data for route busDataPairs " <> routeId <> ": " <> show busDataPairs'
  busDataPairs <- withCrossAppRedisNew $ Hedis.hGetAll key
  logDebug $ "Got bus data for route " <> routeId <> ": " <> show busDataPairs

  -- The values are already BusData objects
  let buses = map (\busDataPair -> FullBusData (fst busDataPair) (snd busDataPair)) busDataPairs

  logDebug $ "Parsed bus data for route " <> routeId <> ": " <> show buses

  return $ RouteWithBuses routeId buses

-- Get buses for multiple routes
getBusesForRoutes :: [Text] -> Environment.Flow [RouteWithBuses]
getBusesForRoutes routeIds = mapM getRoutesBuses routeIds
