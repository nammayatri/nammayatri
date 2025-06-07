module Storage.CachedQueries.Merchant.MultiModalBus
  ( BusStopETA (..),
    BusData (..),
    BusDataWithoutETA (..),
    RouteWithBuses (..),
    FullBusData (..),
    getRoutesBuses,
    getBusesForRoutes,
    mkRouteKey,
    withCrossAppRedisNew,
    utcToIST,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, id, map)
import Kernel.Prelude hiding (encodeUtf8)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common

utcToIST :: UTCTime -> UTCTime
utcToIST = addUTCTime 19800

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
    let arrivalTime = utcToIST $ posixSecondsToUTCTime $ realToFrac arrivalTimestamp
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
    eta_data :: Maybe [BusStopETA],
    route_id :: Text,
    vehicle_number :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- Type for bus data without ETA
data BusDataWithoutETA = BusDataWithoutETA
  { latitude :: Double,
    longitude :: Double,
    timestamp :: Int,
    speed :: Double,
    device_id :: Text,
    route_id :: Text,
    vehicle_number :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

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
getRoutesBuses :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => Text -> m RouteWithBuses
getRoutesBuses routeId = do
  let key = mkRouteKey routeId

  busDataPairs <- withCrossAppRedisNew $ Hedis.hGetAll key
  logDebug $ "Got bus data for route " <> routeId <> ": " <> show busDataPairs

  let buses = map (uncurry FullBusData) busDataPairs

  logDebug $ "Parsed bus data for route " <> routeId <> ": " <> show buses

  return $ RouteWithBuses routeId buses

getBusesForRoutes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => [Text] -> m [RouteWithBuses]
getBusesForRoutes = mapM getRoutesBuses
