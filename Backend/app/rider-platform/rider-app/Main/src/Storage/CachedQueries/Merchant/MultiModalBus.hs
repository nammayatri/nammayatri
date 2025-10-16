module Storage.CachedQueries.Merchant.MultiModalBus
  ( BusStopETA (..),
    BusData (..),
    RouteWithBuses (..),
    FullBusData (..),
    RouteState (..),
    BusRouteInfo (..),
    BusRouteId,
    BusDataWithRoutesInfo (..),
    getRoutesBuses,
    getBusesForRoutes,
    mkRouteKey,
    withCrossAppRedisNew,
    utcToIST,
    incrementBusRouteMismatchCounter,
    getBusRouteMismatchCount,
  )
where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Domain.Utils (mapConcurrently)
import EulerHS.Prelude hiding (encodeUtf8, fromStrict, id, map)
import Kernel.Prelude hiding (encodeUtf8)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common

-- Route state enum for bus routes
data RouteState = ConfirmedHigh | Alternate | All | Schedule | ConfirmedMed
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Hashable RouteState

utcToIST :: UTCTime -> UTCTime
utcToIST = addUTCTime 19800

-- Type for bus stop ETA information
data BusStopETA = BusStopETA
  { stopCode :: Text,
    arrivalTime :: UTCTime
  }
  deriving (Generic, Show, Eq)

withCrossAppRedisNew ::
  (Hedis.HedisFlow m env, Kernel.Prelude.HasField "ltsHedisEnv" env Hedis.HedisEnv) => m f -> m f
withCrossAppRedisNew f = do
  local (\env -> env{hedisEnv = env.ltsHedisEnv, hedisClusterEnv = env.ltsHedisEnv}) f

instance FromJSON BusStopETA where
  parseJSON = withObject "BusStopETA" $ \v -> do
    stopCode <- v .: "stop_id"
    arrivalTimestamp <- v .: "arrival_time" :: Parser Integer
    let arrivalTime = utcToIST $ posixSecondsToUTCTime $ realToFrac arrivalTimestamp
    return $ BusStopETA {..}

instance ToJSON BusStopETA where
  toJSON BusStopETA {..} =
    object
      [ "stop_id" .= stopCode,
        "arrival_time" .= floor @Double @Integer (realToFrac $ utcTimeToPOSIXSeconds arrivalTime)
      ]

-- Type for bus data including location and ETAs
data BusData = BusData
  { latitude :: Double,
    longitude :: Double,
    timestamp :: Int,
    eta_data :: Maybe [BusStopETA],
    route_id :: Text,
    route_state :: Maybe RouteState,
    route_number :: Maybe Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data BusRouteInfo = BusRouteInfo
  { route_number :: Maybe Text,
    route_state :: RouteState
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

type BusRouteId = Text

data BusDataWithRoutesInfo = BusDataWithRoutesInfo
  { latitude :: Double,
    longitude :: Double,
    timestamp :: Int,
    vehicle_number :: Maybe Text,
    routes_info :: Maybe (M.Map BusRouteId BusRouteInfo)
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
getRoutesBuses :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => Text -> m RouteWithBuses
getRoutesBuses routeId = do
  let key = mkRouteKey routeId
  busDataPairs <- withCrossAppRedisNew $ Hedis.hGetAll key
  let buses = map (uncurry FullBusData) busDataPairs
  return $ RouteWithBuses routeId buses

getBusesForRoutes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => [Text] -> m [RouteWithBuses]
getBusesForRoutes = mapConcurrently getRoutesBuses

incrementBusRouteMismatchCounter :: (MonadFlow m, CacheFlow m r, HasField "ltsHedisEnv" r Hedis.HedisEnv) => Text -> Text -> m ()
incrementBusRouteMismatchCounter busNumber routeNumber = do
  let key = busRouteMismatchKey busNumber routeNumber
  withCrossAppRedisNew $ do
    _ <- Hedis.incr key
    Hedis.expire key 3600
    return ()

busRouteMismatchKey :: Text -> Text -> Text
busRouteMismatchKey busNumber routeNumber = "BusTracking:MismatchedRouteNumber-" <> busNumber <> "-" <> routeNumber

getBusRouteMismatchCount :: (MonadFlow m, Hedis.HedisFlow m env, HasField "ltsHedisEnv" env Hedis.HedisEnv) => Text -> Text -> m (Maybe Integer)
getBusRouteMismatchCount busNumber routeNumber = withCrossAppRedisNew $ Hedis.get (busRouteMismatchKey busNumber routeNumber)
