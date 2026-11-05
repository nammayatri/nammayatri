{-# LANGUAGE TypeApplications #-}

-- | Reads of gps-processor's route hash (`route:{routeCode}`, one field per plate) in the LTS Redis: the only place that knows its layout.
module SharedLogic.SharedCab.Lts
  ( CabTrack (..),
    RouteRead (..),
    stopPointOf,
    readRoutePositions,
    readRoutePosition,
    etaAt,
    stopEta,
    nextStopCode,
  )
where

import qualified Data.Aeson as A
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Domain.Types.RouteStopMapping as DRSM
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB

-- | One cab as gps-processor reports it.
data CabTrack = CabTrack
  { vehicleNumber :: Text,
    position :: LatLong,
    pingAt :: UTCTime,
    -- | m/s (gps-processor's route-on-ping fleets)
    speed :: Maybe Double,
    -- | the stop the cab is at or heads to; every stop before it is passed
    stopSequence :: Maybe Int,
    tripId :: Maybe Text,
    -- | the stops still ahead, in arrival order
    etas :: [CQMMB.BusStopETA],
    -- | farther from its route than gps-processor's gate (it then lists no stops ahead)
    offRoute :: Bool
  }
  deriving (Show)

-- | A route's cabs and its stops in order: gps-processor reports no stop positions, the route's feed has them.
data RouteRead = RouteRead
  { positions :: [CabTrack],
    stops :: [DRSM.RouteStopMapping]
  }

stopPointOf :: RouteRead -> Text -> Maybe LatLong
stopPointOf route stopCode = (.stopPoint) <$> find ((== stopCode) . (.stopCode)) route.stops

-- | gps-processor's field, as it writes it.
data GpsEntry = GpsEntry
  { latitude :: Double,
    longitude :: Double,
    timestamp :: Int,
    speed :: Maybe Double,
    stop_sequence :: Maybe Int,
    current_trip_id :: Maybe Text,
    eta_data :: Maybe [CQMMB.BusStopETA],
    off_route :: Maybe Bool
  }
  deriving (Generic, FromJSON)

fromGps :: Text -> GpsEntry -> CabTrack
fromGps plate e =
  CabTrack
    { vehicleNumber = plate,
      position = LatLong e.latitude e.longitude,
      pingAt = posixSecondsToUTCTime (fromIntegral e.timestamp),
      speed = e.speed,
      stopSequence = e.stop_sequence,
      tripId = e.current_trip_id,
      etas = sortOn (.arrivalTimeUnix) (fromMaybe [] e.eta_data),
      offRoute = fromMaybe False e.off_route
    }

-- | Read straight from the LTS Redis, the hot path (the tick every few seconds, every rider poll). Fields decode one by one: a bad one is
-- dropped, not the route.
readRoutePositions ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, Log m) =>
  Text ->
  m [CabTrack]
readRoutePositions routeCode = do
  pairs <- Redis.runInMultiCloudLTSRedisForListFromReplica $ Redis.hGetAll @A.Value (routeKey routeCode)
  fmap catMaybes . forM pairs $ \(plate, raw) -> case A.fromJSON raw of
    A.Success entry -> pure (Just (fromGps plate entry))
    A.Error err -> Nothing <$ logWarning ("shared-cab: dropping gps field " <> plate <> " on route " <> routeCode <> ": " <> T.pack err)

-- | One cab's position; a read failure is Nothing, like a cab with no position.
readRoutePosition ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Text ->
  Text ->
  m (Maybe CabTrack)
readRoutePosition routeCode plate = do
  fields <- either (const []) identity <$> withTryCatch "sharedCab:readRoutePosition" (Redis.runInMultiCloudLTSRedisForListFromReplica $ maybeToList <$> Redis.hGet @A.Value (routeKey routeCode) plate)
  pure $ case listToMaybe fields of
    Just raw | A.Success entry <- A.fromJSON raw -> Just (fromGps plate entry)
    _ -> Nothing

-- | When the cab reaches a stop still ahead of it; `arrival_time` is UTC (CQMMB's `arrivalTime` is shifted to IST for display).
etaAt :: Text -> CabTrack -> Maybe UTCTime
etaAt stopCode cab = arrivalUtc <$> find ((== stopCode) . (.stopCode)) cab.etas

-- | A stop's entry with its arrival in UTC and the seconds left from `now`.
stopEta :: UTCTime -> CQMMB.BusStopETA -> CQMMB.BusStopETA
stopEta now e = e {CQMMB.arrivalTime = arrivalUtc e, CQMMB.etaSeconds = Just (max 0 (floor (diffUTCTime (arrivalUtc e) now)))}

arrivalUtc :: CQMMB.BusStopETA -> UTCTime
arrivalUtc e = posixSecondsToUTCTime (fromInteger e.arrivalTimeUnix)

-- | The stop the cab stands at or heads to.
nextStopCode :: CabTrack -> Maybe Text
nextStopCode cab = (.stopCode) <$> listToMaybe cab.etas

-- | gps-processor writes cab positions under the unprefixed key; a bus feed's redisPrefix names a partner feed's own hash.
routeKey :: Text -> Text
routeKey = CQMMB.mkRouteKey Nothing
