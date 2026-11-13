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

import Control.Monad.Extra (firstJustM)
import qualified Data.Aeson as A
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.RouteStopMapping as DRSM
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
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
    offRoute :: Bool,
    -- | gps-processor's answer: the cab is held at the stop at the head of `etas`. Nothing from a gps-processor that doesn't send it.
    onStop :: Maybe Bool
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
    off_route :: Maybe Bool,
    is_on_stop :: Maybe Bool
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
      offRoute = fromMaybe False e.off_route,
      onStop = e.is_on_stop
    }

-- | Read straight from the LTS Redis, the hot path (the tick every few seconds, every rider poll). Fields decode one by one: a bad one is
-- dropped, not the route.
readRoutePositions ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Id DIBC.IntegratedBPPConfig ->
  Text ->
  m [CabTrack]
readRoutePositions integratedBppConfigId routeCode = do
  keys <- routeKeys integratedBppConfigId routeCode
  pairs <- fromMaybe [] <$> firstJustM (\key -> mfilter (not . null) . Just <$> Redis.runInMultiCloudLTSRedisForListFromReplica (Redis.hGetAll @A.Value key)) keys
  fmap catMaybes . forM pairs $ \(plate, raw) -> case A.fromJSON raw of
    A.Success entry -> pure (Just (fromGps plate entry))
    A.Error err -> Nothing <$ logWarning ("shared-cab: dropping gps field " <> plate <> " on route " <> routeCode <> ": " <> T.pack err)

-- | One cab's position; a read failure is Nothing, like a cab with no position.
readRoutePosition ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, CacheFlow m r, EsqDBFlow m r) =>
  Id DIBC.IntegratedBPPConfig ->
  Text ->
  Text ->
  m (Maybe CabTrack)
readRoutePosition integratedBppConfigId routeCode plate = do
  keys <- routeKeys integratedBppConfigId routeCode
  fields <- either (const []) identity <$> withTryCatch "sharedCab:readRoutePosition" (maybeToList <$> firstJustM (\key -> listToMaybe <$> Redis.runInMultiCloudLTSRedisForListFromReplica (maybeToList <$> Redis.hGet @A.Value key plate)) keys)
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

-- | The cab feed's prefixed hash, then the unprefixed one.
-- ponytail: the unprefixed fallback covers gps-processor until its shared-cab feed sets external_key_prefix; drop it after.
routeKeys :: (CacheFlow m r, EsqDBFlow m r) => Id DIBC.IntegratedBPPConfig -> Text -> m [Text]
routeKeys integratedBppConfigId routeCode = do
  prefix <- (>>= redisPrefixOf) <$> CQIBC.findById integratedBppConfigId
  let prefixed = CQMMB.mkRouteKey prefix routeCode
      bare = CQMMB.mkRouteKey Nothing routeCode
  pure $ prefixed : [bare | bare /= prefixed]

redisPrefixOf :: DIBC.IntegratedBPPConfig -> Maybe Text
redisPrefixOf config = case config.providerConfig of
  DIBC.ONDC c -> c.redisPrefix
  DIBC.DIRECT c -> c.redisPrefix
  _ -> Nothing
