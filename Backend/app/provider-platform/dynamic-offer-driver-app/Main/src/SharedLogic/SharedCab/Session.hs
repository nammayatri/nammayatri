-- | A shared cab's live run, which driver-app owns: the session in Redis, its vehicle_trip row and the LTS attach that puts the cab
-- on its route's `route:{code}` hash. Every write runs under the plate lock; rider-app hears of each change through 'cabUpdate'.
module SharedLogic.SharedCab.Session
  ( Session (..),
    NewSession (..),
    newSession,
    SelectPlan (..),
    planSelect,
    ownedSession,
    returnRouteOf,
    withPlateLock,
    readSession,
    getSession,
    getSessionLocked,
    openTrip,
    switchTrip,
    queueRoute,
    finishTrip,
    pause,
    resume,
    addOfflineBoardings,
    defaultCapacity,
    endedCopy,
    cabUpdate,
  )
where

import Data.List (sortOn)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.SharedCabSession (CabChange, PauseReason (..), SessionStatus (..))
import qualified Domain.Types.SharedCabSession as SCS
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.GtfsDataServer.Domain.Types.RouteStopMapping as RSM
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.IntegratedBPPConfig as QIBC
import qualified Storage.Queries.VehicleTrip as QVT
import Tools.Error

data Session = Session
  { driverId :: Id DP.Person,
    vehicleNumber :: Text,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    integratedBppConfigId :: Id DIBC.IntegratedBPPConfig,
    agencyKey :: Text,
    routeCode :: Text,
    queuedRouteCode :: Maybe Text,
    capacity :: Int,
    status :: SessionStatus,
    pauseReason :: Maybe PauseReason,
    version :: Int,
    startedAt :: UTCTime,
    vehicleTripId :: Id DVT.VehicleTrip,
    offlineBoardings :: Int,
    driverName :: Maybe Text,
    vehicleModel :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | How long a session lives without a write.
sessionTtlSec :: Int
sessionTtlSec = 16 * 60 * 60

sessionKey :: Text -> Text
sessionKey plate = "sharedcab:driver:session:" <> plate

plateLockKey :: Text -> Text
plateLockKey plate = "sharedcab:driver:plateLock:" <> plate

withPlateLock :: (Redis.HedisFlow m r, MonadMask m) => Text -> m a -> m a
withPlateLock plate = Redis.withWaitAndLockRedis (plateLockKey plate) 30 50000

readSession :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
readSession = Redis.safeGet . sessionKey

saveSession :: (Redis.HedisFlow m r, MonadFlow m) => Session -> m Session
saveSession s = s <$ Redis.setExp (sessionKey s.vehicleNumber) s sessionTtlSec

-- | After a Redis flush the live vehicle_trip restores the session: ACTIVE or PAUSED recovers, no live trip means the run ended. The
-- version restarts above any pre-flush counter, so rider-app takes the restored copy.
getSession :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => Text -> m (Maybe Session)
getSession plate =
  readSession plate >>= \case
    Just s -> pure (Just s)
    Nothing ->
      QVT.findActiveByVehicleNumber plate >>= \case
        Nothing -> pure Nothing
        Just _ -> withPlateLock plate (getSessionLocked plate)

-- | 'getSession' for a caller already holding the plate lock (it isn't re-entrant).
getSessionLocked :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m (Maybe Session)
getSessionLocked plate = readSession plate >>= maybe recover (pure . Just)
  where
    recover = QVT.findActiveByVehicleNumber plate >>= traverse rebuild
    rebuild trip = do
      ibc <- QIBC.findById trip.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound
      now <- getCurrentTime
      logInfo $ "sharedCab: rebuilt session for " <> plate <> " from vehicle_trip " <> trip.id.getId
      saveSession (sessionFromTrip ibc.agencyKey trip now)

sessionFromTrip :: Text -> DVT.VehicleTrip -> UTCTime -> Session
sessionFromTrip agencyKey trip now =
  Session
    { driverId = trip.driverId,
      vehicleNumber = trip.vehicleNumber,
      merchantId = trip.merchantId,
      merchantOperatingCityId = trip.merchantOperatingCityId,
      integratedBppConfigId = trip.integratedBppConfigId,
      agencyKey,
      routeCode = trip.routeCode,
      queuedRouteCode = Nothing,
      capacity = fromMaybe defaultCapacity trip.capacity,
      status = if trip.status == DVT.PAUSED then PAUSED else ACTIVE,
      pauseReason = Nothing,
      version = floor (utcTimeToPOSIXSeconds now),
      startedAt = trip.startedAt,
      vehicleTripId = trip.id,
      offlineBoardings = trip.walkupCount,
      driverName = Nothing,
      vehicleModel = Nothing
    }

data NewSession = NewSession
  { driverId :: Id DP.Person,
    vehicleNumber :: Text,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    integratedBPPConfig :: DIBC.IntegratedBPPConfig,
    routeCode :: Text,
    capacity :: Int,
    driverName :: Maybe Text,
    vehicleModel :: Maybe Text
  }

newSession :: NewSession -> Id DVT.VehicleTrip -> UTCTime -> Maybe Session -> Session
newSession req tripId now prior =
  Session
    { driverId = req.driverId,
      vehicleNumber = req.vehicleNumber,
      merchantId = req.merchantId,
      merchantOperatingCityId = req.merchantOperatingCityId,
      integratedBppConfigId = req.integratedBPPConfig.id,
      agencyKey = req.integratedBPPConfig.agencyKey,
      routeCode = req.routeCode,
      queuedRouteCode = Nothing,
      capacity = req.capacity,
      status = ACTIVE,
      pauseReason = Nothing,
      version = maybe 1 ((+ 1) . (.version)) prior,
      startedAt = now,
      vehicleTripId = tripId,
      offlineBoardings = 0,
      driverName = req.driverName,
      vehicleModel = req.vehicleModel
    }

data SelectPlan = OpenSession | ChangeRoute Session | KeepRoute Session
  deriving (Show, Eq)

isLive :: Session -> Bool
isLive s = s.status /= ENDED

planSelect :: Id DP.Person -> Text -> Maybe Session -> Either SharedCabSessionError SelectPlan
planSelect driver route = \case
  Just s
    | not (isLive s) -> Right OpenSession
    | s.driverId /= driver -> Left SessionHeldByAnotherDriver
    | s.routeCode == route -> Right (KeepRoute s)
    | otherwise -> Right (ChangeRoute s)
  Nothing -> Right OpenSession

ownedSession :: Id DP.Person -> Maybe Session -> Either SharedCabSessionError Session
ownedSession driver = \case
  Just s
    | not (isLive s) -> Left SessionNotFound
    | s.driverId /= driver -> Left SessionHeldByAnotherDriver
    | otherwise -> Right s
  Nothing -> Left SessionNotFound

returnRouteOf :: Text -> Either SharedCabSessionError Text
returnRouteOf = maybe (Left NoReturnRoute) Right . SCS.returnRouteCode

bump :: Session -> Session
bump s = s {version = s.version + 1}

closedTripStatus :: DVT.VehicleTripEndReason -> DVT.VehicleTripStatus
closedTripStatus = \case
  DVT.SESSION_TIMEOUT -> DVT.ABANDONED
  DVT.OPS_FORCED -> DVT.ABANDONED
  _ -> DVT.COMPLETED

defaultCapacity :: Int
defaultCapacity = 6

tripFor :: Session -> UTCTime -> DVT.VehicleTrip
tripFor s now =
  DVT.VehicleTrip
    { id = s.vehicleTripId,
      driverId = s.driverId,
      vehicleNumber = s.vehicleNumber,
      capacity = Just s.capacity,
      merchantId = s.merchantId,
      merchantOperatingCityId = s.merchantOperatingCityId,
      integratedBppConfigId = s.integratedBppConfigId,
      routeCode = s.routeCode,
      status = DVT.ACTIVE,
      startedAt = now,
      endedAt = Nothing,
      endReason = Nothing,
      walkupCount = s.offlineBoardings,
      createdAt = now,
      updatedAt = now
    }

type LtsFlow m r c =
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CoreMetrics m,
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    MonadReader r m
  )

busRideInfo :: Session -> [RSM.RouteStopMapping] -> LT.RideInfo
busRideInfo s stops =
  LT.Bus
    LT.BusRideInfo
      { routeCode = s.routeCode,
        busNumber = s.vehicleNumber,
        source = maybe nowhere (.stopPoint) (listToMaybe ordered),
        destination = maybe nowhere (.stopPoint) (listToMaybe (reverse ordered)),
        routeLongName = Nothing,
        driverName = s.driverName,
        groupId = Just s.merchantOperatingCityId.getId
      }
  where
    ordered = sortOn (.sequenceNum) stops
    nowhere = LatLong 0 0

routeStops :: LtsFlow m r c => Session -> m [RSM.RouteStopMapping]
routeStops s = do
  ibc <- QIBC.findById s.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound
  OTPRest.getRouteStopMappingByRouteCode s.routeCode ibc

-- | The LTS attach is what puts the cab's pings on its route's hash, which every rider-app read of cab positions uses.
attach :: LtsFlow m r c => Session -> m ()
attach s = do
  stops <- routeStops s
  let start = maybe (LatLong 0 0) (.stopPoint) (listToMaybe (sortOn (.sequenceNum) stops))
  void $ LF.rideStart (cast s.vehicleTripId) start.lat start.lon s.merchantId s.driverId (Just (busRideInfo s stops))

-- | A failed detach is logged: a lingering `route:{code}` field is harmless, since rider-app lists cabs from its live copies.
detach :: (LtsFlow m r c, MonadCatch m) => Session -> m ()
detach s =
  withTryCatch "sharedCab:ltsDetach" (routeStops s >>= \stops -> LF.rideEnd (cast s.vehicleTripId) 0 0 s.merchantId s.driverId Nothing (Just (busRideInfo s stops)) Nothing)
    >>= either (\err -> logError $ "sharedCab: LTS rideEnd failed for " <> s.vehicleNumber <> " on " <> s.routeCode <> ": " <> show err) (const (pure ()))

-- | A new run: its trip row, then the attach. A failed attach abandons the row, then runs `undo` (rider-app was told of the open first).
openTrip :: (LtsFlow m r c, MonadCatch m) => m () -> Session -> m Session
openTrip undo s = do
  now <- getCurrentTime
  QVT.create (tripFor s now)
  (attach s >> saveSession s) `onException` (QVT.closeTrip DVT.ABANDONED (Just DVT.ROUTE_CHANGED) (Just now) s.vehicleTripId >> undo)

-- | The run as rider-app should see it after an open that failed: ended.
endedCopy :: Session -> Session
endedCopy s = bump s {status = ENDED, pauseReason = Nothing}

-- | Close the run and open one on `newRoute`. The old attach goes first (rideEnd clears LTS's per-driver ride record); if the new
-- one fails, the old run is put back.
switchTrip :: (LtsFlow m r c, MonadCatch m) => DVT.VehicleTripEndReason -> Text -> Session -> m Session
switchTrip reason newRoute s = do
  now <- getCurrentTime
  tripId <- generateGUID
  let s' = bump s {routeCode = newRoute, vehicleTripId = tripId, queuedRouteCode = Nothing, status = ACTIVE, pauseReason = Nothing, startedAt = now, offlineBoardings = 0}
  detach s
  QVT.closeTrip (closedTripStatus reason) (Just reason) (Just now) s.vehicleTripId
  QVT.create (tripFor s' now)
  attach s' `onException` restore now s'
  saveSession s'
  where
    restore now s' = do
      QVT.closeTrip DVT.ABANDONED (Just DVT.ROUTE_CHANGED) (Just now) s'.vehicleTripId
      QVT.closeTrip (if s.status == PAUSED then DVT.PAUSED else DVT.ACTIVE) Nothing Nothing s.vehicleTripId
      withTryCatch "sharedCab:ltsReattach" (attach s) >>= either (\err -> logError $ "sharedCab: reattach failed for " <> s.vehicleNumber <> ": " <> show err) pure

queueRoute :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Session -> m Session
queueRoute route s = saveSession (bump s {queuedRouteCode = Just route})

-- | ENDED first: rider-app claims onto ACTIVE cabs only, so a crash between the steps never leaves a phantom ACTIVE cab.
finishTrip :: (LtsFlow m r c, MonadCatch m) => DVT.VehicleTripEndReason -> Session -> m Session
finishTrip reason s = do
  now <- getCurrentTime
  ended <- saveSession (bump s {status = ENDED, pauseReason = Nothing})
  QVT.closeTrip (closedTripStatus reason) (Just reason) (Just now) s.vehicleTripId
  detach s
  pure ended

-- | Mirrored to the trip row, so a flush restores PAUSED.
pause :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => PauseReason -> Session -> m Session
pause reason s = do
  unless (s.status == ACTIVE) $ throwError SessionNotActive
  QVT.updateStatus DVT.PAUSED s.vehicleTripId
  saveSession (bump s {status = PAUSED, pauseReason = Just reason})

resume :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Session -> m Session
resume s = do
  unless (s.status == PAUSED) $ throwError SessionNotPaused
  QVT.updateStatus DVT.ACTIVE s.vehicleTripId
  saveSession (bump s {status = ACTIVE, pauseReason = Nothing})

addOfflineBoardings :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Int -> Session -> m Session
addOfflineBoardings added s
  | added <= 0 = pure s
  | otherwise = do
    let s' = s {offlineBoardings = s.offlineBoardings + added}
    QVT.updateWalkupCount s'.offlineBoardings s.vehicleTripId
    saveSession s'

cabUpdate :: Maybe CabChange -> Bool -> Session -> SCS.CabUpdate
cabUpdate change notifyOnBoard s =
  SCS.CabUpdate
    { SCS.cab =
        SCS.CabState
          { SCS.agencyId = s.agencyKey,
            SCS.driverId = s.driverId.getId,
            SCS.vehicleNumber = s.vehicleNumber,
            SCS.vehicleTripId = s.vehicleTripId.getId,
            SCS.routeCode = s.routeCode,
            SCS.queuedRouteCode = s.queuedRouteCode,
            SCS.capacity = s.capacity,
            SCS.status = s.status,
            SCS.pauseReason = s.pauseReason,
            SCS.version = s.version,
            SCS.startedAt = s.startedAt,
            SCS.offlineBoardings = s.offlineBoardings,
            SCS.driverName = s.driverName,
            SCS.vehicleModel = s.vehicleModel
          },
      SCS.change = change,
      SCS.notifyOnBoard = notifyOnBoard
    }
