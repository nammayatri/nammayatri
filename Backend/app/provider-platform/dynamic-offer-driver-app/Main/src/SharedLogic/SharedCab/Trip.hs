-- | A shared cab's runs (vehicle_trip). rider-app's session is the truth: after each proxied call that answers with it, the plate's row
-- is brought in line, under the plate's lock, so a plate has at most one ACTIVE trip.
module SharedLogic.SharedCab.Trip
  ( withTripLock,
    RiderRun (..),
    riderRun,
    TripAction (..),
    tripAction,
    syncTrip,
    closeTrip,
  )
where

import Data.Maybe (listToMaybe)
import Data.Time (utctDay)
import qualified Domain.Types.DailyStats as DDS
import Domain.Types.IntegratedBPPConfig (IntegratedBPPConfig)
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.Person as SP
import Domain.Types.SharedCabSession (SessionStatus (ENDED), SharedCabSession)
import qualified Domain.Types.Vehicle as DVehicle
import Domain.Types.VehicleTrip
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.External.LocationTrackingService.Types (BusRideInfo (..), LocationTrackingeServiceConfig, RideInfo (Bus))
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DailyStats as SQDS
import qualified Storage.Queries.VehicleTrip as QVT

-- | A run is also the driver's LTS ride: LTS forwards the cab's pings with the ride's id as their trip and its route and plate, so
-- the phone sets none of them, and refuses them without one. A driver changes route only through a new run (and ride).
type TripFlow m r c =
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, CoreMetrics m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c, HasRequestId r)

-- | Held across the rider-app call, whose client gives up after 5 s.
withTripLock :: (Redis.HedisFlow m r, MonadMask m) => Text -> m a -> m a
withTripLock plate = Redis.withWaitAndLockRedis ("sharedcab:trip:lock:" <> plate) 10 100000

-- | The run rider-app's live session is on, with its route's first and last stop.
data RiderRun = RiderRun
  { tripId :: Text,
    routeCode :: Text,
    walkupCount :: Int,
    earnings :: Maybe HighPrecMoney,
    fromStop :: Maybe Text,
    toStop :: Maybe Text,
    routeEnds :: Maybe (LatLong, LatLong)
  }
  deriving (Show, Eq)

riderRun :: SharedCabSession -> Maybe RiderRun
riderRun s
  | s.status == ENDED = Nothing
  | otherwise =
    Just
      RiderRun
        { tripId = s.tripId,
          routeCode = s.route.code,
          walkupCount = s.walkupCount,
          earnings = s.tripEarnings,
          fromStop = (.name) <$> listToMaybe s.stops,
          toStop = (.name) <$> listToMaybe (reverse s.stops),
          routeEnds = (,) <$> (point <$> listToMaybe s.stops) <*> (point <$> listToMaybe (reverse s.stops))
        }
  where
    point stop = LatLong stop.lat stop.lon

data TripAction
  = TripUnchanged
  | SetProgress (Id VehicleTrip) Int HighPrecMoney
  | CloseTrip (Id VehicleTrip) VehicleTripEndReason
  | OpenTrip (Maybe (Id VehicleTrip, VehicleTripEndReason)) RiderRun
  deriving (Show, Eq)

-- | `endReason` closes the ACTIVE row when rider-app has no live session; a row for another run is closed as a route change of the same
-- driver, or as abandoned by another.
tripAction :: Id SP.Person -> VehicleTripEndReason -> Maybe VehicleTrip -> Maybe RiderRun -> TripAction
tripAction driverId endReason active = \case
  Just run -> case active of
    Just t
      | t.id.getId == run.tripId ->
        let stored = fromMaybe 0 t.earnings
            earnings = fromMaybe stored run.earnings
         in if t.walkupCount == run.walkupCount && stored == earnings then TripUnchanged else SetProgress t.id run.walkupCount earnings
    _ -> OpenTrip ((\t -> (t.id, if t.driverId == driverId then ROUTE_CHANGED else SESSION_TIMEOUT)) <$> active) run
  Nothing -> maybe TripUnchanged (\t -> CloseTrip t.id endReason) active

syncTrip ::
  TripFlow m r c =>
  Id SP.Person ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  IntegratedBPPConfig ->
  DVehicle.Vehicle ->
  VehicleTripEndReason ->
  Maybe HighPrecMoney ->
  Maybe RiderRun ->
  m ()
syncTrip driverId merchantId merchantOperatingCityId integratedBppConfig vehicle endReason endedEarnings mbRun = do
  active <- QVT.findActiveByVehicleNumber vehicle.registrationNo
  case tripAction driverId endReason active mbRun of
    TripUnchanged -> pure ()
    SetProgress tripId count earnings -> QVT.updateProgress count (Just earnings) tripId
    -- the row CloseTrip and OpenTrip name is the active one
    CloseTrip _ reason -> whenJust active (closeTrip reason endedEarnings)
    OpenTrip mbClose run -> do
      whenJust ((,) <$> mbClose <*> active) $ \((_, reason), t) -> closeTrip reason endedEarnings t
      -- before the row: a failed start leaves no row, so the next sync opens the run again and retries it
      (source, destination) <- fromMaybeM (InternalError $ "shared-cab route " <> run.routeCode <> " has no stops") run.routeEnds
      let ride = BusRideInfo {routeCode = run.routeCode, busNumber = vehicle.registrationNo, source, destination, routeLongName = Nothing, driverName = Nothing, groupId = Nothing}
      void $ LF.rideStart (Id run.tripId) 0 0 merchantId driverId (Just (Bus ride))
      now <- getCurrentTime
      QVT.create
        VehicleTrip
          { id = Id run.tripId,
            driverId,
            vehicleNumber = vehicle.registrationNo,
            capacity = vehicle.capacity,
            merchantId,
            merchantOperatingCityId,
            integratedBppConfigId = integratedBppConfig.id,
            routeCode = run.routeCode,
            status = ACTIVE,
            startedAt = now,
            endedAt = Nothing,
            endReason = Nothing,
            walkupCount = run.walkupCount,
            earnings = run.earnings,
            fromStop = run.fromStop,
            toStop = run.toStop,
            createdAt = now,
            updatedAt = now
          }

closeTrip :: TripFlow m r c => VehicleTripEndReason -> Maybe HighPrecMoney -> VehicleTrip -> m ()
closeTrip reason finalEarnings trip = do
  now <- getCurrentTime
  whenJust finalEarnings $ \earnings -> QVT.updateProgress trip.walkupCount (Just earnings) trip.id
  QVT.closeTrip (if reason == SESSION_TIMEOUT then ABANDONED else COMPLETED) (Just reason) (Just now) trip.id
  withTryCatch "sharedCab:dailyStats" (addToDailyStats trip (fromMaybe 0 (finalEarnings <|> trip.earnings)) now)
    >>= either (\e -> logError $ "shared-cab daily stats failed for trip " <> trip.id.getId <> ": " <> show e) (const $ pure ())
  -- best effort: a ride left behind only expires
  withTryCatch "sharedCab:ltsRideEnd" (LF.rideEnd (cast trip.id) 0 0 trip.merchantId trip.driverId Nothing Nothing Nothing)
    >>= either (\e -> logError $ "shared-cab LTS rideEnd failed for trip " <> trip.id.getId <> ": " <> show e) (const $ pure ())

addToDailyStats :: TripFlow m r c => VehicleTrip -> HighPrecMoney -> UTCTime -> m ()
addToDailyStats trip earnings now = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = trip.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound trip.merchantOperatingCityId.getId)
  let day = utctDay (addUTCTime (fromIntegral transporterConfig.timeDiffFromUtc) trip.startedAt)
      rideTime :: Seconds = round (diffUTCTime now trip.startedAt)
  SQDS.findByDriverIdAndDate trip.driverId day >>= \case
    Just stats -> SQDS.updateSharedCabStatsByDriverId (stats.sharedCabTrips + 1) (stats.sharedCabEarnings + earnings) (stats.sharedCabRideTime + rideTime) trip.driverId day
    Nothing -> do
      merchantOpCity <- CQMOC.findById trip.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound trip.merchantOperatingCityId.getId)
      statsId <- generateGUIDText
      SQDS.create
        DDS.DailyStats
          { id = statsId,
            driverId = trip.driverId,
            totalEarnings = 0,
            numRides = 0,
            totalDistance = 0,
            merchantLocalDate = day,
            currency = merchantOpCity.currency,
            distanceUnit = merchantOpCity.distanceUnit,
            activatedValidRides = 0,
            referralEarnings = 0,
            referralCounts = 0,
            d2dReferralEarnings = 0,
            d2dReferralCounts = 0,
            d2dActivatedValidRides = 0,
            sharedCabTrips = 1,
            sharedCabEarnings = earnings,
            sharedCabRideTime = rideTime,
            payoutStatus = DDS.Initialized,
            payoutOrderId = Nothing,
            payoutOrderStatus = Nothing,
            tollCharges = 0,
            bonusEarnings = 0,
            createdAt = now,
            updatedAt = now,
            cancellationCharges = 0,
            tipAmount = 0,
            totalRideTime = 0,
            numDriversOnboarded = 0,
            numFleetsOnboarded = 0,
            merchantId = Just trip.merchantId,
            merchantOperatingCityId = Just trip.merchantOperatingCityId,
            onlineDuration = Nothing
          }
