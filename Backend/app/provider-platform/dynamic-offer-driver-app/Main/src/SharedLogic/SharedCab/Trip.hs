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

import Domain.Types.IntegratedBPPConfig (IntegratedBPPConfig)
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.Person as SP
import Domain.Types.SharedCabSession (SessionStatus (ENDED), SharedCabSession)
import qualified Domain.Types.Vehicle as DVehicle
import Domain.Types.VehicleTrip
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.VehicleTrip as QVT

-- | Held across the rider-app call, whose client gives up after 5 s.
withTripLock :: (Redis.HedisFlow m r, MonadMask m) => Text -> m a -> m a
withTripLock plate = Redis.withWaitAndLockRedis ("sharedcab:trip:lock:" <> plate) 10 100000

-- | The run rider-app's live session is on.
data RiderRun = RiderRun {tripId :: Text, routeCode :: Text, walkupCount :: Int}
  deriving (Show, Eq)

riderRun :: SharedCabSession -> Maybe RiderRun
riderRun s
  | s.status == ENDED = Nothing
  | otherwise = Just RiderRun {tripId = s.tripId, routeCode = s.route.code, walkupCount = s.walkupCount}

data TripAction
  = TripUnchanged
  | SetWalkups (Id VehicleTrip) Int
  | CloseTrip (Id VehicleTrip) VehicleTripEndReason
  | OpenTrip (Maybe (Id VehicleTrip, VehicleTripEndReason)) RiderRun
  deriving (Show, Eq)

-- | `endReason` closes the ACTIVE row when rider-app has no live session; a row for another run is closed as a route change of the same
-- driver, or as abandoned by another.
tripAction :: Id SP.Person -> VehicleTripEndReason -> Maybe VehicleTrip -> Maybe RiderRun -> TripAction
tripAction driverId endReason active = \case
  Just run -> case active of
    Just t
      | t.id.getId == run.tripId -> if t.walkupCount == run.walkupCount then TripUnchanged else SetWalkups t.id run.walkupCount
    _ -> OpenTrip ((\t -> (t.id, if t.driverId == driverId then ROUTE_CHANGED else SESSION_TIMEOUT)) <$> active) run
  Nothing -> maybe TripUnchanged (\t -> CloseTrip t.id endReason) active

syncTrip ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id SP.Person ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  IntegratedBPPConfig ->
  DVehicle.Vehicle ->
  VehicleTripEndReason ->
  Maybe RiderRun ->
  m ()
syncTrip driverId merchantId merchantOperatingCityId integratedBppConfig vehicle endReason mbRun = do
  active <- QVT.findActiveByVehicleNumber vehicle.registrationNo
  case tripAction driverId endReason active mbRun of
    TripUnchanged -> pure ()
    SetWalkups tripId count -> QVT.updateWalkupCount count tripId
    CloseTrip tripId reason -> closeTrip reason tripId
    OpenTrip mbClose run -> do
      whenJust mbClose $ \(tripId, reason) -> closeTrip reason tripId
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
            createdAt = now,
            updatedAt = now
          }

closeTrip :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => VehicleTripEndReason -> Id VehicleTrip -> m ()
closeTrip reason tripId = do
  now <- getCurrentTime
  QVT.closeTrip (if reason == SESSION_TIMEOUT then ABANDONED else COMPLETED) (Just reason) (Just now) tripId
