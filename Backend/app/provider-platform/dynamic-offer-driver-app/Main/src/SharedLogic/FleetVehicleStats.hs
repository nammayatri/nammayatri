module SharedLogic.FleetVehicleStats where

import Data.Time hiding (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Domain.Types.FleetRcDailyStats as DFRDS
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.TransporterConfig as DTTC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.Queries.FleetRcDailyStats as QFRDS
import qualified Storage.Queries.RideDetails as QRideDetails

buildInitialFleetVehicleDailyStats :: (MonadFlow m) => Text -> Text -> Day -> DTTC.TransporterConfig -> Meters -> Seconds -> HighPrecMoney -> m DFRDS.FleetRcDailyStats
buildInitialFleetVehicleDailyStats fleetOwnerId rcId merchantLocalDate transporterConfig rideDistance rideDuration totalEarnings = do
  now <- getCurrentTime
  pure
    DFRDS.FleetRcDailyStats
      { fleetOwnerId = fleetOwnerId,
        rcId = rcId,
        merchantLocalDate = merchantLocalDate,
        totalCompletedRides = 1,
        totalEarnings = totalEarnings,
        rideDistance = rideDistance,
        rideDuration = rideDuration,
        currency = Just transporterConfig.currency,
        merchantId = Just transporterConfig.merchantId,
        merchantOperatingCityId = Just transporterConfig.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

upsertFleetVehicleDailyStats :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> Meters -> Seconds -> HighPrecMoney -> m ()
upsertFleetVehicleDailyStats fleetOwnerId rcId transporterConfig rideDistance rideDuration totalEarnings = do
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let merchantLocalDate = utctDay now
  res <- QFRDS.findByPrimaryKey fleetOwnerId merchantLocalDate rcId
  case res of
    Nothing -> do
      dailyStats <- buildInitialFleetVehicleDailyStats fleetOwnerId rcId merchantLocalDate transporterConfig rideDistance rideDuration totalEarnings
      QFRDS.create dailyStats
    Just s -> do
      let addMoney (HighPrecMoney a) (HighPrecMoney b) = HighPrecMoney (a + b)
      let addMeters (Meters a) (Meters b) = Meters (a + b)
      let addSeconds (Seconds a) (Seconds b) = Seconds (a + b)

      let totalEarnings' = addMoney totalEarnings s.totalEarnings
          rideDistance' = addMeters rideDistance s.rideDistance
          rideDuration' = addSeconds rideDuration s.rideDuration
          totalCompletedRides = s.totalCompletedRides + 1

      let dailyStats =
            s
              { DFRDS.totalCompletedRides = totalCompletedRides,
                DFRDS.totalEarnings = totalEarnings',
                DFRDS.rideDistance = rideDistance',
                DFRDS.rideDuration = rideDuration'
              }

      QFRDS.updateByPrimaryKey dailyStats

updateFleetVehicleDailyStats :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> DTTC.TransporterConfig -> Ride.Ride -> m ()
updateFleetVehicleDailyStats fleetOwnerId transporterConfig ride = do
  rideDetails <- QRideDetails.findById ride.id >>= fromMaybeM (InvalidRequest $ "RideDetailsNotFound: " <> ride.id.getId)
  rcId <- fromMaybeM (InvalidRequest $ "RcIdNotFound: " <> ride.id.getId) rideDetails.rcId

  let rideDistance = fromMaybe (Meters 0) ride.chargeableDistance
  let rideDuration = fromMaybe (Seconds 0) rideDurationSeconds
  let totalEarnings = fromMaybe (HighPrecMoney 0) ride.fare

  upsertFleetVehicleDailyStats fleetOwnerId rcId transporterConfig rideDistance rideDuration totalEarnings
  where
    rideDurationSeconds :: Maybe Seconds
    rideDurationSeconds = do
      start <- ride.tripStartTime
      end <- ride.tripEndTime
      let diff = diffUTCTime end start
      pure $ Seconds (floor diff)
