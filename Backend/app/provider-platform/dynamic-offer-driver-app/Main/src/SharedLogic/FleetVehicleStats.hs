module SharedLogic.FleetVehicleStats where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Calendar
import qualified Domain.Types.FleetRcAssociationDailyStats as DFRADS
import qualified Domain.Types.TransporterConfig as DTTC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Storage.Queries.FleetRcAssociationDailyStats as QFRADS

buildInitialFleetVehicleDailyStats :: (MonadFlow m) => Text -> Text -> Day -> DTTC.TransporterConfig -> m DFRADS.FleetRcAssociationDailyStats
buildInitialFleetVehicleDailyStats fleetOwnerId rcId merchantLocalDate transporterConfig = do
  now <- getCurrentTime
  pure
    DFRADS.FleetRcAssociationDailyStats
      { fleetOwnerId = fleetOwnerId,
        rcId = rcId,
        merchantLocalDate = merchantLocalDate,
        totalCompletedRides = 0,
        totalEarnings = Nothing,
        rideDistance = Nothing,
        rideDuration = Nothing,
        currency = Just transporterConfig.currency,
        merchantId = Just transporterConfig.merchantId,
        merchantOperatingCityId = Just transporterConfig.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        distanceUnit = Just transporterConfig.distanceUnit
      }

upsertFleetVehicleDailyStats :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> DTTC.TransporterConfig -> Maybe Meters -> Maybe Seconds -> Maybe HighPrecMoney -> m ()
upsertFleetVehicleDailyStats fleetOwnerId rcId transporterConfig rideDistance rideDuration totalEarnings = do
  nowUTCTime <- getCurrentTime
  let now = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) nowUTCTime
  let merchantLocalDate = utctDay now
  res <- QFRADS.findByFleetOwnerIdAndRcIdAndMerchantLocalDate fleetOwnerId rcId merchantLocalDate
  case res of
    Nothing -> do
      dailyStats <- buildInitialFleetVehicleDailyStats fleetOwnerId rcId merchantLocalDate transporterConfig
      QFRADS.create dailyStats
    Just s -> do
      let totalEarnings = totalEarnings <> s.totalEarnings
      let rideDistance = rideDistance <> s.rideDistance
      let rideDuration = rideDuration <> s.rideDuration
      let totalCompletedRides = s.totalCompletedRides + 1
      QFRADS.updateByFleetOwnerIdAndRcIdAndMerchantLocalDate totalCompletedRides totalEarnings rideDistance rideDuration fleetOwnerId rcId merchantLocalDate
