{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.VehicleServiceTier where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Time (diffDays, utctDay)
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverStats as DDriverStats
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTPC
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleCategory as VC
import qualified Domain.Types.VehicleServiceTier as DVST
import Domain.Utils (getVehicleAge)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

selectVehicleTierForDriverWithUsageRestriction :: Bool -> DI.DriverInformation -> DV.Vehicle -> [DVST.VehicleServiceTier] -> Maybe DDriverStats.DriverStats -> UTCTime -> [(DVST.VehicleServiceTier, Bool)]
selectVehicleTierForDriverWithUsageRestriction onlyAutoSelected driverInfo vehicle cityVehicleServiceTiers mbDriverStats now =
  map mapUsageRestriction $ filter filterVehicleTier cityVehicleServiceTiers
  where
    vehicleAgeInMonths = getVehicleAge vehicle.mYManufacturing now

    mapUsageRestriction :: DVST.VehicleServiceTier -> (DVST.VehicleServiceTier, Bool)
    mapUsageRestriction vehicleServiceTier = do
      let _seatingCapacityCheck = compareNumber vehicle.capacity vehicleServiceTier.seatingCapacity
          luggageCapacityCheck = compareNumber vehicle.luggageCapacity vehicleServiceTier.luggageCapacity
          airConditionedCheck =
            (vehicleServiceTier.vehicleCategory == Just VC.AMBULANCE)
              || ( (compareNumber vehicleServiceTier.airConditionedThreshold driverInfo.airConditionScore)
                     && (isNothing vehicleServiceTier.airConditionedThreshold || vehicle.airConditioned /= Just False)
                 )
          driverRatingCheck = compareNumber' False (mbDriverStats >>= (.rating)) vehicleServiceTier.driverRating
          vehicleRatingCheck = compareNumber' (fromMaybe False vehicleServiceTier.allowNullVehicleRating) vehicle.vehicleRating vehicleServiceTier.vehicleRating
          mfcCheck = case (vehicleAgeInMonths, vehicleServiceTier.vehicleAgeThreshold) of
            (Just (Months age), Just (Months threshold)) -> age < threshold
            _ -> True
          cancellationRateCheck =
            case (vehicleServiceTier.cancellationRateConfig, mbDriverStats) of
              (Just cfg, Just stats) ->
                not (stats.totalRides > cfg.cancellationRideThreshold && driverCancellationRate stats > cfg.cancellationRate)
              _ -> True

      let usageRestricted = not (luggageCapacityCheck && airConditionedCheck && driverRatingCheck && vehicleRatingCheck && mfcCheck && cancellationRateCheck)
      (vehicleServiceTier, usageRestricted)

    filterVehicleTier vehicleServiceTier = vehicle.variant `elem` (if onlyAutoSelected then vehicleServiceTier.autoSelectedVehicleVariant else vehicleServiceTier.allowedVehicleVariant)

    compareNumber :: Ord a => Maybe a -> Maybe a -> Bool
    compareNumber mbX mbY =
      case (mbX, mbY) of
        (Just x, Just y) -> x >= y
        _ -> True

    compareNumber' :: Ord a => Bool -> Maybe a -> Maybe a -> Bool
    compareNumber' allow mbX mbY =
      case (mbX, mbY) of
        (Just x, Just y) -> x >= y
        (Nothing, Just _) -> allow
        _ -> True

    driverCancellationRate :: DDriverStats.DriverStats -> Centesimal
    driverCancellationRate stats =
      (fromIntegral (fromMaybe 0 stats.ridesCancelled) / fromIntegral stats.totalRides)

fetchVehicleTierForDriverWithUsageRestriction ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Bool ->
  Maybe DI.DriverInformation ->
  Maybe DV.Vehicle ->
  Maybe DDriverStats.DriverStats ->
  Maybe [DVST.VehicleServiceTier] ->
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  m [(DVST.VehicleServiceTier, Bool)]
fetchVehicleTierForDriverWithUsageRestriction onlyAutoSelected mbDriverInfo mbVehicle mbDriverStats mbCityServiceTiers personId merchantOpCityId = do
  driverInfo <- maybe (QDI.findById personId >>= fromMaybeM DriverInfoNotFound) pure mbDriverInfo
  vehicle <- maybe (QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)) pure mbVehicle
  driverStats <- maybe (QDriverStats.findById personId) (pure . Just) mbDriverStats
  cityServiceTiers <- maybe (CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing Nothing) pure mbCityServiceTiers
  now <- getCurrentTime
  pure $ selectVehicleTierForDriverWithUsageRestriction onlyAutoSelected driverInfo vehicle cityServiceTiers driverStats now

-- | Mirrors Driver.hs's default-tier derivation. Picks the highest-AC tier
-- whose `defaultForVehicleVariant` contains the driver's variant *and* whose
-- serviceTierType is in the city's supportedServiceTiers list. Falls back to
-- any default-for-variant tier if the intersection is empty. Also returns
-- whether AC is currently working for this driver (using the picked tier's
-- threshold) and whether the vehicle is supported at all in this city.
getDriverDefaultSupportedServiceTier ::
  DV.Vehicle ->
  DI.DriverInformation ->
  DTPC.TransporterConfig ->
  [DTC.ServiceTierType] ->
  [DVST.VehicleServiceTier] ->
  UTCTime ->
  (Bool, Maybe DVST.VehicleServiceTier, Bool)
getDriverDefaultSupportedServiceTier vehicle driverInfo transporterConfig supportedServiceTiers cityServiceTiers now =
  let allVehicleSupportedDefaultServiceTiers =
        sortOn (fmap Down . (.airConditionedThreshold)) $
          filter
            (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant && vst.serviceTierType `elem` supportedServiceTiers)
            cityServiceTiers
      isVehicleSupported = not $ null allVehicleSupportedDefaultServiceTiers
      mbDefaultServiceTierItem =
        if null allVehicleSupportedDefaultServiceTiers
          then find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
          else listToMaybe allVehicleSupportedDefaultServiceTiers
      checkIfACWorking = isACWorkingForDefault driverInfo transporterConfig mbDefaultServiceTierItem now
   in (checkIfACWorking, mbDefaultServiceTierItem, isVehicleSupported)

-- | Like 'getDriverDefaultSupportedServiceTier' but ignores the city's
-- supportedServiceTiers list — returns the highest-AC tier whose
-- `defaultForVehicleVariant` contains the driver's variant, regardless of
-- whether that tier is currently marked as supported in the city.
getDriverDefaultServiceTier ::
  DV.Vehicle ->
  DI.DriverInformation ->
  DTPC.TransporterConfig ->
  [DVST.VehicleServiceTier] ->
  UTCTime ->
  (Bool, Maybe DVST.VehicleServiceTier)
getDriverDefaultServiceTier vehicle driverInfo transporterConfig cityServiceTiers now =
  let mbDefaultServiceTierItem =
        listToMaybe $
          sortOn (fmap Down . (.airConditionedThreshold)) $
            filter (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
      checkIfACWorking = isACWorkingForDefault driverInfo transporterConfig mbDefaultServiceTierItem now
   in (checkIfACWorking, mbDefaultServiceTierItem)

isACWorkingForDefault ::
  DI.DriverInformation ->
  DTPC.TransporterConfig ->
  Maybe DVST.VehicleServiceTier ->
  UTCTime ->
  Bool
isACWorkingForDefault driverInfo transporterConfig mbDefaultServiceTierItem now =
  case mbDefaultServiceTierItem >>= (.airConditionedThreshold) of
    Nothing -> False
    Just acThreshold ->
      fromMaybe 0 driverInfo.airConditionScore <= acThreshold
        && maybe True (\lastCheckedAt -> fromInteger (diffDays (utctDay now) (utctDay lastCheckedAt)) >= transporterConfig.acStatusCheckGap) driverInfo.lastACStatusCheckedAt

-- | Self-heal `selectedServiceTiers` on Vehicle + LTS pool data when either is empty.
-- Called from the driver-info handlers (driver UI and dashboard) with the current
-- vehicle.selectedServiceTiers as input. Intended to run inside a fork.
--
-- Behavior:
--   * input tiers empty  → resolve the driver's default tier and write it to DB
--     (which cascades to LTS via QVehicle.updateSelectedServiceTiers).
--   * input non-empty but LTS pool data has empty selectedServiceTiers → push
--     the input tiers to LTS via syncDriverPoolDataToLTS.
--   * otherwise no-op.
backfillSelectedServiceTiers ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  [DTC.ServiceTierType] ->
  DV.Vehicle ->
  DI.DriverInformation ->
  DTPC.TransporterConfig ->
  Id DMOC.MerchantOperatingCity ->
  m ()
backfillSelectedServiceTiers inputTiers vehicle driverInfo transporterConfig merchantOpCityId
  | null inputTiers = do
    now <- getCurrentTime
    cityServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing Nothing
    let (_, mbDefault) = getDriverDefaultServiceTier vehicle driverInfo transporterConfig cityServiceTiers now
    case mbDefault of
      Nothing ->
        logWarning $ "backfillSelectedServiceTiers: no default service tier for driver " <> vehicle.driverId.getId <> " variant=" <> show vehicle.variant
      Just defaultTier -> do
        logInfo $ "backfillSelectedServiceTiers: empty selectedServiceTiers for driver " <> vehicle.driverId.getId <> ", inserting default " <> show defaultTier.serviceTierType
        QVehicle.updateSelectedServiceTiers [defaultTier.serviceTierType] vehicle.driverId
  | otherwise = do
    let driverId = cast @DP.Person @DP.Driver vehicle.driverId
    mbLtsData <- listToMaybe <$> DPD.getDriverPoolDataBatch [driverId]
    whenJust mbLtsData $ \ltsData ->
      when (null ltsData.selectedServiceTiers) $ do
        logInfo $ "backfillSelectedServiceTiers: LTS selectedServiceTiers empty for driver " <> vehicle.driverId.getId <> ", pushing DB tiers"
        LTSSync.syncDriverPoolDataToLTS driverId $
          LTSSync.emptyUpdate {LTSSync.selectedServiceTiers = LTSSync.Set inputTiers}
