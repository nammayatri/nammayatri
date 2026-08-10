{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.VehicleServiceTier where

import qualified Dashboard.Common as DC
import qualified Data.HashMap.Strict as HashMap
import Data.List (nub, sortOn)
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
import Lib.Finance (CounterpartyType (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync
import SharedLogic.Finance.WalletAccount (getWalletBalanceByOwner)
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import Storage.Queries.Person.GetNearestDrivers (isTierEligibleForDriver)
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

data ServiceTierFilterMode
  = AutoSelectedVariants
  | AllowedVariants
  | SelectedServiceTiers
  deriving (Show, Eq)

selectVehicleTierForDriverWithUsageRestriction :: ServiceTierFilterMode -> DI.DriverInformation -> DV.Vehicle -> [DVST.VehicleServiceTier] -> Maybe DDriverStats.DriverStats -> Maybe [LYT.TagNameValueExpiry] -> UTCTime -> [(DVST.VehicleServiceTier, Bool)]
selectVehicleTierForDriverWithUsageRestriction mode driverInfo vehicle vehicleServiceTiers mbDriverStats driverTag now =
  map mapUsageRestriction $ filter filterVehicleTier vehicleServiceTiers
  where
    vehicleAgeInMonths = getVehicleAge vehicle.mYManufacturing now
    tierConfigsMap = HashMap.fromList [(vst.serviceTierType, vst) | vst <- vehicleServiceTiers]

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

    filterVehicleTier vehicleServiceTier = case mode of
      AutoSelectedVariants -> vehicle.variant `elem` vehicleServiceTier.autoSelectedVehicleVariant && isCohortEligible
      AllowedVariants -> vehicle.variant `elem` vehicleServiceTier.allowedVehicleVariant && isCohortEligible
      SelectedServiceTiers -> vehicleServiceTier.serviceTierType `elem` vehicle.selectedServiceTiers
      where
        isCohortEligible = isTierEligibleForDriver now driverTag tierConfigsMap vehicleServiceTier.serviceTierType

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
      fromIntegral (fromMaybe 0 stats.ridesCancelled) / fromIntegral stats.totalRides

fetchVehicleTierForDriverWithUsageRestriction ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  ServiceTierFilterMode ->
  Maybe DI.DriverInformation ->
  Maybe DV.Vehicle ->
  Maybe DDriverStats.DriverStats ->
  Maybe [DVST.VehicleServiceTier] ->
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  m [(DVST.VehicleServiceTier, Bool)]
fetchVehicleTierForDriverWithUsageRestriction mode mbDriverInfo mbVehicle mbDriverStats mbCityServiceTiers personId merchantOpCityId = do
  driverInfo <- maybe (QDI.findById personId >>= fromMaybeM DriverInfoNotFound) pure mbDriverInfo
  vehicle <- maybe (QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)) pure mbVehicle
  driverStats <- maybe (QDriverStats.findById personId) (pure . Just) mbDriverStats
  cityServiceTiers <- maybe (CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing) pure mbCityServiceTiers
  -- SelectedServiceTiers mode only re-filters tiers already in Vehicle.selectedServiceTiers and
  -- never consults isTierEligibleForDriver (see filterVehicleTier above), so skip this fetch there
  -- entirely -- it would otherwise add an unconditional Person lookup to several automated,
  -- high-frequency recomputation call sites (rating changes, AC-score changes, etc.) that don't
  -- need it; the real dispatch-time hard gate is enforced independently regardless.
  driverTag <- case mode of
    SelectedServiceTiers -> pure Nothing
    _ -> (.driverTag) <$> (QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId))
  now <- getCurrentTime
  pure $ selectVehicleTierForDriverWithUsageRestriction mode driverInfo vehicle cityServiceTiers driverStats driverTag now

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
    cityServiceTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
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

-- | Serializes checkAndAutoDisableWalletGatedTiers's own concurrent invocations for one driver, since two can otherwise race and undo each other's write.
selectedServiceTiersLockKey :: Id DP.Person -> Text
selectedServiceTiersLockKey driverId = "Driver:SelectedServiceTiers:DId-" <> driverId.getId

-- | Re-checks every wallet-gated tier a driver holds and drops any they can no longer afford.
--   Called automatically after every wallet debit via PostActions, which already gates the call on enableWalletGatedTierCheck -- this function doesn't check the flag itself.
checkAndAutoDisableWalletGatedTiers :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) => Id DP.Person -> Id DMOC.MerchantOperatingCity -> m ()
checkAndAutoDisableWalletGatedTiers driverId merchantOpCityId =
  Redis.withWaitOnLockRedisWithExpiry (selectedServiceTiersLockKey driverId) 5 10 $ do
    mbVehicle <- QVehicle.findById driverId
    whenJust mbVehicle $ \vehicle -> do
      let selectedAutoAcceptTiers = fromMaybe [] vehicle.selectedAutoAcceptTiers
          tiersToCheck = nub (vehicle.selectedServiceTiers <> selectedAutoAcceptTiers)
      unless (null tiersToCheck) $ do
        -- One fetch for the whole city's tier configs, not one per selected tier.
        -- The wallet read happens at most once, only if something is gated.
        allTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
        let tierByType = HashMap.fromList [(tier.serviceTierType, tier) | tier <- allTiers]
            gatedTiers =
              catMaybes
                [ do
                    tier <- HashMap.lookup tierType tierByType
                    cfg <- tier.autoAcceptanceConfig
                    guard cfg.enabled
                    minBalance <- cfg.minWalletBalance
                    pure (tierType, cfg.mode, minBalance)
                  | tierType <- tiersToCheck
                ]
        unless (null gatedTiers) $ do
          mbBalance <- getWalletBalanceByOwner DRIVER driverId.getId
          -- Missing wallet row is not treated as below-minimum -- a transient read miss must never strip a persistent tier selection.
          let walletIneligibleTiers = [(tierType, mode) | (tierType, mode, minBalance) <- gatedTiers, maybe False (< minBalance) mbBalance]
          unless (null walletIneligibleTiers) $ do
            let instantOnlyWalletIneligibleTiers = [t | (t, DC.AutoAcceptOnly) <- walletIneligibleTiers]
                allWalletIneligibleTiers = map fst walletIneligibleTiers
            QVehicle.updateSelectedServiceTiersAndAutoAcceptTiers
              (filter (`notElem` instantOnlyWalletIneligibleTiers) vehicle.selectedServiceTiers)
              (filter (`notElem` allWalletIneligibleTiers) selectedAutoAcceptTiers)
              driverId
