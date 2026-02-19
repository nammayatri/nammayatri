module SharedLogic.RuleBasedTierUpgrade
  ( computeEligibleUpgradeTiers,
  )
where

import Data.Either.Extra (eitherToMaybe)
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.ServiceTierType as ST
import qualified Domain.Types.TransporterConfig as DTTC
import qualified Domain.Types.UpgradedTier as DU
import qualified Domain.Types.Yudhishthira as Yudhishthira
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Constants
import Tools.Error

mkTierInfos :: UTCTime -> [ST.ServiceTierType] -> [DU.UpgradedTier]
mkTierInfos now tiers =
  [ DU.UpgradedTier
      { tier = t,
        lastComputed = now
      }
    | t <- tiers
  ]

computeMergedUpgrades :: UTCTime -> Seconds -> [DU.UpgradedTier] -> [ST.ServiceTierType] -> [DU.UpgradedTier]
computeMergedUpgrades now retentionSeconds existing newlyEligible =
  let isRetained DU.UpgradedTier {..} = tier `elem` newlyEligible || nominalDiffTimeToSeconds (diffUTCTime now lastComputed) <= retentionSeconds
      retained = filter isRetained existing
      existingTiers = (.tier) <$> existing
      totallyNew = mkTierInfos now $ filter (\tier -> tier `notElem` existingTiers) newlyEligible
   in retained <> totallyNew

eligibleTiersFromTags :: [LYT.TagNameValue] -> [ST.ServiceTierType]
eligibleTiersFromTags tags = mapMaybe matchTag tags
  where
    matchTag t
      | t == acPriorityEligibleTag = Just ST.AC_PRIORITY
      | otherwise = Nothing

computeEligibleUpgradeTiers ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  DRide.Ride ->
  DTTC.TransporterConfig ->
  m ()
computeEligibleUpgradeTiers ride transporterConfig =
  when (isJust transporterConfig.upgradeTierDropRetentionTime) $
    fork ("computeEligibleUpgradeTiers for driver: " <> ride.driverId.getId) $ do
      now <- getCurrentTime
      driverStats <- runInReplica $ QDriverStats.findById ride.driverId >>= fromMaybeM (InternalError $ "computeEligibleUpgradeTiers: DriverStatsNotFound: " <> ride.driverId.getId)
      rideDetails <- runInReplica $ QRideDetails.findById ride.id >>= fromMaybeM (InternalError $ "computeEligibleUpgradeTiers: RideDetailsNotFound: " <> ride.id.getId)
      driverInfo <- QDriverInformation.findById ride.driverId >>= fromMaybeM (InternalError $ "computeEligibleUpgradeTiers: DriverInfoNotFound: " <> ride.driverId.getId)
      vehicle <- runInReplica $ QVehicle.findById ride.driverId >>= fromMaybeM (InternalError $ "computeEligibleUpgradeTiers: VehicleNotFound for driver: " <> ride.driverId.getId)
      let upgradeTierTagData =
            Yudhishthira.UpgradeTierTagData
              { driverRating = realToFrac <$> driverStats.rating,
                vehicleAgeInMonths = (.getMonths) <$> rideDetails.vehicleAge,
                ridesCount = driverStats.totalRides,
                favRiderCount = driverStats.favRiderCount,
                vehicleVariant = vehicle.variant
              }
      nammaTags <- withTryCatch "computeNammaTags:UpgradeTier" (LYDL.computeNammaTagsWithDebugLog (Id.cast ride.merchantOperatingCityId) Yudhishthira.UpgradeTier upgradeTierTagData)
      let newEligibleTiers = eligibleTiersFromTags $ fromMaybe [] $ eitherToMaybe nammaTags
          newUpgrades = computeMergedUpgrades now (fromMaybe 0 transporterConfig.upgradeTierDropRetentionTime) (fromMaybe [] driverInfo.ruleBasedUpgradeTiers) newEligibleTiers
          vehicleNewUpgrades = computeMergedUpgrades now (fromMaybe 0 transporterConfig.upgradeTierDropRetentionTime) (fromMaybe [] vehicle.ruleBasedUpgradeTiers) newEligibleTiers
      QDriverInformation.updateUpgradedTiers (Just newUpgrades) driverInfo.driverId
      QVehicle.updateRuleBasedUpgradeTiers (Just vehicleNewUpgrades) vehicle.driverId
