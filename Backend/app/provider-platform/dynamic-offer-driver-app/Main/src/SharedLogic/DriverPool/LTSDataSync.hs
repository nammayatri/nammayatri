module SharedLogic.DriverPool.LTSDataSync
  ( syncDriverPoolDataToLTS,
    DriverPoolDataUpdate (..),
    SetField (..),
    set,
    emptyUpdate,
  )
where

import qualified Data.Time.Calendar as Days
import Domain.Types.Common (DriverMode)
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import Domain.Types.Person (Driver, Gender)
import Domain.Types.ServiceTierType (ServiceTierType)
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Types.Version (CloudType, Device, Version)
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.DriverPoolData as DPD

-- | Represents an optional field update.
--   Unchanged  = don't touch this field in LTS (default in emptyUpdate)
--   Set value  = write this value to LTS (value can itself be Maybe for nullable fields)
data SetField a = Unchanged | Set a
  deriving (Show, Eq)

-- | Convenience: wrap a value in Set.
set :: a -> SetField a
set = Set

-- | Represents a partial update to the driver's pool data in LTS.
-- Only fields marked 'Set' will be written; 'Unchanged' means "skip this field".
-- This is a MERGE operation — Unchanged fields keep their existing LTS values.
data DriverPoolDataUpdate = DriverPoolDataUpdate
  { -- Class 2 fields (sync, LTS authoritative for dispatch)
    active :: SetField Bool,
    mode :: SetField (Maybe DriverMode),
    onRide :: SetField Bool,
    onRideTripCategory :: SetField (Maybe Text),
    hasAdvanceBooking :: SetField (Maybe Bool),
    latestScheduledBooking :: SetField (Maybe UTCTime),
    latestScheduledPickup :: SetField (Maybe Maps.LatLong),
    deviceToken :: SetField (Maybe FCM.FCMRecipientToken),
    goHomeStatus :: SetField (Maybe DDGR.DriverGoHomeRequestStatus),
    variant :: SetField VehicleVariant,
    selectedServiceTiers :: SetField [ServiceTierType],
    -- Class 1 fields (sync, driver DB authoritative)
    enabled :: SetField Bool,
    blocked :: SetField Bool,
    subscribed :: SetField Bool,
    canSwitchToRental :: SetField Bool,
    canSwitchToInterCity :: SetField Bool,
    canSwitchToIntraCity :: SetField Bool,
    enableForAirport :: SetField (Maybe DI.AirportRestrictionType),
    forwardBatchingEnabled :: SetField Bool,
    isSpecialLocWarrior :: SetField Bool,
    tollRouteBlockedTill :: SetField (Maybe UTCTime),
    softBlockStiers :: SetField (Maybe [ServiceTierType]),
    acUsageRestrictionType :: SetField (Maybe Text),
    acRestrictionLiftCount :: SetField Int,
    tripDistanceMinThreshold :: SetField (Maybe Meters),
    tripDistanceMaxThreshold :: SetField (Maybe Meters),
    maxPickupRadius :: SetField (Maybe Meters),
    isPetModeEnabled :: SetField Bool,
    chargesEnabled :: SetField Bool,
    bankAccountPaymentMode :: SetField (Maybe DMPM.PaymentMode),
    language :: SetField (Maybe Maps.Language),
    gender :: SetField Gender,
    driverTag :: SetField (Maybe [LYT.TagNameValueExpiry]),
    clientDevice :: SetField (Maybe Device),
    clientSdkVersion :: SetField (Maybe Version),
    clientBundleVersion :: SetField (Maybe Version),
    clientConfigVersion :: SetField (Maybe Version),
    vehicleTags :: SetField (Maybe [Text]),
    mYManufacturing :: SetField (Maybe Days.Day),
    fleetOwnerId :: SetField (Maybe Text),
    -- On-ride / forward batching fields
    driverTripEndLocation :: SetField (Maybe Maps.LatLong),
    hasRideStarted :: SetField (Maybe Bool),
    -- Vehicle attributes for service tier usage restriction
    airConditionScore :: SetField (Maybe Double),
    airConditioned :: SetField (Maybe Bool),
    luggageCapacity :: SetField (Maybe Int),
    vehicleRating :: SetField (Maybe Double),
    registrationNo :: SetField Text,
    cloudType :: SetField (Maybe CloudType)
  }

emptyUpdate :: DriverPoolDataUpdate
emptyUpdate =
  DriverPoolDataUpdate
    { active = Unchanged,
      mode = Unchanged,
      onRide = Unchanged,
      onRideTripCategory = Unchanged,
      hasAdvanceBooking = Unchanged,
      latestScheduledBooking = Unchanged,
      latestScheduledPickup = Unchanged,
      deviceToken = Unchanged,
      goHomeStatus = Unchanged,
      variant = Unchanged,
      selectedServiceTiers = Unchanged,
      enabled = Unchanged,
      blocked = Unchanged,
      subscribed = Unchanged,
      canSwitchToRental = Unchanged,
      canSwitchToInterCity = Unchanged,
      canSwitchToIntraCity = Unchanged,
      enableForAirport = Unchanged,
      forwardBatchingEnabled = Unchanged,
      isSpecialLocWarrior = Unchanged,
      tollRouteBlockedTill = Unchanged,
      softBlockStiers = Unchanged,
      acUsageRestrictionType = Unchanged,
      acRestrictionLiftCount = Unchanged,
      tripDistanceMinThreshold = Unchanged,
      tripDistanceMaxThreshold = Unchanged,
      maxPickupRadius = Unchanged,
      isPetModeEnabled = Unchanged,
      chargesEnabled = Unchanged,
      bankAccountPaymentMode = Unchanged,
      language = Unchanged,
      gender = Unchanged,
      driverTag = Unchanged,
      clientDevice = Unchanged,
      clientSdkVersion = Unchanged,
      clientBundleVersion = Unchanged,
      clientConfigVersion = Unchanged,
      vehicleTags = Unchanged,
      mYManufacturing = Unchanged,
      fleetOwnerId = Unchanged,
      driverTripEndLocation = Unchanged,
      hasRideStarted = Unchanged,
      airConditionScore = Unchanged,
      airConditioned = Unchanged,
      luggageCapacity = Unchanged,
      vehicleRating = Unchanged,
      registrationNo = Unchanged,
      cloudType = Unchanged
    }

-- | Synchronously update driver pool data in Redis/LTS.
-- MERGE semantics: only fields marked 'Set' are written; 'Unchanged' fields
-- keep their existing values.
-- Writes are routed to the correct cloud based on the driver's cloudType:
-- if it matches the deployment's cloudType → primary, otherwise → secondary.
-- If no LTS entry exists yet, the update is skipped — cold-start initialisation
-- is the responsibility of getOrBuildDriverPoolDataBatch (called at pool-query
-- time), which builds the full entry from DB and populates LTS. Any field
-- changes that arrive before the first pool query will be captured by that
-- rebuild, so no data is permanently lost.
syncDriverPoolDataToLTS ::
  ( MonadFlow m,
    CacheFlow m r,
    Log m,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id Driver ->
  DriverPoolDataUpdate ->
  m ()
syncDriverPoolDataToLTS driverId update = do
  deploymentCloudType <- asks (.cloudType)
  Redis.withWaitOnLockRedisWithExpiry (driverPoolSyncLockKey driverId) 3 10 $ do
    mbExisting <- Redis.withLTSRedis $ Redis.safeGet (DPD.driverPoolDataKey driverId)
    case mbExisting of
      Just existing -> do
        let merged = applyUpdate update existing
        cleanupOldCloudKey deploymentCloudType existing merged
        DPD.setDriverPoolDataByCloud deploymentCloudType merged
      Nothing -> do
        mbExisting' <- Redis.withSecondaryLTSRedis $ Redis.safeGet (DPD.driverPoolDataKey driverId)
        case mbExisting' of
          Just existing' -> do
            let merged = applyUpdate update existing'
            cleanupOldCloudKey deploymentCloudType existing' merged
            DPD.setDriverPoolDataByCloud deploymentCloudType merged
          Nothing ->
            logError $ "syncDriverPoolDataToLTS: no LTS entry for driver in any cloud" <> driverId.getId <> " yet — skipping until getOrBuildDriverPoolDataBatch initialises it"

-- | When a driver's cloudType changes, the key in the old cloud becomes orphaned.
-- Delete it so reads don't return stale data.
cleanupOldCloudKey ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    Redis.HedisLTSFlowEnv r
  ) =>
  Maybe CloudType ->
  DPD.DriverPoolData ->
  DPD.DriverPoolData ->
  m ()
cleanupOldCloudKey deploymentCloudType old new =
  when (old.cloudType /= new.cloudType) $ do
    let key = DPD.driverPoolDataKey old.driverId
    if deploymentCloudType == old.cloudType
      then Redis.withLTSRedis $ Redis.del key
      else Redis.withSecondaryLTSRedis $ Redis.del key

driverPoolSyncLockKey :: Id Driver -> Text
driverPoolSyncLockKey driverId = "driver-pool-sync-lock:" <> driverId.getId

-- | Apply a partial update to an existing DriverPoolData record.
-- Only fields marked 'Set' are overwritten; 'Unchanged' fields keep current values.
applyUpdate :: DriverPoolDataUpdate -> DPD.DriverPoolData -> DPD.DriverPoolData
applyUpdate u d =
  d
    { DPD.active = applyField u.active d.active,
      DPD.mode = applyField u.mode d.mode,
      DPD.onRide = applyField u.onRide d.onRide,
      DPD.onRideTripCategory = applyField u.onRideTripCategory d.onRideTripCategory,
      DPD.hasAdvanceBooking = applyField u.hasAdvanceBooking d.hasAdvanceBooking,
      DPD.latestScheduledBooking = applyField u.latestScheduledBooking d.latestScheduledBooking,
      DPD.latestScheduledPickup = applyField u.latestScheduledPickup d.latestScheduledPickup,
      DPD.deviceToken = applyField u.deviceToken d.deviceToken,
      DPD.goHomeStatus = applyField u.goHomeStatus d.goHomeStatus,
      DPD.variant = applyField u.variant d.variant,
      DPD.selectedServiceTiers = applyField u.selectedServiceTiers d.selectedServiceTiers,
      DPD.enabled = applyField u.enabled d.enabled,
      DPD.blocked = applyField u.blocked d.blocked,
      DPD.subscribed = applyField u.subscribed d.subscribed,
      DPD.canSwitchToRental = applyField u.canSwitchToRental d.canSwitchToRental,
      DPD.canSwitchToInterCity = applyField u.canSwitchToInterCity d.canSwitchToInterCity,
      DPD.canSwitchToIntraCity = applyField u.canSwitchToIntraCity d.canSwitchToIntraCity,
      DPD.enableForAirport = applyField u.enableForAirport d.enableForAirport,
      DPD.forwardBatchingEnabled = applyField u.forwardBatchingEnabled d.forwardBatchingEnabled,
      DPD.isSpecialLocWarrior = applyField u.isSpecialLocWarrior d.isSpecialLocWarrior,
      DPD.tollRouteBlockedTill = applyField u.tollRouteBlockedTill d.tollRouteBlockedTill,
      DPD.softBlockStiers = applyField u.softBlockStiers d.softBlockStiers,
      DPD.acUsageRestrictionType = applyField u.acUsageRestrictionType d.acUsageRestrictionType,
      DPD.acRestrictionLiftCount = applyField u.acRestrictionLiftCount d.acRestrictionLiftCount,
      DPD.tripDistanceMinThreshold = applyField u.tripDistanceMinThreshold d.tripDistanceMinThreshold,
      DPD.tripDistanceMaxThreshold = applyField u.tripDistanceMaxThreshold d.tripDistanceMaxThreshold,
      DPD.maxPickupRadius = applyField u.maxPickupRadius d.maxPickupRadius,
      DPD.isPetModeEnabled = applyField u.isPetModeEnabled d.isPetModeEnabled,
      DPD.chargesEnabled = applyField u.chargesEnabled d.chargesEnabled,
      DPD.bankAccountPaymentMode = applyField u.bankAccountPaymentMode d.bankAccountPaymentMode,
      DPD.language = applyField u.language d.language,
      DPD.gender = applyField u.gender d.gender,
      DPD.driverTag = applyField u.driverTag d.driverTag,
      DPD.clientDevice = applyField u.clientDevice d.clientDevice,
      DPD.clientSdkVersion = applyField u.clientSdkVersion d.clientSdkVersion,
      DPD.clientBundleVersion = applyField u.clientBundleVersion d.clientBundleVersion,
      DPD.clientConfigVersion = applyField u.clientConfigVersion d.clientConfigVersion,
      DPD.vehicleTags = applyField u.vehicleTags d.vehicleTags,
      DPD.mYManufacturing = applyField u.mYManufacturing d.mYManufacturing,
      DPD.fleetOwnerId = applyField u.fleetOwnerId d.fleetOwnerId,
      DPD.driverTripEndLocation = applyField u.driverTripEndLocation d.driverTripEndLocation,
      DPD.hasRideStarted = applyField u.hasRideStarted d.hasRideStarted,
      DPD.airConditionScore = applyField u.airConditionScore d.airConditionScore,
      DPD.airConditioned = applyField u.airConditioned d.airConditioned,
      DPD.luggageCapacity = applyField u.luggageCapacity d.luggageCapacity,
      DPD.vehicleRating = applyField u.vehicleRating d.vehicleRating,
      DPD.registrationNo = applyField u.registrationNo d.registrationNo,
      DPD.cloudType = applyField u.cloudType d.cloudType
    }

-- | Apply a single field update: Set overwrites, Unchanged keeps current.
applyField :: SetField a -> a -> a
applyField Unchanged current = current
applyField (Set newVal) _ = newVal
