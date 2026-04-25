module SharedLogic.DriverPool.LTSDataSync
  ( syncDriverPoolDataToLTS,
    DriverPoolDataUpdate (..),
    SetField (..),
    set,
    emptyUpdate,
    -- Compile-time safe pool field update
    PoolFieldUpdate,
    mkPoolFieldUpdate,
    runPoolFieldUpdate,
  )
where

import qualified Data.Time.Calendar as Days
import Domain.Types.Common (DriverMode)
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.Person (Driver, Gender)
import Domain.Types.ServiceTierType (ServiceTierType)
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Types.Version (Device, Version)
import Kernel.Utils.Common
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import qualified Lib.Yudhishthira.Types as LYT

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
    totalRides :: SetField Int,
    variant :: SetField VehicleVariant,
    selectedServiceTiers :: SetField [ServiceTierType],
    -- Class 1 fields (sync, driver DB authoritative)
    blocked :: SetField Bool,
    subscribed :: SetField Bool,
    canSwitchToRental :: SetField Bool,
    canSwitchToInterCity :: SetField Bool,
    canSwitchToIntraCity :: SetField Bool,
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
    language :: SetField (Maybe Maps.Language),
    gender :: SetField Gender,
    driverTag :: SetField (Maybe [LYT.TagNameValueExpiry]),
    clientDevice :: SetField (Maybe Device),
    clientSdkVersion :: SetField (Maybe Version),
    clientBundleVersion :: SetField (Maybe Version),
    clientConfigVersion :: SetField (Maybe Version),
    vehicleTags :: SetField (Maybe [Text]),
    mYManufacturing :: SetField (Maybe Days.Day),
    safetyPlusEnabled :: SetField Bool,
    fleetOwnerId :: SetField (Maybe Text),
    -- On-ride / forward batching fields
    driverTripEndLocation :: SetField (Maybe Maps.LatLong),
    hasRideStarted :: SetField (Maybe Bool),
    -- Vehicle attributes for service tier usage restriction
    airConditionScore :: SetField (Maybe Double),
    airConditioned :: SetField (Maybe Bool),
    luggageCapacity :: SetField (Maybe Int),
    vehicleRating :: SetField (Maybe Double),
    registrationNo :: SetField Text
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
      totalRides = Unchanged,
      variant = Unchanged,
      selectedServiceTiers = Unchanged,
      blocked = Unchanged,
      subscribed = Unchanged,
      canSwitchToRental = Unchanged,
      canSwitchToInterCity = Unchanged,
      canSwitchToIntraCity = Unchanged,
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
      language = Unchanged,
      gender = Unchanged,
      driverTag = Unchanged,
      clientDevice = Unchanged,
      clientSdkVersion = Unchanged,
      clientBundleVersion = Unchanged,
      clientConfigVersion = Unchanged,
      vehicleTags = Unchanged,
      mYManufacturing = Unchanged,
      safetyPlusEnabled = Unchanged,
      fleetOwnerId = Unchanged,
      driverTripEndLocation = Unchanged,
      hasRideStarted = Unchanged,
      airConditionScore = Unchanged,
      airConditioned = Unchanged,
      luggageCapacity = Unchanged,
      vehicleRating = Unchanged,
      registrationNo = Unchanged
    }

-- | Synchronously update driver pool data in Redis/LTS.
-- MERGE semantics: only fields marked 'Set' are written; 'Unchanged' fields
-- keep their existing values. If the key doesn't exist yet, skip — the full
-- key is built lazily by getOrBuildDriverPoolDataBatch on first pool query.
syncDriverPoolDataToLTS ::
  (MonadFlow m, Log m, Redis.HedisFlow m r) =>
  Id Driver ->
  DriverPoolDataUpdate ->
  m ()
syncDriverPoolDataToLTS driverId update = do
  mbExisting <- Redis.safeGet (DPD.driverPoolDataKey driverId)
  whenJust mbExisting $ \existing -> do
    let merged = applyUpdate update existing
    DPD.setDriverPoolData merged

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
      DPD.totalRides = applyField u.totalRides d.totalRides,
      DPD.variant = applyField u.variant d.variant,
      DPD.selectedServiceTiers = applyField u.selectedServiceTiers d.selectedServiceTiers,
      DPD.blocked = applyField u.blocked d.blocked,
      DPD.subscribed = applyField u.subscribed d.subscribed,
      DPD.canSwitchToRental = applyField u.canSwitchToRental d.canSwitchToRental,
      DPD.canSwitchToInterCity = applyField u.canSwitchToInterCity d.canSwitchToInterCity,
      DPD.canSwitchToIntraCity = applyField u.canSwitchToIntraCity d.canSwitchToIntraCity,
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
      DPD.language = applyField u.language d.language,
      DPD.gender = applyField u.gender d.gender,
      DPD.driverTag = applyField u.driverTag d.driverTag,
      DPD.clientDevice = applyField u.clientDevice d.clientDevice,
      DPD.clientSdkVersion = applyField u.clientSdkVersion d.clientSdkVersion,
      DPD.clientBundleVersion = applyField u.clientBundleVersion d.clientBundleVersion,
      DPD.clientConfigVersion = applyField u.clientConfigVersion d.clientConfigVersion,
      DPD.vehicleTags = applyField u.vehicleTags d.vehicleTags,
      DPD.mYManufacturing = applyField u.mYManufacturing d.mYManufacturing,
      DPD.safetyPlusEnabled = applyField u.safetyPlusEnabled d.safetyPlusEnabled,
      DPD.fleetOwnerId = applyField u.fleetOwnerId d.fleetOwnerId,
      DPD.driverTripEndLocation = applyField u.driverTripEndLocation d.driverTripEndLocation,
      DPD.hasRideStarted = applyField u.hasRideStarted d.hasRideStarted,
      DPD.airConditionScore = applyField u.airConditionScore d.airConditionScore,
      DPD.airConditioned = applyField u.airConditioned d.airConditioned,
      DPD.luggageCapacity = applyField u.luggageCapacity d.luggageCapacity,
      DPD.vehicleRating = applyField u.vehicleRating d.vehicleRating,
      DPD.registrationNo = applyField u.registrationNo d.registrationNo
    }

-- | Apply a single field update: Set overwrites, Unchanged keeps current.
applyField :: SetField a -> a -> a
applyField Unchanged current = current
applyField (Set newVal) _ = newVal

-- | A pool field update that bundles a DB write with its LTS sync.
-- You cannot execute the DB write without providing the LTS update.
--
-- ENFORCEMENT STRATEGY:
-- 1. All pool-relevant field updates MUST use runPoolFieldUpdate (not raw updateOneWithKV)
-- 2. The generated src-read-only functions (e.g. updateOnRide) should NOT be called directly
--    for pool fields — use the  wrappers from Extra files instead
-- 3. CI/pre-commit hook should grep for direct Se.Set on pool-relevant Beam fields
--    outside of approved wrapper functions to catch violations
--
-- Usage:
--   updateOnRide onRide driverId = runPoolFieldUpdate (cast driverId) $
--     mkPoolFieldUpdate
--       (updateOneWithKV [Se.Set BeamDI.onRide onRide, ...] [...])
--       (emptyUpdate {onRide = Set onRide})
data PoolFieldUpdate m = PoolFieldUpdate
  { _dbWrite :: m (),
    _ltsUpdate :: DriverPoolDataUpdate
  }

-- | Create a pool field update. Both DB write and LTS update are required.
mkPoolFieldUpdate :: m () -> DriverPoolDataUpdate -> PoolFieldUpdate m
mkPoolFieldUpdate = PoolFieldUpdate

-- | Execute a pool field update: runs the DB write, then syncs to LTS.
runPoolFieldUpdate ::
  (MonadFlow m, Log m, Redis.HedisFlow m r) =>
  Id Driver ->
  PoolFieldUpdate m ->
  m ()
runPoolFieldUpdate driverId (PoolFieldUpdate dbWrite ltsUpdate) = do
  dbWrite
  syncDriverPoolDataToLTS driverId ltsUpdate
