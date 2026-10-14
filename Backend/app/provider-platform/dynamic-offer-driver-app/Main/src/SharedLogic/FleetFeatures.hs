{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fleet-owner driver controls stored on 'fleet_config': auto-assign defaults and
-- locks, driver ride-cancellation permission, the ignore-driver-area-and-distance-
-- preference switch and the fleet pooling priority read by the POOLING logic.
--
-- autoAssignDefaultOn and ignoreDriverAreaAndDistancePreference are written into driver
-- state (tags / vehicle tiers) by SharedLogic.FleetTags when a driver links to the fleet,
-- and removed when the driver leaves; a config change applies to drivers who link
-- afterwards. The other controls are read where the decision is made (driver APIs, ride
-- cancel, pool build).
--
-- A missing fleet_config row resolves to 'defaultFleetFeatures', i.e. today's
-- behaviour. Hot-path reads (driver APIs, ride cancel, allocator) come from the
-- in-memory Config Pilot copy of the city's fleet configs, which the dashboard config
-- update invalidates after its write. 'getFleetFeaturesFromDB' reads the table, for
-- the write paths that must see a write made moments earlier.
module SharedLogic.FleetFeatures
  ( -- * Resolved features
    FleetFeatures (..),
    getFleetFeatures,
    getFleetFeaturesForDriver,
    getFleetFeaturesFromDB,
    getFleetFeaturesByOwnerForCity,

    -- * Pooling
    fleetPoolingPrioritiesForDrivers,
    fleetPoolingPrioritiesForPool,
    resolveFleetPoolingPriorityForArea,

    -- * Writes
    FleetFeaturesUpdate (..),
    upsertFleetConfig,

    -- * Auto-assign
    DriverAutoAssignStatus (..),
    FleetAutoAssignResult (..),
    setFleetAutoAssign,
    getDriverAutoAssignStatus,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FleetConfig as DFC
import Domain.Types.FleetPoolingPriority (FleetPoolingPriority (..), PoolingPriorityLevel, resolvePoolingPriority)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.FleetTags as SFleetTags
import Storage.ConfigPilot.Config.FleetConfig (FleetConfigDimensions (..))
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

type FleetFeaturesFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  )

-- ---------------------------------------------------------------------------
-- Resolved features
-- ---------------------------------------------------------------------------

data FleetFeatures = FleetFeatures
  { -- | Drivers who link to the fleet get auto-assign on every auto-accept-enabled tier
    -- of the city (AutoAssign tag + selectedAutoAcceptTiers, see SharedLogic.FleetTags),
    -- and so does a fleet driver's new vehicle. A driver may still switch it off when
    -- 'allowDriverToToggleAutoAssign' is set.
    autoAssignDefaultOn :: Bool,
    -- | Whether the driver may change auto-assign from the app (owner/admin always can).
    allowDriverToToggleAutoAssign :: Bool,
    -- | Whether the driver may cancel an assigned ride.
    allowCancellationByDriver :: Bool,
    -- | Drivers who link to the fleet get the AreaPreferenceEnabled#false and
    -- DistancePreferenceEnabled#false tags, so dispatch ignores those preferences; the
    -- driver APIs refuse to turn them on / change them.
    ignoreDriverAreaAndDistancePreference :: Bool,
    -- | Pooling priority map (area key -> level) handed to the POOLING logic on each of
    -- the fleet's drivers (read at pool-build time). Nothing = not set.
    fleetPoolingPriority :: Maybe FleetPoolingPriority
  }
  deriving (Generic, Show, Eq)

defaultFleetFeatures :: FleetFeatures
defaultFleetFeatures =
  FleetFeatures
    { autoAssignDefaultOn = False,
      allowDriverToToggleAutoAssign = True,
      allowCancellationByDriver = True,
      ignoreDriverAreaAndDistancePreference = False,
      fleetPoolingPriority = Nothing
    }

resolveFleetFeatures :: Maybe DFC.FleetConfig -> FleetFeatures
resolveFleetFeatures = maybe defaultFleetFeatures fromConfig
  where
    fromConfig cfg =
      FleetFeatures
        { autoAssignDefaultOn = cfg.autoAssignDefaultOn,
          allowDriverToToggleAutoAssign = cfg.allowDriverToToggleAutoAssign,
          allowCancellationByDriver = cfg.allowCancellationByDriver,
          ignoreDriverAreaAndDistancePreference = cfg.ignoreDriverAreaAndDistancePreference,
          fleetPoolingPriority = cfg.fleetPoolingPriority
        }

-- | A fleet's resolved features from the in-memory Config Pilot copy of its city's fleet
-- configs (hot paths: driver cancel, ride lists, driver preference APIs). The dashboard
-- update invalidates that copy, so a saved change applies on the next read.
getFleetFeatures :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> m FleetFeatures
getFleetFeatures merchantOpCityId fleetOwnerId =
  resolveFleetFeatures <$> getOneConfig (FleetConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, fleetOwnerId = Just fleetOwnerId.getId}) Nothing

-- | Read straight from the table, for the write paths that must see a write made
-- moments earlier.
getFleetFeaturesFromDB :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m FleetFeatures
getFleetFeaturesFromDB fleetOwnerId = resolveFleetFeatures <$> QFC.findByPrimaryKey fleetOwnerId

-- | Resolved features of every fleet of the city that has a fleet_config row, keyed by
-- fleet owner id, from ONE in-memory Config Pilot read (allocator: one read per batch).
-- Fleets without a row are absent; look them up with 'lookupFleetFeatures'. Never throws:
-- a failed read is logged and yields an empty map, so the allocator proceeds exactly as
-- if no fleet had any control set.
getFleetFeaturesByOwnerForCity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m (Map.Map Text FleetFeatures)
getFleetFeaturesByOwnerForCity merchantOpCityId = do
  result <- withTryCatch "getFleetFeaturesByOwnerForCity" $ getConfig (FleetConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, fleetOwnerId = Nothing}) Nothing
  case result of
    Left err -> do
      logError $ "getFleetFeaturesByOwnerForCity failed for city " <> merchantOpCityId.getId <> ": " <> show err
      pure Map.empty
    Right configs -> pure $ Map.fromList [(cfg.fleetOwnerId.getId, resolveFleetFeatures (Just cfg)) | cfg <- configs]

-- | Features of the fleet a pool entry belongs to (@DriverPoolResult.fleetOwnerId@, set
-- only from an active, unexpired association). Nothing for a non-fleet driver or a fleet
-- without a config row, i.e. no gate applies.
lookupFleetFeatures :: Map.Map Text FleetFeatures -> Maybe Text -> Maybe FleetFeatures
lookupFleetFeatures featuresByOwner mbFleetOwnerId = mbFleetOwnerId >>= (`Map.lookup` featuresByOwner)

-- | Fleet owner of the driver's ACTIVE fleet association, if any.
getActiveFleetOwnerIdForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe Text)
getActiveFleetOwnerIdForDriver driverId = fmap (.fleetOwnerId) <$> QFDA.findByDriverId driverId True

-- | 'getFleetFeaturesForDriver' against the table, for the write paths.
getFleetFeaturesForDriverFromDB :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe (Text, FleetFeatures))
getFleetFeaturesForDriverFromDB driverId = do
  mbFleetOwnerId <- getActiveFleetOwnerIdForDriver driverId
  forM mbFleetOwnerId $ \fleetOwnerId -> (fleetOwnerId,) <$> getFleetFeaturesFromDB (Id fleetOwnerId)

-- | Active fleet owner id and its resolved features; Nothing when the driver is not in an active fleet.
getFleetFeaturesForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> m (Maybe (Text, FleetFeatures))
getFleetFeaturesForDriver merchantOpCityId driverId = do
  mbFleetOwnerId <- getActiveFleetOwnerIdForDriver driverId
  forM mbFleetOwnerId $ \fleetOwnerId -> (fleetOwnerId,) <$> getFleetFeatures merchantOpCityId (Id fleetOwnerId)

-- ---------------------------------------------------------------------------
-- Pooling
-- ---------------------------------------------------------------------------

-- | Pooling priority map of each driver's ACTIVE fleet, for one pool build, from the
-- city's fleet features ('getFleetFeaturesByOwnerForCity'). Drivers without an active
-- fleet, or whose fleet has no priority set, are absent from the map.
fleetPoolingPrioritiesForDrivers :: Map.Map Text FleetFeatures -> [(Id DP.Person, Maybe Text)] -> Map.Map (Id DP.Person) FleetPoolingPriority
fleetPoolingPrioritiesForDrivers featuresByOwner driverFleetOwners =
  Map.fromList
    [ (driverId, priority)
      | (driverId, mbFleetOwnerId) <- driverFleetOwners,
        Just features <- [lookupFleetFeatures featuresByOwner mbFleetOwnerId],
        Just priority <- [features.fleetPoolingPriority]
    ]

-- | 'fleetPoolingPrioritiesForDrivers' for one pool batch. Reads nothing unless some fleet of
-- the city has a pooling priority. Pool data carries @fleetOwnerId@ only where prepaid wallet
-- or online payment is on, so drivers without one get a single batched read of their active
-- fleet association. Never throws: a failed read leaves those drivers without a priority.
fleetPoolingPrioritiesForPool ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  [(Id DP.Person, Maybe Text)] ->
  m (Map.Map (Id DP.Person) FleetPoolingPriority)
fleetPoolingPrioritiesForPool merchantOpCityId driverFleetOwners = do
  featuresByOwner <- Map.filter (isJust . (.fleetPoolingPriority)) <$> getFleetFeaturesByOwnerForCity merchantOpCityId
  if Map.null featuresByOwner
    then pure Map.empty
    else do
      let withoutFleetOwner = [driverId | (driverId, Nothing) <- driverFleetOwners]
      assocs <-
        if null withoutFleetOwner
          then pure []
          else
            withTryCatch "fleetPoolingPrioritiesForPool" (QFDA.findAllByDriverIds withoutFleetOwner) >>= \case
              Left err -> do
                logError $ "fleetPoolingPrioritiesForPool: fleet association read failed: " <> show err
                pure []
              Right found -> pure found
      let fleetOwnerByDriver = Map.fromList [(assoc.driverId, assoc.fleetOwnerId) | assoc <- assocs]
      pure $
        fleetPoolingPrioritiesForDrivers
          featuresByOwner
          [(driverId, mbFleetOwnerId <|> Map.lookup driverId fleetOwnerByDriver) | (driverId, mbFleetOwnerId) <- driverFleetOwners]

-- | Level and matched key of a fleet's pooling priority for a search with this area (see
-- 'resolvePoolingPriority': highest-level pickup/drop entry, else the Default entry).
resolveFleetPoolingPriorityForArea :: Maybe SL.Area -> FleetPoolingPriority -> Maybe (PoolingPriorityLevel, Text)
resolveFleetPoolingPriorityForArea = resolvePoolingPriority

-- ---------------------------------------------------------------------------
-- Writes
-- ---------------------------------------------------------------------------

-- | Partial update: Nothing keeps the stored (or default) value. A present
-- 'fleetPoolingPriority' replaces the stored map as a whole; an empty map clears it
-- (stored as NULL). Keys are validated by the caller.
data FleetFeaturesUpdate = FleetFeaturesUpdate
  { autoAssignDefaultOn :: Maybe Bool,
    allowDriverToToggleAutoAssign :: Maybe Bool,
    allowCancellationByDriver :: Maybe Bool,
    ignoreDriverAreaAndDistancePreference :: Maybe Bool,
    fleetPoolingPriority :: Maybe FleetPoolingPriority
  }
  deriving (Generic, Show, Eq)

-- | Applies the partial update to the fleet owner's fleet_config row, creating the row
-- (with the generated fromTType defaults for the columns this update does not cover)
-- when it does not exist yet. The read-modify-write runs under a per-fleet lock, so
-- concurrent updates of one fleet are serialised and none is lost.
upsertFleetConfig ::
  FleetFeaturesFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  FleetFeaturesUpdate ->
  m ()
upsertFleetConfig fleetOwnerId merchantId merchantOpCityId upd =
  Redis.withWaitAndLockRedis (fleetConfigUpdateLockKey fleetOwnerId) 10 50000 $ do
    mbExisting <- QFC.findByPrimaryKey fleetOwnerId
    writeFleetConfig fleetOwnerId merchantId merchantOpCityId upd mbExisting

-- | An empty map means "no priority": stored as NULL rather than {}.
nonEmptyPoolingPriority :: FleetPoolingPriority -> Maybe FleetPoolingPriority
nonEmptyPoolingPriority priority
  | Map.null priority.getFleetPoolingPriority = Nothing
  | otherwise = Just priority

fleetConfigUpdateLockKey :: Id DP.Person -> Text
fleetConfigUpdateLockKey fleetOwnerId = "FleetFeatures:ConfigUpdate:" <> fleetOwnerId.getId

writeFleetConfig ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  FleetFeaturesUpdate ->
  Maybe DFC.FleetConfig ->
  m ()
writeFleetConfig fleetOwnerId merchantId merchantOpCityId upd mbExisting = do
  now <- getCurrentTime
  case mbExisting of
    Just existing -> do
      let updated =
            existing
              { DFC.autoAssignDefaultOn = fromMaybe existing.autoAssignDefaultOn upd.autoAssignDefaultOn,
                DFC.allowDriverToToggleAutoAssign = fromMaybe existing.allowDriverToToggleAutoAssign upd.allowDriverToToggleAutoAssign,
                DFC.allowCancellationByDriver = fromMaybe existing.allowCancellationByDriver upd.allowCancellationByDriver,
                DFC.ignoreDriverAreaAndDistancePreference = fromMaybe existing.ignoreDriverAreaAndDistancePreference upd.ignoreDriverAreaAndDistancePreference,
                DFC.fleetPoolingPriority = maybe existing.fleetPoolingPriority nonEmptyPoolingPriority upd.fleetPoolingPriority,
                DFC.merchantId = existing.merchantId <|> Just merchantId,
                DFC.merchantOperatingCityId = existing.merchantOperatingCityId <|> Just merchantOpCityId,
                DFC.updatedAt = now
              }
      QFC.updateByPrimaryKey updated
    Nothing -> do
      let defaults = defaultFleetFeatures
          newConfig =
            DFC.FleetConfig
              { allowAutomaticRoundTripAssignment = False,
                allowCancellationByDriver = fromMaybe defaults.allowCancellationByDriver upd.allowCancellationByDriver,
                allowDriverToToggleAutoAssign = fromMaybe defaults.allowDriverToToggleAutoAssign upd.allowDriverToToggleAutoAssign,
                allowEndingMidRoute = False,
                allowStartRideFromQR = False,
                autoAssignDefaultOn = fromMaybe defaults.autoAssignDefaultOn upd.autoAssignDefaultOn,
                blacklistCoinEvents = Nothing,
                directlyStartFirstTripAssignment = True,
                endRideDistanceThreshold = 100,
                fleetOwnerId,
                fleetPoolingPriority = upd.fleetPoolingPriority >>= nonEmptyPoolingPriority,
                ignoreDriverAreaAndDistancePreference = fromMaybe defaults.ignoreDriverAreaAndDistancePreference upd.ignoreDriverAreaAndDistancePreference,
                rideEndApproval = False,
                unlinkDriverAndVehicleOnTripTermination = True,
                merchantId = Just merchantId,
                merchantOperatingCityId = Just merchantOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QFC.create newConfig

-- ---------------------------------------------------------------------------
-- Auto-assign (per-driver owner / admin switch)
-- ---------------------------------------------------------------------------

-- | Outcome of 'setFleetAutoAssign'.
data FleetAutoAssignResult
  = FleetAutoAssignApplied
  | -- | enable=True but the city has no auto-accept-enabled tier: nothing written.
    FleetAutoAssignNoEligibleTier
  deriving (Show, Eq)

-- | Fleet owner / admin auto-assign switch for one driver, written exactly like the
-- fleet's autoAssignDefaultOn (SharedLogic.FleetTags).
--
-- enable=True: merges every auto-accept-enabled tier of the city into a never-expiring
-- AutoAssign tag (owner override: re-adds values a penalty may have removed), adds them to
-- selectedAutoAcceptTiers and adds the AutoAcceptOnly ones the vehicle serves to
-- selectedServiceTiers.
--
-- enable=False: removes the AutoAssign tag, clears selectedAutoAcceptTiers and drops
-- AutoAcceptOnly tiers from selectedServiceTiers (mirrors the driver path).
--
-- Throws VehicleNotFound when the driver has no vehicle.
setFleetAutoAssign :: FleetFeaturesFlow m r => Id DP.Person -> Bool -> m FleetAutoAssignResult
setFleetAutoAssign driverId enable =
  SFleetTags.withDriverTiersLock driverId $ do
    vehicle <- QVehicle.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
    person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    if enable
      then do
        cityTiers <- SFleetTags.getCityAutoAcceptTiers person.merchantOperatingCityId
        if null cityTiers
          then do
            logInfo $ "setFleetAutoAssign: no auto-accept-enabled tier in the city of driver " <> driverId.getId
            pure FleetAutoAssignNoEligibleTier
          else SFleetTags.writeAutoAssignOn cityTiers person vehicle $> FleetAutoAssignApplied
      else SFleetTags.writeAutoAssignOff person vehicle $> FleetAutoAssignApplied

data DriverAutoAssignStatus = DriverAutoAssignStatus
  { -- | The driver carries the AutoAssign tag and has auto-accept tiers selected.
    enabled :: Bool,
    -- | vehicle.selectedAutoAcceptTiers
    enabledTiers :: [DTC.ServiceTierType],
    -- | What 'setFleetAutoAssign' True would enable: the city's auto-accept-enabled tiers.
    eligibleTiers :: [DTC.ServiceTierType],
    -- | AutoAcceptOnly tiers the driver's vehicle serves.
    autoAcceptOnlyTiers :: [DTC.ServiceTierType],
    hasAutoAssignTag :: Bool,
    -- | From the driver's active fleet; True when not in a fleet.
    allowDriverToToggle :: Bool
  }
  deriving (Generic, Show, Eq)

-- | Driver with no vehicle yet -> tiers empty / False (not an error).
getDriverAutoAssignStatus :: FleetFeaturesFlow m r => Id DP.Person -> m DriverAutoAssignStatus
getDriverAutoAssignStatus driverId = do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbFleet <- getFleetFeaturesForDriverFromDB driverId
  now <- getCurrentTime
  let allowDriverToToggle = maybe True ((.allowDriverToToggleAutoAssign) . snd) mbFleet
      hasAutoAssignTag = not (null (SFleetTags.autoAssignTagValues now (fromMaybe [] person.driverTag)))
  mbVehicle <- QVehicle.findById driverId
  case mbVehicle of
    Nothing ->
      pure
        DriverAutoAssignStatus
          { enabled = False,
            enabledTiers = [],
            eligibleTiers = [],
            autoAcceptOnlyTiers = [],
            hasAutoAssignTag,
            allowDriverToToggle
          }
    Just vehicle -> do
      cityTiers <- SFleetTags.getCityAutoAcceptTiers person.merchantOperatingCityId
      let enabledTiers = fromMaybe [] vehicle.selectedAutoAcceptTiers
      pure
        DriverAutoAssignStatus
          { enabled = hasAutoAssignTag && not (null enabledTiers),
            enabledTiers,
            eligibleTiers = map (.serviceTierType) cityTiers,
            autoAcceptOnlyTiers = SFleetTags.servedAutoAcceptOnlyTiers now person.driverTag vehicle cityTiers,
            hasAutoAssignTag,
            allowDriverToToggle
          }
