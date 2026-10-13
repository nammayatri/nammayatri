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
-- A missing fleet_config row resolves to 'defaultFleetFeatures', i.e. today's
-- behaviour. Reads go through the generated KV-backed queries, so a write made by
-- 'upsertFleetConfig' is visible to the next read without any extra cache layer.
module SharedLogic.FleetFeatures
  ( -- * Resolved features
    FleetFeatures (..),
    resolveFleetFeatures,
    getFleetFeaturesByFleetOwnerId,
    getFleetFeaturesForDriver,

    -- * Pooling
    getFleetPoolingPrioritiesForDrivers,
    poolingPriorityCandidateKeys,
    resolveFleetPoolingPriorityForArea,

    -- * Writes
    FleetFeaturesUpdate (..),
    upsertFleetConfig,

    -- * Auto-assign
    DriverAutoAssignStatus (..),
    FleetAutoAssignResult (..),
    setFleetAutoAssign,
    getDriverAutoAssignStatus,

    -- * Fleet membership hooks
    applyFleetJoinDefaults,
    applyFleetAutoAssignOnVehicleCreation,
    removeFleetManagedState,

    -- * Fleet-wide sync on config change
    needsDriverSync,
    syncFleetFeaturesToDrivers,

    -- * Tags
    isReservedSystemTagName,
    clearDistancePreferenceOffTag,
  )
where

import Control.Applicative ((<|>))
import qualified Dashboard.Common as DC
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FleetConfig as DFC
import Domain.Types.FleetPoolingPriority (FleetPoolingPriority (..), PoolingPriorityLevel, dropKey, pickupKey, resolvePoolingPriority)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.DriverPool.AreaPreference (areaPreferenceCellTagPrefix, areaPreferenceEnabledTagName, distancePreferenceEnabledTagName, mkEnabledTagValue)
import SharedLogic.VehicleServiceTier (ServiceTierFilterMode (..), fetchVehicleTierForDriverWithUsageRestriction, hasMinWalletBalanceForTier, selectedServiceTiersLockKey)
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

type FleetFeaturesFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  )

-- ---------------------------------------------------------------------------
-- Resolved features
-- ---------------------------------------------------------------------------

data FleetFeatures = FleetFeatures
  { -- | The fleet's drivers have auto-assign turned on: applied to every current
    -- driver when the flag changes ('syncFleetFeaturesToDrivers') and to new joiners
    -- through the join / vehicle-creation hooks. A driver may still switch it off
    -- when 'allowDriverToToggleAutoAssign' is set.
    autoAssignDefaultOn :: Bool,
    -- | Whether the driver may change auto-assign from the app (owner/admin always can).
    allowDriverToToggleAutoAssign :: Bool,
    -- | Whether the driver may cancel an assigned ride.
    allowCancellationByDriver :: Bool,
    -- | Area and trip-distance preferences are ignored for the fleet's drivers (via the
    -- preference-off tags): applied to / removed from every current driver when the
    -- flag changes ('syncFleetFeaturesToDrivers') and applied to new joiners on join.
    ignoreDriverAreaAndDistancePreference :: Bool,
    -- | Pooling priority map (area key -> level) handed to the POOLING logic on each of
    -- the fleet's drivers (read at pool-build time, so a change needs no per-driver
    -- sync). Nothing = not set.
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

getFleetFeaturesByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m FleetFeatures
getFleetFeaturesByFleetOwnerId fleetOwnerId = resolveFleetFeatures <$> QFC.findByPrimaryKey fleetOwnerId

-- | Fleet owner of the driver's ACTIVE fleet association, if any.
getActiveFleetOwnerIdForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe Text)
getActiveFleetOwnerIdForDriver driverId = fmap (.fleetOwnerId) <$> QFDA.findByDriverId driverId True

-- | Active fleet owner id and its resolved features; Nothing when the driver is not in an active fleet.
getFleetFeaturesForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe (Text, FleetFeatures))
getFleetFeaturesForDriver driverId = do
  mbFleetOwnerId <- getActiveFleetOwnerIdForDriver driverId
  forM mbFleetOwnerId $ \fleetOwnerId -> (fleetOwnerId,) <$> getFleetFeaturesByFleetOwnerId (Id fleetOwnerId)

-- ---------------------------------------------------------------------------
-- Pooling
-- ---------------------------------------------------------------------------

-- | Pooling priority map of each driver's ACTIVE fleet, for one pool build. Fleet
-- membership is always resolved here with ONE batch read of the active fleet
-- associations (the pool's own fleetOwnerId can be stale or missing), then the fleets'
-- configs are read in ONE batch. Drivers without an active fleet, or whose fleet has no
-- priority set, are absent from the map. Never throws: a failed lookup is logged and
-- yields an empty map, so pooling proceeds exactly as if no fleet had a priority.
getFleetPoolingPrioritiesForDrivers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id DP.Person] ->
  m (Map.Map (Id DP.Person) FleetPoolingPriority)
getFleetPoolingPrioritiesForDrivers [] = pure Map.empty
getFleetPoolingPrioritiesForDrivers driverIds = do
  result <- withTryCatch "getFleetPoolingPrioritiesForDrivers" lookupPriorities
  case result of
    Left err -> do
      logError $ "getFleetPoolingPrioritiesForDrivers failed for " <> show (length driverIds) <> " drivers: " <> show err
      pure Map.empty
    Right priorities -> pure priorities
  where
    lookupPriorities = do
      associations <- QFDA.findAllByDriverIds driverIds
      let driverFleetOwners = Map.fromList [(fda.driverId, fda.fleetOwnerId) | fda <- associations]
          fleetOwnerIds = DL.nub (Map.elems driverFleetOwners)
      configs <- if null fleetOwnerIds then pure [] else QFC.findAllByFleetOwnerIds (Id <$> fleetOwnerIds)
      let priorityByFleetOwner = Map.fromList [(cfg.fleetOwnerId.getId, priority) | cfg <- configs, Just priority <- [cfg.fleetPoolingPriority]]
      pure $ Map.mapMaybe (`Map.lookup` priorityByFleetOwner) driverFleetOwners

-- | Pooling-priority keys that apply to a search with this Area, in tie-break order:
-- @Pickup_<pickupId>@, then @Drop_<dropId>@ (pickup id from Pickup / PickupDrop, drop id from
-- Drop / PickupDrop; gate ids are ignored). Empty for 'SL.Default'.
poolingPriorityCandidateKeys :: SL.Area -> [Text]
poolingPriorityCandidateKeys area =
  maybeToList (pickupKey <$> mbPickupId)
    <> maybeToList (dropKey <$> mbDropId)
  where
    mbPickupId = SL.pickupSpecialZoneIdFromArea area
    mbDropId = SL.dropSpecialZoneIdFromArea area

-- | Level and matched key of a fleet's pooling priority for a search with this area (see
-- 'resolvePoolingPriority': highest-level candidate key, else "default").
resolveFleetPoolingPriorityForArea :: Maybe SL.Area -> FleetPoolingPriority -> Maybe (PoolingPriorityLevel, Text)
resolveFleetPoolingPriorityForArea mbArea = resolvePoolingPriority (maybe [] poolingPriorityCandidateKeys mbArea)

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
-- when it does not exist yet. Returns the resolved features before and after the
-- write; both come from the same read under a per-fleet lock, so concurrent updates
-- of one fleet are serialised and each caller diffs exactly what it changed.
upsertFleetConfig ::
  FleetFeaturesFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  FleetFeaturesUpdate ->
  m (FleetFeatures, FleetFeatures)
upsertFleetConfig fleetOwnerId merchantId merchantOpCityId upd =
  Redis.withWaitAndLockRedis (fleetConfigUpdateLockKey fleetOwnerId) 10 50000 $ do
    mbExisting <- QFC.findByPrimaryKey fleetOwnerId
    updated <- writeFleetConfig fleetOwnerId merchantId merchantOpCityId upd mbExisting
    pure (resolveFleetFeatures mbExisting, resolveFleetFeatures (Just updated))

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
  m DFC.FleetConfig
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
      pure updated
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
      pure newConfig

-- ---------------------------------------------------------------------------
-- Tag names
-- ---------------------------------------------------------------------------

autoAssignTagName :: Text
autoAssignTagName = "AutoAssign"

-- | Tags owned by the system (fleet controls / preference switches); the driver
-- self-service tag API must not let a driver write these.
reservedSystemTagNames :: [Text]
reservedSystemTagNames = [autoAssignTagName, areaPreferenceEnabledTagName, distancePreferenceEnabledTagName]

-- | Case-insensitive, whitespace-trimmed. Also reserves every AreaPreference* tag
-- (cells, radius, enabled flag), which only the area-preference flow writes.
isReservedSystemTagName :: Text -> Bool
isReservedSystemTagName name =
  normalized `elem` map T.toLower reservedSystemTagNames
    || T.toLower areaPreferenceTagFamilyPrefix `T.isPrefixOf` normalized
  where
    normalized = T.toLower (T.strip name)
    -- "AreaPreference_<geohash>" minus the trailing underscore: covers
    -- AreaPreference_*, AreaPreferenceRadius and AreaPreferenceEnabled.
    areaPreferenceTagFamilyPrefix = fromMaybe areaPreferenceCellTagPrefix (T.stripSuffix "_" areaPreferenceCellTagPrefix)

-- | True unless an explicit AreaPreferenceEnabled#false tag is present.
isAreaPreferenceEnabledTagOn :: [LYT.TagNameValueExpiry] -> Bool
isAreaPreferenceEnabledTagOn tags =
  areaPreferenceEnabledTagName <> "#" <> mkEnabledTagValue False
    `notElem` map (LYT.getTagNameValue . Yudhishthira.convertToTagNameValue) tags

-- | Drops the DistancePreferenceEnabled switch (only fleet join defaults write it), so
-- the driver's trip-distance preference applies again.
clearDistancePreferenceOffTag :: [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
clearDistancePreferenceOffTag = removeTagsNamed distancePreferenceEnabledTagName

-- | ignoreDriverAreaAndDistancePreference on: never-expiring AreaPreferenceEnabled#false and
-- DistancePreferenceEnabled#false. Off: both tags removed.
applyIgnorePreferenceTags :: Bool -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
applyIgnorePreferenceTags True =
  setPermanentTag distancePreferenceEnabledTagName disabled
    . setPermanentTag areaPreferenceEnabledTagName disabled
  where
    disabled = LYT.TextValue (mkEnabledTagValue False)
applyIgnorePreferenceTags False =
  removeTagsNamed distancePreferenceEnabledTagName
    . removeTagsNamed areaPreferenceEnabledTagName

-- | Removes every tag carrying this name, regardless of value/expiry.
removeTagsNamed :: Text -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
removeTagsNamed name tags = Yudhishthira.removeTagName (Just tags) (LYT.TagNameValue name)

-- | Replaces any tag with this name by a single never-expiring name#value tag.
setPermanentTag :: Text -> LYT.TagValue -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
setPermanentTag name value tags =
  removeTagsNamed name tags <> [Yudhishthira.mkTagNameValueExpiryAt (LYT.TagName name) value Nothing]

-- | The "&"-separated values of the (non-expired) AutoAssign tag(s).
autoAssignTagValues :: UTCTime -> [LYT.TagNameValueExpiry] -> [Text]
autoAssignTagValues now tags =
  DL.nub
    [ value
      | tag <- Yudhishthira.filterExpiredTags' now tags,
        (tagName : tagValue : _) <- [T.splitOn "#" (LYT.getTagNameValue (Yudhishthira.convertToTagNameValue tag))],
        tagName == autoAssignTagName,
        value <- T.splitOn "&" tagValue,
        not (T.null value)
    ]

-- ---------------------------------------------------------------------------
-- Auto-assign
-- ---------------------------------------------------------------------------

isAutoAcceptEnabledTier :: DVST.VehicleServiceTier -> Bool
isAutoAcceptEnabledTier tier = maybe False (.enabled) tier.autoAcceptanceConfig

isAutoAcceptOnlyTier :: DVST.VehicleServiceTier -> Bool
isAutoAcceptOnlyTier tier = maybe False (\cfg -> cfg.enabled && cfg.mode == DC.AutoAcceptOnly) tier.autoAcceptanceConfig

-- | Tiers 'setFleetAutoAssign' True would enable, ignoring the AutoAssign tag: the
-- driver's own (cohort-filtered) tiers that are not usage-restricted, have
-- auto-acceptance enabled and pass the wallet gate exactly like the driver path.
computeFleetEligibleAutoAssignTiers ::
  FleetFeaturesFlow m r =>
  DP.Person ->
  DV.Vehicle ->
  m [DVST.VehicleServiceTier]
computeFleetEligibleAutoAssignTiers person vehicle = do
  let merchantOpCityId = person.merchantOperatingCityId
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  driverTiers <- fetchVehicleTierForDriverWithUsageRestriction AllowedVariants Nothing (Just vehicle) Nothing Nothing person.id merchantOpCityId
  let candidates = [tier | (tier, isUsageRestricted) <- driverTiers, not isUsageRestricted, isAutoAcceptEnabledTier tier]
      walletGateEnabled = fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck
  if walletGateEnabled
    then filterM (`hasMinWalletBalanceForTier` person.id) candidates
    else pure candidates

-- | Outcome of 'setFleetAutoAssign'.
data FleetAutoAssignResult
  = FleetAutoAssignApplied
  | -- | enable=True but the driver has no eligible auto-assign tier: nothing written.
    FleetAutoAssignNoEligibleTier
  deriving (Show, Eq)

-- | Fleet owner / admin auto-assign toggle for one driver.
--
-- enable=True: adds every eligible tier to selectedAutoAcceptTiers (union with what
-- is already there), forces the base tier of every eligible tier into
-- selectedServiceTiers (like the driver path does when auto-accept is requested, so
-- selectedAutoAcceptTiers always stays inside selectedServiceTiers), and merges the
-- eligible tier names into a never-expiring AutoAssign tag (owner override: re-adds
-- values a penalty may have removed).
--
-- enable=False: clears selectedAutoAcceptTiers and drops AutoAcceptOnly base tiers
-- from selectedServiceTiers (mirrors the driver path). The AutoAssign tag is kept --
-- turning auto-assign off is not revoking eligibility.
--
-- Throws VehicleNotFound when the driver has no vehicle.
setFleetAutoAssign :: FleetFeaturesFlow m r => Id DP.Person -> Bool -> m FleetAutoAssignResult
setFleetAutoAssign driverId enable =
  withDriverTiersLock driverId $ do
    vehicle <- QVehicle.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
    person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    if enable
      then enableAutoAssign person vehicle
      else disableAutoAssign person vehicle $> FleetAutoAssignApplied

-- | Runs the action under the driver's selectedServiceTiers lock, which also
-- serialises every fleet-feature write of the driver's tag list. Throws when the
-- lock could not be taken (the action did not run).
withDriverTiersLock :: FleetFeaturesFlow m r => Id DP.Person -> m a -> m a
withDriverTiersLock driverId action = do
  resultRef <- liftIO $ newIORef Nothing
  Redis.withWaitOnLockRedisWithExpiry (selectedServiceTiersLockKey driverId) 5 10 $
    action >>= liftIO . writeIORef resultRef . Just
  liftIO (readIORef resultRef)
    >>= fromMaybeM (InternalError $ "Could not acquire the selected service tiers lock for driver " <> driverId.getId)

-- | Locked read-modify-write of the driver's tag list (re-read inside the lock).
-- Returns False when the person does not exist.
modifyDriverTagLocked :: FleetFeaturesFlow m r => Id DP.Person -> ([LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]) -> m Bool
modifyDriverTagLocked driverId modify =
  withDriverTiersLock driverId $ do
    mbPerson <- QPerson.findById driverId
    case mbPerson of
      Nothing -> pure False
      Just person -> do
        QPerson.updateDriverTag (Just (modify (fromMaybe [] person.driverTag))) driverId
        pure True

enableAutoAssign :: FleetFeaturesFlow m r => DP.Person -> DV.Vehicle -> m FleetAutoAssignResult
enableAutoAssign person vehicle = do
  eligibleTiers <- computeFleetEligibleAutoAssignTiers person vehicle
  if null eligibleTiers
    then do
      logInfo $ "setFleetAutoAssign: no eligible auto-assign tier for driver " <> person.id.getId
      pure FleetAutoAssignNoEligibleTier
    else do
      now <- getCurrentTime
      let eligibleTierTypes = map (.serviceTierType) eligibleTiers
          existingTags = fromMaybe [] person.driverTag
          tagValues = DL.nub (autoAssignTagValues now existingTags <> map show eligibleTierTypes)
          newTags = setPermanentTag autoAssignTagName (LYT.ArrayValue tagValues) existingTags
          newAutoAcceptTiers = DL.nub (fromMaybe [] vehicle.selectedAutoAcceptTiers <> eligibleTierTypes)
          newSelectedTiers = DL.nub (vehicle.selectedServiceTiers <> eligibleTierTypes)
      QPerson.updateDriverTag (Just newTags) person.id
      QVehicle.updateSelectedServiceTiersAndAutoAcceptTiers newSelectedTiers newAutoAcceptTiers person.id
      pure FleetAutoAssignApplied

disableAutoAssign :: FleetFeaturesFlow m r => DP.Person -> DV.Vehicle -> m ()
disableAutoAssign person vehicle = do
  autoAcceptOnlyTierTypes <- getCityAutoAcceptOnlyTierTypes person.merchantOperatingCityId
  let newSelectedTiers = filter (`notElem` autoAcceptOnlyTierTypes) vehicle.selectedServiceTiers
  QVehicle.updateSelectedServiceTiersAndAutoAcceptTiers newSelectedTiers [] person.id

getCityAutoAcceptOnlyTierTypes :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [DTC.ServiceTierType]
getCityAutoAcceptOnlyTierTypes merchantOpCityId = do
  cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
  pure $ map (.serviceTierType) (filter isAutoAcceptOnlyTier cityTiers)

data DriverAutoAssignStatus = DriverAutoAssignStatus
  { -- | enabledTiers is non-empty.
    enabled :: Bool,
    -- | vehicle.selectedAutoAcceptTiers
    enabledTiers :: [DTC.ServiceTierType],
    -- | What 'setFleetAutoAssign' True would enable (ignores the AutoAssign tag).
    eligibleTiers :: [DTC.ServiceTierType],
    -- | Driver's tiers whose auto-acceptance mode is AutoAcceptOnly.
    autoAcceptOnlyTiers :: [DTC.ServiceTierType],
    hasAutoAssignTag :: Bool,
    -- | From the driver's active fleet; True when not in a fleet.
    allowDriverToToggle :: Bool
  }
  deriving (Generic, Show, Eq)

-- | Driver with no vehicle yet -> everything empty / False (not an error).
getDriverAutoAssignStatus :: FleetFeaturesFlow m r => Id DP.Person -> m DriverAutoAssignStatus
getDriverAutoAssignStatus driverId = do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbFleet <- getFleetFeaturesForDriver driverId
  now <- getCurrentTime
  let allowDriverToToggle = maybe True ((.allowDriverToToggleAutoAssign) . snd) mbFleet
      hasAutoAssignTag = not (null (autoAssignTagValues now (fromMaybe [] person.driverTag)))
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
      eligible <- computeFleetEligibleAutoAssignTiers person vehicle
      driverTiers <- fetchVehicleTierForDriverWithUsageRestriction AllowedVariants Nothing (Just vehicle) Nothing Nothing driverId person.merchantOperatingCityId
      let enabledTiers = fromMaybe [] vehicle.selectedAutoAcceptTiers
      pure
        DriverAutoAssignStatus
          { enabled = not (null enabledTiers),
            enabledTiers,
            eligibleTiers = map (.serviceTierType) eligible,
            autoAcceptOnlyTiers = [tier.serviceTierType | (tier, _) <- driverTiers, isAutoAcceptOnlyTier tier],
            hasAutoAssignTag,
            allowDriverToToggle
          }

-- ---------------------------------------------------------------------------
-- Fleet membership hooks (never throw)
-- ---------------------------------------------------------------------------

-- | Runs the action, logging (not rethrowing) any failure.
guarded :: (MonadFlow m) => Text -> m () -> m ()
guarded label action = do
  result <- withTryCatch label action
  case result of
    Left err -> logError $ label <> " failed: " <> show err
    Right () -> pure ()

-- | On a NEW fleet-driver association: auto-assign default (only when the driver
-- already has a vehicle) and the preference-off tags when ignoreDriverAreaAndDistancePreference.
applyFleetJoinDefaults :: FleetFeaturesFlow m r => Id DP.Person -> Id DP.Person -> m ()
applyFleetJoinDefaults driverId fleetOwnerId = guarded ("applyFleetJoinDefaults:" <> driverId.getId) $ do
  features <- getFleetFeaturesByFleetOwnerId fleetOwnerId
  when features.autoAssignDefaultOn $
    guarded ("applyFleetJoinDefaults:autoAssign:" <> driverId.getId) $ do
      mbVehicle <- QVehicle.findById driverId
      when (isJust mbVehicle) $ void $ setFleetAutoAssign driverId True
  when features.ignoreDriverAreaAndDistancePreference $
    guarded ("applyFleetJoinDefaults:ignorePreference:" <> driverId.getId) $ do
      personFound <- modifyDriverTagLocked driverId (applyIgnorePreferenceTags True)
      unless personFound $ throwError (PersonNotFound driverId.getId)

-- | On creation of a fleet driver's vehicle: turn auto-assign on when the driver's
-- active fleet has autoAssignDefaultOn. Never fails onboarding.
applyFleetAutoAssignOnVehicleCreation :: FleetFeaturesFlow m r => Id DP.Person -> m ()
applyFleetAutoAssignOnVehicleCreation driverId = guarded ("applyFleetAutoAssignOnVehicleCreation:" <> driverId.getId) $ do
  mbFleet <- getFleetFeaturesForDriver driverId
  whenJust mbFleet $ \(_, features) ->
    when features.autoAssignDefaultOn $ void $ setFleetAutoAssign driverId True

-- | On unlink from a fleet: drop the AutoAssign tag, clear selectedAutoAcceptTiers
-- and drop AutoAcceptOnly base tiers (they cannot exist without auto-accept).
--
-- Only the preference tags the fleet wrote are removed: DistancePreferenceEnabled is
-- written only by the fleet join defaults (the driver tag API reserves it), so it is
-- always dropped; AreaPreferenceEnabled#false is dropped only when the fleet being left
-- has ignoreDriverAreaAndDistancePreference, so a driver's own "area preference off" survives the unlink.
removeFleetManagedState :: FleetFeaturesFlow m r => Id DP.Person -> Id DP.Person -> m ()
removeFleetManagedState driverId fleetOwnerId = guarded ("removeFleetManagedState:" <> driverId.getId) $ do
  features <- getFleetFeaturesByFleetOwnerId fleetOwnerId
  withDriverTiersLock driverId $ do
    mbPerson <- QPerson.findById driverId
    whenJust mbPerson $ \person -> do
      let existingTags = fromMaybe [] person.driverTag
          fleetWroteAreaPreferenceOff = features.ignoreDriverAreaAndDistancePreference && not (isAreaPreferenceEnabledTagOn existingTags)
          tagNamesToRemove =
            [autoAssignTagName, distancePreferenceEnabledTagName]
              <> [areaPreferenceEnabledTagName | fleetWroteAreaPreferenceOff]
          newTags = foldr removeTagsNamed existingTags tagNamesToRemove
      when (length newTags /= length existingTags) $
        QPerson.updateDriverTag (Just newTags) driverId
      mbVehicle <- QVehicle.findById driverId
      whenJust mbVehicle $ disableAutoAssign person

-- ---------------------------------------------------------------------------
-- Fleet-wide sync on config change (never throws)
-- ---------------------------------------------------------------------------

data DriverSyncResult = DriverSyncSucceeded | DriverSyncSkipped | DriverSyncFailed
  deriving (Eq)

data DriverSyncCounts = DriverSyncCounts
  { processedCount :: Int,
    succeededCount :: Int,
    failedCount :: Int,
    skippedCount :: Int
  }

emptyDriverSyncCounts :: DriverSyncCounts
emptyDriverSyncCounts = DriverSyncCounts 0 0 0 0

addDriverSyncResult :: DriverSyncCounts -> DriverSyncResult -> DriverSyncCounts
addDriverSyncResult counts result =
  case result of
    DriverSyncSucceeded -> bumped {succeededCount = counts.succeededCount + 1}
    DriverSyncSkipped -> bumped {skippedCount = counts.skippedCount + 1}
    DriverSyncFailed -> bumped {failedCount = counts.failedCount + 1}
  where
    bumped = counts {processedCount = counts.processedCount + 1}

-- | One changed flag to push to every current driver.
data FlagSync m = FlagSync
  { flagName :: Text,
    -- | The flag's value in the given features; the sync stops once the stored
    -- config no longer has 'targetValue' (a newer change superseded it).
    readFlag :: FleetFeatures -> Bool,
    targetValue :: Bool,
    applyToDriver :: Id DP.Person -> m DriverSyncResult
  }

-- | Whether a config change touches a flag 'syncFleetFeaturesToDrivers' pushes to drivers
-- (autoAssignDefaultOn, ignoreDriverAreaAndDistancePreference). Callers check it before
-- forking the sync, so a lock-flag or pooling-priority-only change starts no sync.
needsDriverSync :: FleetFeatures -> FleetFeatures -> Bool
needsDriverSync oldFeatures newFeatures =
  oldFeatures.autoAssignDefaultOn /= newFeatures.autoAssignDefaultOn
    || oldFeatures.ignoreDriverAreaAndDistancePreference /= newFeatures.ignoreDriverAreaAndDistancePreference

-- | Pushes the flags that changed between the old and new fleet config to every
-- current ACTIVE driver of the fleet:
--
-- * autoAssignDefaultOn: 'setFleetAutoAssign' with the new value; drivers without a
--   vehicle (they get the default via 'applyFleetAutoAssignOnVehicleCreation') or
--   without an eligible auto-assign tier are counted as skipped.
-- * ignoreDriverAreaAndDistancePreference: adds / removes the AreaPreferenceEnabled#false and
--   DistancePreferenceEnabled#false tags.
--
-- The lock flags (allowDriverToToggleAutoAssign, allowCancellationByDriver) are read
-- at request time and fleetPoolingPriority at pool-build time, so they need no sync. Syncs of one fleet run one at a time (per-fleet
-- lock). Drivers are walked in keyset pages of driver ids and handled one by one; a
-- failure for one driver is logged and does not stop the batch. Before each driver the
-- stored config is re-read and a flag whose value changed again is dropped, so a
-- superseded sync stops before the newer one (waiting on the lock) starts. Meant to
-- run in a forked thread.
syncFleetFeaturesToDrivers :: FleetFeaturesFlow m r => Id DP.Person -> FleetFeatures -> FleetFeatures -> m ()
syncFleetFeaturesToDrivers fleetOwnerId oldFeatures newFeatures =
  guarded ("syncFleetFeaturesToDrivers:" <> fleetOwnerId.getId) $
    unless (null flagSyncs) $
      Redis.withWaitAndLockRedis (fleetFeaturesSyncLockKey fleetOwnerId) 1800 500000 $ do
        (active, superseded) <- go Nothing (map (,emptyDriverSyncCounts) flagSyncs, [])
        forM_ (active <> superseded) $ \(flagSync, c) ->
          logInfo $
            "syncFleetFeaturesToDrivers summary: fleetOwnerId: " <> fleetOwnerId.getId
              <> ", flag: "
              <> flagSync.flagName
              <> "="
              <> show flagSync.targetValue
              <> ", processed: "
              <> show c.processedCount
              <> ", succeeded: "
              <> show c.succeededCount
              <> ", failed: "
              <> show c.failedCount
              <> ", skipped: "
              <> show c.skippedCount
  where
    flagSyncs =
      [ FlagSync "autoAssignDefaultOn" (.autoAssignDefaultOn) newFeatures.autoAssignDefaultOn (syncDriverAutoAssign newFeatures.autoAssignDefaultOn)
        | oldFeatures.autoAssignDefaultOn /= newFeatures.autoAssignDefaultOn
      ]
        <> [ FlagSync "ignoreDriverAreaAndDistancePreference" (.ignoreDriverAreaAndDistancePreference) newFeatures.ignoreDriverAreaAndDistancePreference (syncDriverIgnorePreference newFeatures.ignoreDriverAreaAndDistancePreference)
             | oldFeatures.ignoreDriverAreaAndDistancePreference /= newFeatures.ignoreDriverAreaAndDistancePreference
           ]

    pageSize :: Int
    pageSize = 100

    go mbLastDriverId state = do
      page <- QFDA.findActiveDriverIdsByFleetOwnerIdAfter fleetOwnerId.getId mbLastDriverId pageSize
      state'@(active, _) <- foldM processDriver state page
      case listToMaybe (reverse page) of
        Just lastDriverId | length page >= pageSize && not (null active) -> go (Just lastDriverId.getId) state'
        _ -> pure state'

    processDriver (active, superseded) driverId
      | null active = pure (active, superseded)
      | otherwise = do
        current <- getFleetFeaturesByFleetOwnerId fleetOwnerId
        let (stillActive, nowSuperseded) = DL.partition (\(flagSync, _) -> flagSync.readFlag current == flagSync.targetValue) active
        forM_ nowSuperseded $ \(flagSync, _) ->
          logInfo $ "syncFleetFeaturesToDrivers: fleetOwnerId: " <> fleetOwnerId.getId <> ", flag: " <> flagSync.flagName <> " changed again, stopping before driverId " <> driverId.getId
        updated <- forM stillActive $ \(flagSync, c) -> do
          result <- runDriverSync flagSync driverId
          pure (flagSync, addDriverSyncResult c result)
        pure (updated, nowSuperseded <> superseded)

    runDriverSync flagSync driverId = do
      result <- withTryCatch ("syncFleetFeaturesToDrivers:" <> flagSync.flagName) (flagSync.applyToDriver driverId)
      case result of
        Right syncResult -> pure syncResult
        Left err -> do
          logError $
            "syncFleetFeaturesToDrivers: fleetOwnerId: " <> fleetOwnerId.getId
              <> ", flag: "
              <> flagSync.flagName
              <> ", driverId: "
              <> driverId.getId
              <> " failed: "
              <> show err
          pure DriverSyncFailed

fleetFeaturesSyncLockKey :: Id DP.Person -> Text
fleetFeaturesSyncLockKey fleetOwnerId = "FleetFeatures:Sync:" <> fleetOwnerId.getId

-- | Skips a driver without a vehicle (see 'applyFleetAutoAssignOnVehicleCreation') or
-- with no eligible auto-assign tier (nothing was written).
syncDriverAutoAssign :: FleetFeaturesFlow m r => Bool -> Id DP.Person -> m DriverSyncResult
syncDriverAutoAssign enable driverId = do
  mbVehicle <- QVehicle.findById driverId
  case mbVehicle of
    Nothing -> pure DriverSyncSkipped
    Just _ ->
      setFleetAutoAssign driverId enable <&> \case
        FleetAutoAssignApplied -> DriverSyncSucceeded
        FleetAutoAssignNoEligibleTier -> DriverSyncSkipped

syncDriverIgnorePreference :: FleetFeaturesFlow m r => Bool -> Id DP.Person -> m DriverSyncResult
syncDriverIgnorePreference enable driverId = do
  personFound <- modifyDriverTagLocked driverId (applyIgnorePreferenceTags enable)
  pure $ if personFound then DriverSyncSucceeded else DriverSyncSkipped
