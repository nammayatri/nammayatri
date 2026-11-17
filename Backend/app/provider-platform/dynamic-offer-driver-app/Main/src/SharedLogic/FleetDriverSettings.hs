-- | A fleet's auto-assign and preference settings, applied to one of its drivers in a single place: when a vehicle is
-- assigned to the driver, and when a driver who already has a vehicle becomes active in the fleet.
--
-- Only the tiers that driver's vehicle serves get auto-assign, and nothing is removed when the driver leaves the fleet.
module SharedLogic.FleetDriverSettings
  ( autoAssignTagName,
    vehicleAutoAcceptTiers,
    fleetAutoAssignTiers,
    autoAssignedTiers,
    applyFleetSettings,
    applyFleetSettingsToDriver,
    applyLockedFleetSettingsToDriver,
    removeAutoAssign,
    fleetLocksTiers,
    fleetLocksPreferences,
    FleetLock (..),
    getFleetLock,
  )
where

import qualified Data.Aeson as A
import qualified Data.List as DL
import qualified Data.Vector as V
import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FleetConfig as DFC
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (CounterpartyType (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.AreaPreference as AreaPref
import SharedLogic.Finance.WalletAccount (hasMinWalletBalance)
import qualified SharedLogic.VehicleServiceTier as SVST
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error

autoAssignTagName :: Text
autoAssignTagName = "AutoAssign"

-- | The city's auto-accept tiers this vehicle serves.
vehicleAutoAcceptTiers :: [DVST.VehicleServiceTier] -> DV.Vehicle -> [ServiceTierType]
vehicleAutoAcceptTiers cityTiers vehicle =
  [ tier.serviceTierType
    | tier <- cityTiers,
      maybe False (.enabled) tier.autoAcceptanceConfig,
      tier.serviceTierType `elem` vehicle.selectedServiceTiers
  ]

-- | Tiers on the driver's AutoAssign tag.
autoAssignedTiers :: [LYT.TagNameValueExpiry] -> [Text]
autoAssignedTiers tags = case Yudhishthira.accessTagKey (LYT.TagName autoAssignTagName) (Yudhishthira.convertTags tags) of
  Just (A.String value) -> [value]
  Just (A.Array values) -> [value | A.String value <- V.toList values]
  _ -> []

-- | Applies the driver's fleet settings to the driver and this vehicle, one after another on the same tags: auto-assign
-- on the vehicle's auto-accept tiers when the fleet has it on by default (or 'forceAutoAssign', the fleet's per-driver
-- switch), then area preference off and trip-distance preference off when the fleet ignores them. No-op for a driver
-- whose fleet has no auto-assign settings, unless forced.
applyFleetSettings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Bool ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m ()
applyFleetSettings forceAutoAssign cityTiers driver vehicle = do
  mbFleetAutoAssign <- QFDA.getDriverFleetAutoAssignConfig driver.id driver.merchantOperatingCityId
  when (forceAutoAssign || isJust mbFleetAutoAssign) $ do
    FleetTiers {autoAssignTiers = tiers, forceSelectedTiers} <- planFleetTiers forceAutoAssign mbFleetAutoAssign cityTiers driver vehicle
    let ignorePreferences = maybe False (.disableDriverAreaAndDistancePreference) mbFleetAutoAssign
        oldTags = fromMaybe [] driver.driverTag
        newTags =
          oldTags
            & addAutoAssign tiers
            & disablePreferenceIf ignorePreferences AreaPref.areaPreferenceEnabledTagName
            & disablePreferenceIf ignorePreferences AreaPref.distancePreferenceEnabledTagName
    when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
    let selectedTiers = DL.nub (vehicle.selectedServiceTiers <> forceSelectedTiers)
    when (selectedTiers /= vehicle.selectedServiceTiers) $ QVehicle.updateSelectedServiceTiers selectedTiers vehicle.driverId
    updateAutoAcceptTiers vehicle (DL.nub (fromMaybe [] vehicle.selectedAutoAcceptTiers <> tiers))
  where
    addAutoAssign [] tags = tags
    addAutoAssign tiers tags = setTag autoAssignTagName (LYT.ArrayValue (DL.nub (autoAssignedTiers tags <> map show tiers))) tags
    disablePreferenceIf ignore name tags =
      if ignore then setTag name (LYT.TextValue (AreaPref.mkEnabledTagValue False)) tags else tags

-- | The tiers 'applyFleetSettings' would put on auto-assign; empty means the fleet's per-driver switch has nothing to turn on.
fleetAutoAssignTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r) =>
  Bool ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m [ServiceTierType]
fleetAutoAssignTiers forceAutoAssign cityTiers driver vehicle = do
  mbFleetAutoAssign <- QFDA.getDriverFleetAutoAssignConfig driver.id driver.merchantOperatingCityId
  (.autoAssignTiers) <$> planFleetTiers forceAutoAssign mbFleetAutoAssign cityTiers driver vehicle

data FleetTiers = FleetTiers
  { autoAssignTiers :: [ServiceTierType],
    -- | Tiers added to the vehicle's selected tiers (locked fleets only).
    forceSelectedTiers :: [ServiceTierType]
  }

planFleetTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r) =>
  Bool ->
  Maybe DFC.AutoAssignConfig ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m FleetTiers
planFleetTiers forceAutoAssign mbFleetAutoAssign cityTiers driver vehicle = do
  let assignsTiers = forceAutoAssign || maybe False (.defaultOn) mbFleetAutoAssign
      fleetLocksAutoAssign = fleetLocksTiers mbFleetAutoAssign
      selectedTiers = vehicleAutoAcceptTiers cityTiers vehicle
  if not assignsTiers
    then pure FleetTiers {autoAssignTiers = [], forceSelectedTiers = []}
    else
      if not fleetLocksAutoAssign
        then pure FleetTiers {autoAssignTiers = selectedTiers, forceSelectedTiers = []}
        else do
          -- Locked fleet: every auto-accept tier the vehicle can serve, selected or not, minus usage-restricted tiers and
          -- tiers the wallet can't cover (the same min-wallet gate as postDriverUpdateServiceTiers).
          driverTiers <- SVST.fetchVehicleTierForDriverWithUsageRestriction SVST.AllowedVariants Nothing (Just vehicle) Nothing (Just cityTiers) driver.id driver.merchantOperatingCityId
          let eligibleTiers = [tier | (tier, isUsageRestricted) <- driverTiers, not isUsageRestricted, maybe False (.enabled) tier.autoAcceptanceConfig]
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = driver.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
          walletFailedTiers <-
            if fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck
              then filterM (fmap not . walletCovers) [tier | tier <- eligibleTiers, isJust (tier.autoAcceptanceConfig >>= (.minWalletBalance))]
              else pure []
          let fleetTiers = [tier.serviceTierType | tier <- eligibleTiers, tier.serviceTierType `notElem` map (.serviceTierType) walletFailedTiers]
          pure FleetTiers {autoAssignTiers = fleetTiers, forceSelectedTiers = fleetTiers}
  where
    walletCovers tier = hasMinWalletBalance DRIVER (tier.autoAcceptanceConfig >>= (.minWalletBalance)) driver.id.getId

-- | For a driver who becomes active in a fleet, or whose vehicle was just written: applies the fleet's settings to it.
applyFleetSettingsToDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsToDriver = applyFleetSettingsLocked \mbFleetAutoAssign driver ->
  -- Forced for a locked driver: with the fleet's default off, the per-driver switch put them on.
  Just <$> lockedOnAutoAssign mbFleetAutoAssign driver

-- | After AC is back on: re-applies the fleet's settings, forced, for a locked driver only (others keep their choices).
applyLockedFleetSettingsToDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyLockedFleetSettingsToDriver = applyFleetSettingsLocked \mbFleetAutoAssign driver ->
  bool Nothing (Just True) <$> lockedOnAutoAssign mbFleetAutoAssign driver

-- | The driver's fleet locks tiers ('fleetLocksTiers') and the driver is on auto-assign (has the AutoAssign tag).
lockedOnAutoAssign :: MonadFlow m => Maybe DFC.AutoAssignConfig -> Person -> m Bool
lockedOnAutoAssign mbFleetAutoAssign driver = do
  now <- getCurrentTime
  let onAutoAssign = not (null (autoAssignedTiers (Yudhishthira.filterExpiredTags' now (fromMaybe [] driver.driverTag))))
  pure (fleetLocksTiers mbFleetAutoAssign && onAutoAssign)

-- | 'applyFleetSettings' under the selected-tiers lock, on a fresh driver and vehicle read. 'decide': Nothing skips, Just
-- force applies. The lock isn't re-entrant: a caller that already holds it calls 'applyFleetSettings'.
applyFleetSettingsLocked ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  (Maybe DFC.AutoAssignConfig -> Person -> m (Maybe Bool)) ->
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsLocked decide driverId merchantOpCityId =
  Redis.withWaitAndLockRedisWithTimeout (SVST.selectedServiceTiersLockKey driverId) 5 10 50000 $
    QVehicle.findById driverId >>= mapM_ \vehicle -> do
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      mbFleetAutoAssign <- QFDA.getDriverFleetAutoAssignConfig driverId merchantOpCityId
      decide mbFleetAutoAssign driver >>= mapM_ \forceAutoAssign -> do
        cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
        applyFleetSettings forceAutoAssign cityTiers driver vehicle

-- | The fleet's per-driver switch turned off (caller holds the selected-tiers lock): the whole AutoAssign tag goes, and
-- auto-accept comes off these tiers and every tier on the tag, also one the wallet strip unselected or the city retired.
removeAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  [ServiceTierType] ->
  Person ->
  DV.Vehicle ->
  m ()
removeAutoAssign tiers driver vehicle = do
  let oldTags = fromMaybe [] driver.driverTag
      tagTiers = autoAssignedTiers oldTags
      newTags = Yudhishthira.removeTagName (Just oldTags) (LYT.TagNameValue autoAssignTagName)
      offTier tier = tier `elem` tiers || show tier `elem` tagTiers
  when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
  updateAutoAcceptTiers vehicle (filter (not . offTier) (fromMaybe [] vehicle.selectedAutoAcceptTiers))

-- | The fleet locks its auto-assign tiers: 'lockTiers', by default on when drivers can't toggle auto-assign.
fleetLocksTiers :: Maybe DFC.AutoAssignConfig -> Bool
fleetLocksTiers = maybe False \cfg -> fromMaybe (not cfg.allowDriverToToggle) cfg.lockTiers

-- | The app hides area/trip-distance preferences: when the fleet ignores them, or 'lockPreferences'.
fleetLocksPreferences :: Maybe DFC.AutoAssignConfig -> Bool
fleetLocksPreferences = maybe False \cfg -> cfg.disableDriverAreaAndDistancePreference || fromMaybe False cfg.lockPreferences

-- | What the driver's fleet locks. The driver only: ops, recomputes, the wallet strip and the fleet still can.
data FleetLock = FleetLock
  { -- | 'lockTiers', on auto-assign: the tiers the fleet gave auto-assign on; the driver can't turn off them or their auto-accept.
    fleetTiers :: [ServiceTierType],
    -- | 'lockAc', any driver of the fleet: can't turn AC off, unless rider AC complaints have restricted it.
    acLocked :: Bool,
    -- | 'lockIntraCity', any driver of the fleet: can't turn intracity rides off.
    intraCityLocked :: Bool
  }

-- | The lock from the driver's active fleet's auto-assign config (as read by the caller); Nothing if nothing is locked.
getFleetLock ::
  MonadFlow m =>
  Maybe DFC.AutoAssignConfig ->
  Person ->
  DI.DriverInformation ->
  DV.Vehicle ->
  [DVST.VehicleServiceTier] ->
  m (Maybe FleetLock)
getFleetLock mbFleetAutoAssign driver driverInfo vehicle cityTiers = do
  now <- getCurrentTime
  let tagTiers = autoAssignedTiers (Yudhishthira.filterExpiredTags' now (fromMaybe [] driver.driverTag))
      tiersLocked = fleetLocksTiers mbFleetAutoAssign && not (null tagTiers)
      fleetTiers = [tier | tiersLocked, tier <- fromMaybe [] vehicle.selectedAutoAcceptTiers, show tier `elem` tagTiers]
      acLocked = isLocked (.lockAc) && not acRestrictedByComplaints
      intraCityLocked = isLocked (.lockIntraCity)
  pure $
    if tiersLocked || acLocked || intraCityLocked
      then Just FleetLock {fleetTiers, acLocked, intraCityLocked}
      else Nothing
  where
    isLocked flag = maybe False (fromMaybe False . flag) mbFleetAutoAssign
    -- Same rule as checkIfACAllowedForDriver (score over every threshold of the vehicle's tiers, not the restriction type).
    -- An ops lift resets the score to 0, so the lock holds again.
    acThresholds = mapMaybe (.airConditionedThreshold) (filter (\tier -> vehicle.variant `elem` tier.allowedVehicleVariant) cityTiers)
    acRestrictedByComplaints = not (null acThresholds || any (fromMaybe 0 driverInfo.airConditionScore <=) acThresholds)

-- | Sets the tag in place (appended if absent), so an unchanged value leaves the list equal and skips the person write.
setTag :: Text -> LYT.TagValue -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
setTag name value tags = case break isName tags of
  (before, _ : after) -> before <> [newTag] <> filter (not . isName) after
  _ -> tags <> [newTag]
  where
    newTag = Yudhishthira.mkTagNameValueExpiryAt (LYT.TagName name) value Nothing
    isName tag = Yudhishthira.parseTagName tag == Just (LYT.TagName name)

updateAutoAcceptTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  DV.Vehicle ->
  [ServiceTierType] ->
  m ()
updateAutoAcceptTiers vehicle autoAcceptTiers =
  -- Only this column: the vehicle may be an older read than a concurrent service-tier update.
  when (autoAcceptTiers /= fromMaybe [] vehicle.selectedAutoAcceptTiers) $
    QVehicle.updateSelectedAutoAcceptTiers autoAcceptTiers vehicle.driverId
