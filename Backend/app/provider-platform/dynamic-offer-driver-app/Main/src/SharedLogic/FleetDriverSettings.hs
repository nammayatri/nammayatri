-- | A fleet's auto-assign and preference settings, applied to one of its drivers in a single place: when a vehicle is
-- assigned to the driver, and when a driver who already has a vehicle becomes active in the fleet.
--
-- Only the tiers that driver's vehicle serves get auto-assign. Nothing is removed when the driver leaves the fleet; what
-- an earlier fleet left is cleared when the driver joins the next one ('applyFleetSettingsOnJoin').
module SharedLogic.FleetDriverSettings
  ( autoAssignTagName,
    vehicleAutoAcceptTiers,
    fleetAutoAssignTiers,
    autoAssignedTiers,
    applyFleetSettings,
    applyFleetSettingsToDriver,
    applyFleetSettingsOnJoin,
    applyLockedFleetSettingsToDriver,
    applyLockedFleetSettingsOnWalletPosting,
    clearAutoAssign,
    clearAllAutoAssign,
    reconcileAutoAssignAfterMgSync,
    onCampaignMembershipCleared,
    getDriverFleetAutoAssignConfig,
    FleetLock (..),
    getFleetLock,
    getFleetLockWith,
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
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (CounterpartyType (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import qualified Lib.IncentiveJourney.Storage.Beam.BeamFlow as IJBeam
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.AreaPreference as AreaPref
import SharedLogic.Finance.WalletAccount (hasMinWalletBalance)
import qualified SharedLogic.IncentiveJourney.MgCampaign as MgCampaign
import qualified SharedLogic.VehicleServiceTier as SVST
import Storage.Beam.IncentiveJourney ()
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
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

-- | Auto-assign settings: any IJ campaign membership wins (incl. behaviour-only →
-- Nothing, suppressing fleet). Else active fleet. For controlFeatures members,
-- returns snapshot AutoAssign even outside the perk window so reconcile/apply can
-- install tags at opt-in; silent-assign still gates on perksActive at dispatch.
-- Lives in SharedLogic (not query Extra) to avoid PersonExtra ↔ FleetDriverAssociationExtra ↔ MgCampaign cycle.
getDriverFleetAutoAssignConfig ::
  (MonadFlow m, IJBeam.BeamFlow m r, EsqDBFlow m r, CacheFlow m r, EsqDBReplicaFlow m r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m (Maybe DFC.AutoAssignConfig)
getDriverFleetAutoAssignConfig driverId merchantOpCityId = do
  mbInfo <- QDI.findById driverId
  mgSource <- MgCampaign.resolveMgPerkSource merchantOpCityId mbInfo
  case mgSource of
    MgCampaign.CampaignMember {snapshot} -> pure $ MgCampaign.mgAutoAssignConfigFromSnapshot snapshot
    MgCampaign.UseNoCampaign ->
      QFDA.findByDriverId driverId True >>= maybe (pure Nothing) (\assoc -> QFDA.getFleetAutoAssignConfig assoc.fleetOwnerId merchantOpCityId)

-- | Applies the driver's fleet settings to the driver and this vehicle, one after another on the same tags: auto-assign
-- on the vehicle's auto-accept tiers when the fleet has it on by default (or 'forceAutoAssign', the fleet's per-driver
-- switch), then area preference off and trip-distance preference off when the fleet ignores them. No-op for a driver
-- whose fleet has no auto-assign settings, unless forced.
applyFleetSettings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Bool ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m ()
applyFleetSettings forceAutoAssign cityTiers driver vehicle = do
  mbInfo <- QDI.findById driver.id
  mgSource <- MgCampaign.resolveMgPerkSource driver.merchantOperatingCityId mbInfo
  mbFleetAutoAssign <- getDriverFleetAutoAssignConfig driver.id driver.merchantOperatingCityId
  -- Active IJ membership owns the perk slot: do not force-install fleet AA.
  let allowForceFleet = case mgSource of
        MgCampaign.UseNoCampaign -> True
        MgCampaign.CampaignMember {} -> False
      force = forceAutoAssign && allowForceFleet
  when (force || isJust mbFleetAutoAssign) $ do
    FleetTiers {autoAssignTiers = tiers, forceSelectedTiers} <- planFleetTiers force mbFleetAutoAssign cityTiers driver vehicle
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

-- | The tiers 'applyFleetSettings' would give auto-assign on for this driver and vehicle (with 'forceAutoAssign' for the
-- fleet's per-driver switch). Empty means the switch has nothing to turn on.
fleetAutoAssignTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r) =>
  Bool ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m [ServiceTierType]
fleetAutoAssignTiers forceAutoAssign cityTiers driver vehicle = do
  mbFleetAutoAssign <- getDriverFleetAutoAssignConfig driver.id driver.merchantOperatingCityId
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
      fleetLocksAutoAssign = maybe False (not . (.allowDriverToToggle)) mbFleetAutoAssign
      selectedTiers = vehicleAutoAcceptTiers cityTiers vehicle
  if not assignsTiers
    then pure FleetTiers {autoAssignTiers = [], forceSelectedTiers = []}
    else
      if not fleetLocksAutoAssign
        then pure FleetTiers {autoAssignTiers = selectedTiers, forceSelectedTiers = []}
        else do
          -- A fleet that doesn't let its drivers toggle gets every auto-accept tier the vehicle can serve, turned off or
          -- not, behind the same min-wallet gate as the driver's own tier update (postDriverUpdateServiceTiers): a tier the
          -- wallet can't cover isn't force-selected and gets no auto-accept. Already-selected tiers go through the same
          -- filter, so a selected tier that is usage-restricted gets no auto-assign either.
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

-- | For a driver whose vehicle was just written (RC activate/re-verify, add-vehicle): applies the current fleet's settings
-- to the vehicle they have, if any. A driver joining a fleet goes through 'applyFleetSettingsOnJoin' instead. Re-reads the driver and vehicle under the selected-tiers lock (see 'applyFleetSettingsLocked').
applyFleetSettingsToDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsToDriver = applyFleetSettingsLocked \mbFleetAutoAssign driver ->
  -- Forced for a driver on auto-assign in a fleet that doesn't let its drivers toggle it: with the fleet's default off
  -- that driver is on through the per-driver switch, and a rewritten vehicle (RC re-verify/activate) has no auto-accept
  -- tiers left to keep.
  Just <$> lockedOnAutoAssign mbFleetAutoAssign driver

-- | Strip the AutoAssign tag and its tiers' auto-accept ('clearAutoAssign') under the selected-tiers lock, so leftover
-- fleet/MG tiers cannot silent-assign.
clearAllAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  m ()
clearAllAutoAssign driverId =
  Redis.withWaitAndLockRedisWithTimeout (SVST.selectedServiceTiersLockKey driverId) 5 10 50000 $
    QPerson.findById driverId >>= mapM_ \driver -> void . clearAutoAssign driver =<< QVehicle.findById driverId

-- | After IJCampaign tag sync.
-- Recompute from *all* current memberships (resolveMgPerkSource /
-- getDriverFleetAutoAssignConfig), not the single campaign just synced. A
-- behaviour-only opt-in must not wipe AutoAssign that another controls membership
-- still supplies. rewrite = clear then apply effective config: campaign AA ->
-- reinstall; behaviour-only / controls without AA -> clear only (fleet suppressed
-- until unassign). Invite-only keeps driver-owned selections. Silent-assign at
-- dispatch still requires perksActive via 'mgAutoAssignConfig'.
reconcileAutoAssignAfterMgSync ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id IJCommon.Person ->
  Maybe Mg.MgCampaignSnapshot ->
  m ()
reconcileAutoAssignAfterMgSync personId mbSnap = do
  let driverId = cast personId
  case mbSnap of
    Just snap
      | snap.optedIn -> rewriteAutoAssignFromCurrentConfig driverId
      | otherwise -> pure () -- invite only: keep driver-owned selections
    Nothing -> restoreFleetAutoAssignOrKeep driverId

-- | After one CJM membership is removed from tags: strip leftover AA, then re-apply
-- fleet (or remaining campaign) settings so fleet perks return after unassign.
onCampaignMembershipCleared ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id DCJM.CohortJourneyMapping ->
  m ()
onCampaignMembershipCleared driverId _cjmId = do
  clearAllAutoAssign driverId
  QPerson.findById driverId >>= mapM_ \driver -> applyFleetSettingsToDriver driver.id driver.merchantOperatingCityId

-- | clear + apply current MG/fleet AA from getDriverFleetAutoAssignConfig.
rewriteAutoAssignFromCurrentConfig ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  m ()
rewriteAutoAssignFromCurrentConfig driverId = do
  clearAllAutoAssign driverId
  QPerson.findById driverId >>= mapM_ \driver -> applyFleetSettingsToDriver driver.id driver.merchantOperatingCityId

-- | After all IJCampaign tags cleared: restore fleet AA if any; never wipe independent
-- driver self-selections when there is no fleet AutoAssign to reinstall.
restoreFleetAutoAssignOrKeep ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  m ()
restoreFleetAutoAssignOrKeep driverId =
  QPerson.findById driverId >>= mapM_ \driver -> do
    mbFleetAA <-
      QFDA.findByDriverId driverId True
        >>= maybe (pure Nothing) (\assoc -> QFDA.getFleetAutoAssignConfig assoc.fleetOwnerId driver.merchantOperatingCityId)
    case mbFleetAA of
      Just _ -> rewriteAutoAssignFromCurrentConfig driverId
      Nothing -> pure ()

-- | 'applyFleetSettingsToDriver', only for a driver on auto-assign in a fleet that doesn't let its drivers toggle it. For
-- use after an event that makes more tiers eligible (AC back on, a wallet posting): a fleet that allows toggling keeps the driver's choices,
-- and a driver the fleet switched off stays off. Forced, like the fleet's per-driver switch that put the driver on
-- auto-assign: with the fleet's default off, an unforced apply would do nothing.
applyLockedFleetSettingsToDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyLockedFleetSettingsToDriver = applyFleetSettingsLocked \mbFleetAutoAssign driver ->
  bool Nothing (Just True) <$> lockedOnAutoAssign mbFleetAutoAssign driver

-- | 'applyLockedFleetSettingsToDriver' after a posting to the driver's wallet: a fleet tier the wallet covers again (one a
-- debit stripped, or one the wallet was short for when the fleet's settings were applied) comes back with its auto-accept.
-- Every posting, not only a credit: a transfer_ credit collects no entry id.
-- A driver without an AutoAssign tag (outside a fleet, or switched off) stops at the person read: no lock, no fleet read.
-- With nothing to restore it writes nothing ('applyFleetSettings' writes only a changed column).
applyLockedFleetSettingsOnWalletPosting ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyLockedFleetSettingsOnWalletPosting driverId merchantOpCityId = do
  now <- getCurrentTime
  mbDriver <- QPerson.findById driverId
  let onAutoAssign driver = not (null (autoAssignedTiers (Yudhishthira.filterExpiredTags' now (fromMaybe [] driver.driverTag))))
  when (maybe False onAutoAssign mbDriver) $
    applyLockedFleetSettingsToDriver driverId merchantOpCityId

-- | The driver's fleet doesn't let its drivers toggle auto-assign, and the driver is on it (has the AutoAssign tag).
lockedOnAutoAssign :: MonadFlow m => Maybe DFC.AutoAssignConfig -> Person -> m Bool
lockedOnAutoAssign mbFleetAutoAssign driver = do
  now <- getCurrentTime
  let onAutoAssign = not (null (autoAssignedTiers (Yudhishthira.filterExpiredTags' now (fromMaybe [] driver.driverTag))))
  pure (maybe False (not . (.allowDriverToToggle)) mbFleetAutoAssign && onAutoAssign)

-- | Runs 'applyFleetSettings' under the driver's selected-tiers lock (the one 'postDriverUpdateServiceTiers', the wallet
-- strip and the recomputes take), on a fresh read of the driver and vehicle, so a forked caller's older vehicle can't undo
-- a concurrent tier change. 'decide' sees the fleet's auto-assign config and the driver: Nothing skips, Just force applies.
-- Not for a caller that already holds the lock (the lock isn't re-entrant): call 'applyFleetSettings' there.
applyFleetSettingsLocked ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  (Maybe DFC.AutoAssignConfig -> Person -> m (Maybe Bool)) ->
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsLocked decide driverId merchantOpCityId =
  Redis.withWaitAndLockRedisWithTimeout (SVST.selectedServiceTiersLockKey driverId) 5 10 50000 $
    QVehicle.findById driverId >>= mapM_ \vehicle -> do
      driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      mbFleetAutoAssign <- getDriverFleetAutoAssignConfig driverId merchantOpCityId
      decide mbFleetAutoAssign driver >>= mapM_ \forceAutoAssign -> do
        cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
        applyFleetSettings forceAutoAssign cityTiers driver vehicle

-- | For a driver who joins a fleet or moves to another one. Whatever auto-assign an earlier fleet left (nothing is
-- removed on leave) is cleared first ('clearAutoAssign'), so the new fleet's default, and later its per-driver switch,
-- decides from scratch; then the new fleet's settings are applied, unforced. Under the selected-tiers lock, on a fresh
-- read. Not for a vehicle rewrite in the same fleet, which keeps the driver's auto-assign ('applyFleetSettingsToDriver').
applyFleetSettingsOnJoin ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsOnJoin driverId merchantOpCityId =
  Redis.withWaitAndLockRedisWithTimeout (SVST.selectedServiceTiersLockKey driverId) 5 10 50000 $ do
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    (driver', mbVehicle) <- clearAutoAssign driver =<< QVehicle.findById driverId
    whenJust mbVehicle \vehicle -> do
      cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
      applyFleetSettings False cityTiers driver' vehicle

-- | Takes the driver off fleet auto-assign: the whole AutoAssign tag, whatever it holds (also tiers the city no longer
-- has), and those tiers' auto-accept on the vehicle. Selected service tiers are left as they are. Returns the driver and
-- vehicle as written. The caller holds the selected-tiers lock.
clearAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Person ->
  Maybe DV.Vehicle ->
  m (Person, Maybe DV.Vehicle)
clearAutoAssign driver mbVehicle = do
  let oldTags = fromMaybe [] driver.driverTag
      tagTiers = autoAssignedTiers oldTags
      newTags = Yudhishthira.removeTagName (Just oldTags) (LYT.TagNameValue autoAssignTagName)
  when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
  mbVehicle' <- forM mbVehicle \vehicle -> do
    let autoAcceptTiers = filter ((`notElem` tagTiers) . show) (fromMaybe [] vehicle.selectedAutoAcceptTiers)
    updateAutoAcceptTiers vehicle autoAcceptTiers
    pure vehicle {DV.selectedAutoAcceptTiers = Just autoAcceptTiers}
  pure (driver {DP.driverTag = Just newTags}, mbVehicle')

-- | What a fleet that doesn't let its drivers toggle auto-assign locks for one of its drivers. It holds the driver only:
-- ops edits, the rating/AC recomputes, the wallet strip and the fleet's own switch still change their tiers.
data FleetLock = FleetLock
  { -- | Tiers the fleet gave auto-assign on: the driver can't turn off the tier or its auto-accept.
    fleetTiers :: [ServiceTierType],
    -- | The driver can't turn AC off, unless rider AC complaints have restricted it.
    acLocked :: Bool,
    -- | The driver can't turn intracity rides off.
    intraCityLocked :: Bool
  }

-- | The fleet lock on this driver, if their fleet doesn't let drivers toggle auto-assign and the driver is on it (has the
-- AutoAssign tag): a driver the fleet switched off with its per-driver switch isn't locked.
getFleetLock ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, IJBeam.BeamFlow m r, EsqDBReplicaFlow m r) =>
  Id MerchantOperatingCity ->
  Person ->
  DI.DriverInformation ->
  DV.Vehicle ->
  [DVST.VehicleServiceTier] ->
  m (Maybe FleetLock)
getFleetLock merchantOpCityId driver driverInfo vehicle cityTiers = do
  mbFleetAutoAssign <- getDriverFleetAutoAssignConfig driver.id merchantOpCityId
  getFleetLockWith mbFleetAutoAssign driver driverInfo vehicle cityTiers

-- | 'getFleetLock' with the fleet's auto-assign config already read.
getFleetLockWith ::
  MonadFlow m =>
  Maybe DFC.AutoAssignConfig ->
  Person ->
  DI.DriverInformation ->
  DV.Vehicle ->
  [DVST.VehicleServiceTier] ->
  m (Maybe FleetLock)
getFleetLockWith mbFleetAutoAssign driver driverInfo vehicle cityTiers = do
  now <- getCurrentTime
  let tagTiers = autoAssignedTiers (Yudhishthira.filterExpiredTags' now (fromMaybe [] driver.driverTag))
      fleetTiers = [tier | tier <- fromMaybe [] vehicle.selectedAutoAcceptTiers, show tier `elem` tagTiers]
  pure $
    if maybe True (.allowDriverToToggle) mbFleetAutoAssign || null tagTiers
      then Nothing
      else Just FleetLock {fleetTiers, acLocked = not acRestrictedByComplaints, intraCityLocked = True}
  where
    -- The same rule as SharedLogic.DriverOnboarding.checkIfACAllowedForDriver, which the GET and the AC tier restriction
    -- use: the score over every threshold of the vehicle's tiers. Not the restriction type, which is set only past the
    -- city-wide maximum. An ops lift resets the score to 0, so the lock holds again.
    acThresholds = mapMaybe (.airConditionedThreshold) (filter (\tier -> vehicle.variant `elem` tier.allowedVehicleVariant) cityTiers)
    acRestrictedByComplaints = not (null acThresholds || any (fromMaybe 0 driverInfo.airConditionScore <=) acThresholds)

-- | Sets the tag in place (where the first tag of that name sits, other copies dropped), appended if absent: a value that
-- didn't change leaves the list equal, so the caller's diff skips the person write (other writers append theirs at the end).
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
