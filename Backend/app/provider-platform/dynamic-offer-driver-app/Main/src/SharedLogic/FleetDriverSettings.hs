-- | A fleet's auto-assign and preference settings, applied to one of its drivers in a single place: when a vehicle is
-- assigned to the driver, and when a driver who already has a vehicle becomes active in the fleet.
--
-- Only the tiers that driver's vehicle serves get auto-assign, and nothing is removed when the driver leaves the fleet.
module SharedLogic.FleetDriverSettings
  ( autoAssignTagName,
    vehicleAutoAcceptTiers,
    autoAssignedTiers,
    applyFleetSettings,
    applyFleetSettingsToDriver,
    removeAutoAssign,
    clearAllAutoAssign,
    reconcileAutoAssignAfterMgSync,
    onCampaignMembershipCleared,
    getDriverFleetAutoAssignConfig,
  )
where

import qualified Data.Aeson as A
import qualified Data.List as DL
import qualified Data.Vector as V
import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.FleetConfig as DFC
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import qualified Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.AreaPreference as AreaPref
import qualified SharedLogic.IncentiveJourney.MgCampaign as MgCampaign
import Storage.Beam.IncentiveJourney ()
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
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
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r, EsqDBReplicaFlow m r) =>
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
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, EsqDBReplicaFlow m r) =>
  Bool ->
  [DVST.VehicleServiceTier] ->
  Person ->
  DV.Vehicle ->
  m ()
applyFleetSettings forceAutoAssign cityTiers driver vehicle = do
  mbInfo <- QDI.findById driver.id
  mgSource <- MgCampaign.resolveMgPerkSource driver.merchantOperatingCityId mbInfo
  mbFleetAutoAssign <- getDriverFleetAutoAssignConfig driver.id driver.merchantOperatingCityId
  -- Active IJ membership (incl. behaviour-only) owns the perk slot: do not force-install fleet AA.
  let allowForceFleet = case mgSource of
        MgCampaign.UseNoCampaign -> True
        MgCampaign.CampaignMember {} -> False
  when ((forceAutoAssign && allowForceFleet) || isJust mbFleetAutoAssign) $ do
    -- Gate force the same way as the when-guard: campaign membership owns AA;
    -- force must not install tiers when campaign defaultOn is False.
    let tiers = if (forceAutoAssign && allowForceFleet) || maybe False (.defaultOn) mbFleetAutoAssign then vehicleAutoAcceptTiers cityTiers vehicle else []
        ignorePreferences = maybe False (.disableDriverAreaAndDistancePreference) mbFleetAutoAssign
        oldTags = fromMaybe [] driver.driverTag
        newTags =
          oldTags
            & addAutoAssign tiers
            & disablePreferenceIf ignorePreferences AreaPref.areaPreferenceEnabledTagName
            & disablePreferenceIf ignorePreferences AreaPref.distancePreferenceEnabledTagName
    when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
    updateAutoAcceptTiers vehicle (DL.nub (fromMaybe [] vehicle.selectedAutoAcceptTiers <> tiers))
  where
    addAutoAssign [] tags = tags
    addAutoAssign tiers tags = setTag autoAssignTagName (LYT.ArrayValue (DL.nub (autoAssignedTiers tags <> map show tiers))) tags
    disablePreferenceIf ignore name tags =
      if ignore then setTag name (LYT.TextValue (AreaPref.mkEnabledTagValue False)) tags else tags

-- | For a driver who becomes active in a fleet: applies the fleet's settings to the vehicle they already have, if any.
applyFleetSettingsToDriver ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, EsqDBReplicaFlow m r) =>
  Id Person ->
  Id MerchantOperatingCity ->
  m ()
applyFleetSettingsToDriver driverId merchantOpCityId =
  QVehicle.findById driverId >>= mapM_ \vehicle -> do
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    cityTiers <- CQVST.findAllByMerchantOpCityId merchantOpCityId Nothing
    applyFleetSettings False cityTiers driver vehicle

-- | Strip AutoAssign tag + selectedAutoAcceptTiers so leftover fleet/MG tiers cannot silent-assign.
clearAllAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id Person ->
  m ()
clearAllAutoAssign driverId = do
  mbVehicle <- QVehicle.findById driverId
  mbDriver <- QPerson.findById driverId
  case (mbVehicle, mbDriver) of
    (Just vehicle, Just driver) -> removeAutoAssign (fromMaybe [] vehicle.selectedAutoAcceptTiers) driver vehicle
    _ -> pure ()

-- | After IJCampaign tag sync.
-- Recompute from *all* current memberships (resolveMgPerkSource /
-- getDriverFleetAutoAssignConfig), not the single campaign just synced. A
-- behaviour-only opt-in must not wipe AutoAssign that another controls membership
-- still supplies. rewrite = clear then apply effective config: campaign AA ->
-- reinstall; behaviour-only / controls without AA -> clear only (fleet suppressed
-- until unassign). Invite-only keeps driver-owned selections. Silent-assign at
-- dispatch still requires perksActive via 'mgAutoAssignConfig'.
reconcileAutoAssignAfterMgSync ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, EsqDBReplicaFlow m r) =>
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
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, EsqDBReplicaFlow m r) =>
  Id Person ->
  Id DCJM.CohortJourneyMapping ->
  m ()
onCampaignMembershipCleared driverId _cjmId = do
  clearAllAutoAssign driverId
  QPerson.findById driverId >>= mapM_ \driver -> applyFleetSettingsToDriver driver.id driver.merchantOperatingCityId

-- | clear + apply current MG/fleet AA from getDriverFleetAutoAssignConfig.
rewriteAutoAssignFromCurrentConfig ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, EsqDBReplicaFlow m r) =>
  Id Person ->
  m ()
rewriteAutoAssignFromCurrentConfig driverId = do
  clearAllAutoAssign driverId
  QPerson.findById driverId >>= mapM_ \driver -> applyFleetSettingsToDriver driver.id driver.merchantOperatingCityId

-- | After all IJCampaign tags cleared: restore fleet AA if any; never wipe independent
-- driver self-selections when there is no fleet AutoAssign to reinstall.
restoreFleetAutoAssignOrKeep ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, EsqDBReplicaFlow m r) =>
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

-- | The fleet's per-driver switch turned off: these tiers come off the AutoAssign tag and the vehicle's auto-accept tiers.
removeAutoAssign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  [ServiceTierType] ->
  Person ->
  DV.Vehicle ->
  m ()
removeAutoAssign tiers driver vehicle = do
  let oldTags = fromMaybe [] driver.driverTag
      remaining = filter (`notElem` map show tiers) (autoAssignedTiers oldTags)
      newTags =
        if null remaining
          then Yudhishthira.removeTagName (Just oldTags) (LYT.TagNameValue autoAssignTagName)
          else setTag autoAssignTagName (LYT.ArrayValue remaining) oldTags
  when (newTags /= oldTags) $ QPerson.updateDriverTag (Just newTags) driver.id
  updateAutoAcceptTiers vehicle (filter (`notElem` tiers) (fromMaybe [] vehicle.selectedAutoAcceptTiers))

setTag :: Text -> LYT.TagValue -> [LYT.TagNameValueExpiry] -> [LYT.TagNameValueExpiry]
setTag name value tags =
  Yudhishthira.removeTagName (Just tags) (LYT.TagNameValue name) <> [Yudhishthira.mkTagNameValueExpiryAt (LYT.TagName name) value Nothing]

updateAutoAcceptTiers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  DV.Vehicle ->
  [ServiceTierType] ->
  m ()
updateAutoAcceptTiers vehicle autoAcceptTiers =
  -- Only this column: the vehicle may be an older read than a concurrent service-tier update.
  when (autoAcceptTiers /= fromMaybe [] vehicle.selectedAutoAcceptTiers) $
    QVehicle.updateSelectedAutoAcceptTiers autoAcceptTiers vehicle.driverId
