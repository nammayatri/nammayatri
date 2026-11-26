{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Who controls this driver: the one per-request lookup behind fleet auto-assign, the preference policy and "is the
-- driver in a fleet".
--
-- One pass resolves both sources that can be in charge of a driver's booking preferences:
--
--   * the incentive-journey campaign (MG): 'MgCampaign.resolveMgPerkSource' on the driver's IJCampaign tags. An
--     unexpired tag occupies the perk slot, including a behaviour-only campaign (no controlFeatures);
--   * the fleet: the driver's active fleet association (newest wins) and that fleet's config.
--
-- and exposes, for the source in charge ('ControlSource'), the effective auto-assign config and preference policy.
-- Handlers and triggers branch only on these fields ("policy present?", "source is a campaign?"); they never detect
-- campaigns themselves. The policy layer ('Domain.Types.DriverPreferencePolicy', the validators in
-- 'SharedLogic.DriverPreferencePolicy') takes a 'DDPP.PreferencePolicy' and never knows where it came from.
--
-- It lives in its own module, below 'SharedLogic.FleetDriverSettings' (whose 'getDriverFleetAutoAssignConfig' is a thin
-- wrapper over 'tryDriverFleetContext') and 'SharedLogic.DriverPreferencePolicy', so neither imports the other
-- through it.
--
-- How to add a new source (e.g. a city policy):
--
--   1. Add a constructor to 'ControlSource' and decide its precedence in 'resolveDriverFleetContextWith'.
--   2. Give it its policy in 'sourcePolicy' and its auto-assign config in 'sourceAutoAssign'. A policy kept as JSON
--      elsewhere is decoded with the normal JSON decode ('Kernel.Utils.JSON.valueToMaybe': undecodable = no policy).
--   3. If the source can be saved with a policy, check it on save with 'SharedLogic.DriverPreferencePolicy.validatePolicyConfig'
--      (400 INVALID_PREFERENCE_POLICY).
--   4. If its policy should be APPLIED (v2), call 'SharedLogic.DriverPreferencePolicy.Apply.applyCurrentPolicyToDriver'
--      where the source starts or changes for a driver. The fleet-only triggers (fleet policy job, join / vehicle,
--      fleet auto-assign ON) skip drivers a campaign is in charge of; the wallet restore follows 'preferencePolicy'.
--   5. To apply it to all its drivers at once (a save, or a time window opening), add a 'SharedLogic.Allocator.PolicySource'
--      constructor and its 'PolicySourceOps' case in 'SharedLogic.Allocator.Jobs.PreferencePolicy.ApplyPreferencePolicy'
--      (fresh policy read, members page, still-in-charge check through this resolver, optional next window), and queue
--      the ApplyPreferencePolicy job where the policy is saved. The paging, retry, apply and logging are shared.
--   Every validator and GET flag already follows 'preferencePolicy' / 'controlSource'.
--
-- The campaign source (v3) is that recipe: 'CampaignSource' wins over the fleet, its policy is the campaign's
-- PreferencePolicy perk ('sourcePolicy'), it is validated on campaign create / update, applied on the campaign's
-- membership sync (assign / opt-in / list ensure) and after a membership is cleared (the fleet's policy returns), and,
-- for a TimeBound perk, re-applied to its members a little after each window opens (job source CampaignPolicySource,
-- queued on campaign create / update / roll: 'SharedLogic.DriverPreferencePolicy.Campaign').
module SharedLogic.DriverPreferencePolicy.Context
  ( DriverFleetContext (..),
    FleetLink (..),
    ControlSource (..),
    FleetConfigRead (..),
    tryDriverFleetContext,
    getDriverFleetContext,
    failOpen,
    noDriverFleetContext,
    contextPolicy,
    sourceName,
    lockMessageFor,
    contextLockMessage,
    contextApplyPolicy,
    contextAutoAssign,
    isCampaignControlled,
  )
where

import qualified Data.Map.Strict as Map
import qualified Domain.Types.DriverPreferencePolicy as DDPP
import qualified Domain.Types.FleetConfig as DFC
import qualified Domain.Types.FleetDriverAssociation as DFDA
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.JSON
import qualified Lib.IncentiveJourney.MgCampaign as Mg
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified SharedLogic.IncentiveJourney.MgCampaign as MgCampaign
import Storage.Beam.IncentiveJourney ()
import Storage.ConfigPilot.Config.FleetConfig (getCityFleetConfigs)
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA

-- | The driver's active fleet association and that fleet's config.
data FleetLink = FleetLink
  { association :: DFDA.FleetDriverAssociation,
    fleetConfig :: Maybe DFC.FleetConfig
  }

-- | Who is in charge of the driver's booking preferences.
data ControlSource
  = -- | An unexpired IJCampaign membership (controls or behaviour-only) holds the perk slot.
    CampaignSource Mg.MgCampaignSnapshot
  | -- | No campaign; the driver's active fleet.
    FleetSource FleetLink
  | NoSource

data DriverFleetContext = DriverFleetContext
  { -- | The active fleet association, whoever is in charge (cash ride: "is in a fleet").
    fleet :: Maybe FleetLink,
    mgPerkSource :: MgCampaign.MgPerkSource,
    controlSource :: ControlSource,
    -- | The campaign's AutoAssign (from the snapshot, whatever the perk window:
    -- 'MgCampaign.mgAutoAssignConfigFromSnapshot'; Nothing for a behaviour-only campaign) when a campaign is in charge,
    -- else the fleet's.
    autoAssignConfig :: Maybe DFC.AutoAssignConfig,
    -- | The preference policy that applies to the driver ('sourcePolicy'): validation and the GET lock flags.
    preferencePolicy :: Maybe DDPP.PreferencePolicy,
    -- | The policy whose locked values the apply paths write ('sourceApplyPolicy'): as 'preferencePolicy', except that a
    -- campaign's perk counts whatever its enableIn window, so the values are in place when the window opens (as the
    -- campaign's AutoAssign is installed at opt-in: 'MgCampaign.mgAutoAssignConfigFromSnapshot').
    applyPolicy :: Maybe DDPP.PreferencePolicy
  }

noDriverFleetContext :: DriverFleetContext
noDriverFleetContext = DriverFleetContext Nothing MgCampaign.UseNoCampaign NoSource Nothing Nothing Nothing

-- | Where the fleet config is read from. 'Cached' is the ConfigPilot city cache (GET flags, dispatch, auto-assign).
-- 'Fresh' reads fleet_config by the fleet owner id from the DB: the driver preference-CHANGE handlers use it, because
-- a read that started before a fleet config save can refill the cache with the old config right after the save
-- invalidated it, which would let a driver change a just-locked preference for up to the cache TTL.
data FleetConfigRead = Cached | Fresh

resolveDriverFleetContextWith ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  FleetConfigRead ->
  Id Person ->
  Id MerchantOperatingCity ->
  m DriverFleetContext
resolveDriverFleetContextWith configRead driverId merchantOpCityId = do
  mbInfo <- QDI.findById driverId
  mgSource <- MgCampaign.resolveMgPerkSource merchantOpCityId mbInfo
  mbFleet <- readFleetLink configRead driverId merchantOpCityId
  -- Precedence: a campaign membership wins over the fleet (his "campaign in charge" rule), then the fleet.
  let source = case mgSource of
        MgCampaign.CampaignMember {snapshot} -> CampaignSource snapshot
        MgCampaign.UseNoCampaign -> maybe NoSource FleetSource mbFleet
      context =
        DriverFleetContext
          { fleet = mbFleet,
            mgPerkSource = mgSource,
            controlSource = source,
            autoAssignConfig = sourceAutoAssign source,
            preferencePolicy = Nothing,
            applyPolicy = Nothing
          }
  pure context {preferencePolicy = sourcePolicy context, applyPolicy = sourceApplyPolicy context}

sourceAutoAssign :: ControlSource -> Maybe DFC.AutoAssignConfig
sourceAutoAssign = \case
  CampaignSource snapshot -> MgCampaign.mgAutoAssignConfigFromSnapshot snapshot
  FleetSource link -> link.fleetConfig >>= (.autoAssign)
  NoSource -> Nothing

-- | The single switch for which preference policy applies.
--
--   * A campaign in charge (including a behaviour-only one): the campaign's own PreferencePolicy perk
--     (controlFeatures @{"feature":"PreferencePolicy","config":{"locked":[...],"hidden":[...]},"enableIn":...}@) while
--     its enableIn window is open ('MgCampaign.mgPreferencePolicyValue': AllDay = while a member, TimeBound = inside the
--     first milestone's timeBounds, the default), decoded with the normal JSON decode, as the fleet column. Else
--     Nothing. The campaign wins: the fleet's locks are suppressed like its other perks (auto-assign, cancel, pooling)
--     and never merged with the campaign's. Unassign the driver from the campaign to bring the fleet's locks back.
--   * Otherwise the active fleet's policy.
sourcePolicy :: DriverFleetContext -> Maybe DDPP.PreferencePolicy
sourcePolicy context = case context.controlSource of
  CampaignSource _ -> MgCampaign.mgPreferencePolicyValue context.mgPerkSource >>= Kernel.Utils.JSON.valueToMaybe
  FleetSource link -> link.fleetConfig >>= (.preferencePolicy)
  NoSource -> Nothing

-- | 'sourcePolicy' for the apply paths: a campaign's PreferencePolicy perk whatever its enableIn window.
sourceApplyPolicy :: DriverFleetContext -> Maybe DDPP.PreferencePolicy
sourceApplyPolicy context = case context.controlSource of
  CampaignSource snapshot -> snapshot.preferencePolicy >>= Kernel.Utils.JSON.valueToMaybe
  _ -> sourcePolicy context

readFleetLink :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FleetConfigRead -> Id Person -> Id MerchantOperatingCity -> m (Maybe FleetLink)
readFleetLink configRead driverId merchantOpCityId = do
  associations <- QFDA.findAllByDriverId driverId True
  case associations of
    [] -> pure Nothing
    (newest : rest) -> do
      unless (null rest) $
        logWarning $
          "Driver " <> driverId.getId <> " has " <> show (length associations)
            <> " active fleet associations; using the newest (fleet "
            <> newest.fleetOwnerId
            <> ")"
      fleetConfig <- case configRead of
        Cached -> Map.lookup newest.fleetOwnerId <$> getCityFleetConfigs merchantOpCityId
        -- Same city scoping as the cache, which only holds the city's configs.
        Fresh -> mfilter ((== Just merchantOpCityId) . (.merchantOperatingCityId)) <$> QFC.findByPrimaryKey (Id newest.fleetOwnerId)
      pure $ Just FleetLink {association = newest, fleetConfig}

-- | The lookup with its read error kept, for a caller that must not fail open on it: the cash-ride guard (which still
-- wants the policy to 'failOpen'), and, with @>>= either throwM pure@, the callers that throw on it (fleet auto-assign,
-- the fleet policy job).
tryDriverFleetContext ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  FleetConfigRead ->
  Id Person ->
  Id MerchantOperatingCity ->
  m (Either SomeException DriverFleetContext)
tryDriverFleetContext configRead driverId merchantOpCityId = withTryCatch "getDriverFleetContext" $ resolveDriverFleetContextWith configRead driverId merchantOpCityId

-- | Fails OPEN: a read error is logged and treated as "no source", so an outage never blocks drivers.
getDriverFleetContext ::
  (MonadFlow m, BeamFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  FleetConfigRead ->
  Id Person ->
  Id MerchantOperatingCity ->
  m DriverFleetContext
getDriverFleetContext configRead driverId merchantOpCityId = tryDriverFleetContext configRead driverId merchantOpCityId >>= failOpen driverId

failOpen :: (Monad m, Log m) => Id Person -> Either SomeException DriverFleetContext -> m DriverFleetContext
failOpen driverId = \case
  Left err -> do
    logError $ "Fleet context lookup failed for driver " <> driverId.getId <> ", treating it as no fleet: " <> show err
    pure noDriverFleetContext
  Right context -> pure context

contextPolicy :: DriverFleetContext -> Maybe DDPP.PreferencePolicy
contextPolicy = (.preferencePolicy)

-- | Who manages a locked setting, in the driver-facing wording (the lock messages: 'lockMessageFor', and the 400s in
-- 'SharedLogic.DriverPreferencePolicy'). A new source (city, ops) adds a case here. The error codes stay
-- PREFERENCE_LOCKED_BY_FLEET / SERVICE_TIER_LOCKED_BY_FLEET for every source.
sourceName :: ControlSource -> Text
sourceName = \case
  CampaignSource _ -> "campaign"
  _ -> "fleet"

-- | The GET responses' 'lockMessage' (set only when a lock flag in the response is true).
lockMessageFor :: ControlSource -> Maybe Text
lockMessageFor = \case
  NoSource -> Nothing
  source -> Just $ "Managed by your " <> sourceName source

-- | 'lockMessageFor' the source in charge when @anyLocked@, else Nothing.
contextLockMessage :: DriverFleetContext -> Bool -> Maybe Text
contextLockMessage context anyLocked = if anyLocked then lockMessageFor context.controlSource else Nothing

contextApplyPolicy :: DriverFleetContext -> Maybe DDPP.PreferencePolicy
contextApplyPolicy = (.applyPolicy)

contextAutoAssign :: DriverFleetContext -> Maybe DFC.AutoAssignConfig
contextAutoAssign = (.autoAssignConfig)

isCampaignControlled :: DriverFleetContext -> Bool
isCampaignControlled context = case context.controlSource of
  CampaignSource _ -> True
  _ -> False
