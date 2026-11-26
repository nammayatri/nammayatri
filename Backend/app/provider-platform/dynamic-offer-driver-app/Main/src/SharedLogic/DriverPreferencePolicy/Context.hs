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
-- How to add a new source (e.g. campaign preference locks, a city policy):
--
--   1. Add a constructor to 'ControlSource' and decide its precedence in 'resolveDriverFleetContextWith'.
--   2. Give it its policy in 'sourcePolicy' and its auto-assign config in 'sourceAutoAssign'.
--   That is the whole change: every validator, GET flag and trigger already follows 'preferencePolicy' /
--   'controlSource'.
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
    -- | The preference policy that applies to the driver ('sourcePolicy').
    preferencePolicy :: Maybe DDPP.PreferencePolicy
  }

noDriverFleetContext :: DriverFleetContext
noDriverFleetContext = DriverFleetContext Nothing MgCampaign.UseNoCampaign NoSource Nothing Nothing

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
            preferencePolicy = Nothing
          }
  pure context {preferencePolicy = sourcePolicy context}

sourceAutoAssign :: ControlSource -> Maybe DFC.AutoAssignConfig
sourceAutoAssign = \case
  CampaignSource snapshot -> MgCampaign.mgAutoAssignConfigFromSnapshot snapshot
  FleetSource link -> link.fleetConfig >>= (.autoAssign)
  NoSource -> Nothing

-- | The single switch for which preference policy applies.
--
--   * A campaign in charge (including a behaviour-only one): no fleet policy. The fleet's locks are suppressed like
--     its other perks (auto-assign, cancel, pooling): no validation, no GET lock flags, no apply. Unassign the driver
--     from the campaign to bring the fleet's locks back.
--   * Otherwise the active fleet's policy.
sourcePolicy :: DriverFleetContext -> Maybe DDPP.PreferencePolicy
sourcePolicy context = case context.controlSource of
  CampaignSource _ -> Nothing
  FleetSource link -> link.fleetConfig >>= (.preferencePolicy)
  NoSource -> Nothing

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

contextAutoAssign :: DriverFleetContext -> Maybe DFC.AutoAssignConfig
contextAutoAssign = (.autoAssignConfig)

isCampaignControlled :: DriverFleetContext -> Bool
isCampaignControlled context = case context.controlSource of
  CampaignSource _ -> True
  _ -> False
