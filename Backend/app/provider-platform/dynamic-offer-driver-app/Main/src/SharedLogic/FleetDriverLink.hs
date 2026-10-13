{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fleet-driver link / unlink wrappers that also apply or clear the fleet-owner
-- driver controls from "SharedLogic.FleetFeatures".
--
-- These live here rather than in "Storage.Queries.FleetDriverAssociationExtra"
-- because Storage.Queries must never import SharedLogic.
module SharedLogic.FleetDriverLink
  ( createFleetDriverAssociationWithJoinDefaults,
    endFleetDriverAssociationWithCleanup,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Person (Person)
import Domain.Types.VehicleCategory (VehicleCategory)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified SharedLogic.FleetFeatures as SFleetFeatures
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA

type FleetDriverLinkFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  )

-- | 'QFDA.createFleetDriverAssociationIfNotExists', plus the fleet's join defaults
-- ('SFleetFeatures.applyFleetJoinDefaults') when a NEW, ACTIVE association row was created.
--
-- An inactive (pending invite / pending request) row is not a join yet: the defaults
-- are applied when it is activated (driver consent / fleet approval), so that a
-- driver who never joins is not left with fleet-managed state.
--
-- The defaults run after the per driver+fleet lock is released (not inside
-- @onCreated@), so the association lock is not held across the auto-assign writes.
-- 'SFleetFeatures.applyFleetJoinDefaults' never throws.
createFleetDriverAssociationWithJoinDefaults ::
  FleetDriverLinkFlow m r =>
  Id Person ->
  Id Person ->
  Maybe (Id Person) ->
  VehicleCategory ->
  Bool ->
  Maybe Text ->
  Maybe (Id Merchant) ->
  Maybe (Id MerchantOperatingCity) ->
  m () ->
  m ()
createFleetDriverAssociationWithJoinDefaults driverId fleetOwnerId onboardedOperatorId onboardingVehicleCategory isActive requestReason merchantId merchantOperatingCityId onCreated = do
  createdRef <- liftIO $ newIORef False
  QFDA.createFleetDriverAssociationIfNotExists driverId fleetOwnerId onboardedOperatorId onboardingVehicleCategory isActive requestReason merchantId merchantOperatingCityId $ do
    onCreated
    liftIO $ writeIORef createdRef True
  created <- liftIO $ readIORef createdRef
  when (created && isActive) $
    SFleetFeatures.applyFleetJoinDefaults driverId fleetOwnerId

-- | 'QFDA.endFleetDriverAssociation', plus 'SFleetFeatures.removeFleetManagedState' when the
-- association being ended was ACTIVE. Ending a pending (inactive) row must not touch
-- the driver's state: the driver may be an active member of a different fleet.
-- 'SFleetFeatures.removeFleetManagedState' never throws.
endFleetDriverAssociationWithCleanup ::
  FleetDriverLinkFlow m r =>
  Text ->
  Id Person ->
  m ()
endFleetDriverAssociationWithCleanup fleetOwnerId driverId = do
  wasActive <- isJust <$> QFDA.findByDriverIdAndFleetOwnerId driverId fleetOwnerId True
  QFDA.endFleetDriverAssociation fleetOwnerId driverId
  when wasActive $
    SFleetFeatures.removeFleetManagedState driverId (Id fleetOwnerId)
