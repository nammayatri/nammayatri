{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for direct-dashboard routes whose request needs more
-- than the verified operator's id or name: fleet-owner resolution, fleet-owner
-- verification, dashboard-database writes after the call, and similar.
--
-- provider-dashboard did this work in its own hand-written
-- @Domain.Action.ProviderPlatform.*@ layer before forwarding the call. The
-- generated @API.Action.DashboardAuth@ handler calls these functions instead of
-- the domain handler for every endpoint marked @appServerHandler: custom@ in
-- the spec, so this logic lives here and is never overwritten by the generator.
module Domain.Action.DashboardAuth.Fleet.LiveMap
  ( getLiveMapDrivers,
    getLiveMapDemandHotspots,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Fleet.LiveMap
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Fleet
import Tools.Auth
import Tools.Auth.DashboardUserAuth

getLiveMapDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Kernel.Prelude.Maybe [Kernel.Types.Id.Id Dashboard.Common.Driver] -> Environment.Flow [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes])
getLiveMapDrivers a8 a7 a6 a5 a4 a3 a2 a1 = do
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a6) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a6) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6) a4
  Domain.Action.Dashboard.Fleet.LiveMap.getLiveMapDrivers a8 a7 a5 requestorId mbFleetOwnerId a3 a2 a1

getLiveMapDemandHotspots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.Flow API.Types.ProviderPlatform.Fleet.LiveMap.DemandHotspotsResp)
getLiveMapDemandHotspots merchantShortId opCity _dashboardUser =
  Domain.Action.Dashboard.Fleet.LiveMap.getLiveMapDemandHotspots merchantShortId opCity
