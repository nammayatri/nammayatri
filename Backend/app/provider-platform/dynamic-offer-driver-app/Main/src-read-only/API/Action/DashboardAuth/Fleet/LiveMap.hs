{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Fleet.LiveMap
  ( API,
    handler,
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

type API = ("liveMap" :> GetLiveMapDrivers)

type GetLiveMapDrivers = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_FLEET/LIVE_MAP/GET_LIVE_MAP_DRIVERS" :> API.Types.ProviderPlatform.Fleet.LiveMap.GetLiveMapDrivers)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getLiveMapDrivers merchantId city

getLiveMapDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.External.Maps.Types.LatLong) -> Environment.FlowHandler [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes])
getLiveMapDrivers a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  (mbFleetOwnerId, requestorId) <- SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased (Tools.Auth.DashboardUserAuth.requestorHasFleetMemberHierarchy a5) (Tools.Auth.DashboardUserAuth.requestorIsFleetOwner a5) (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5) a3
  Domain.Action.Dashboard.Fleet.LiveMap.getLiveMapDrivers a7 a6 a4 requestorId mbFleetOwnerId a2 a1
