{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Fleet.LiveMap
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.LiveMap
import qualified Control.Lens
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Fleet.LiveMap
import qualified Domain.Action.DashboardAuth.Fleet.LiveMap
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("liveMap" :> (GetLiveMapDrivers :<|> GetLiveMapDemandHotspots))

type GetLiveMapDrivers = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_FLEET/LIVE_MAP/GET_LIVE_MAP_DRIVERS" :> API.Types.ProviderPlatform.Fleet.LiveMap.GetLiveMapDrivers)

type GetLiveMapDemandHotspots =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_FLEET/LIVE_MAP/GET_LIVE_MAP_DEMAND_HOTSPOTS"
      :> API.Types.ProviderPlatform.Fleet.LiveMap.GetLiveMapDemandHotspots
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getLiveMapDrivers merchantId city :<|> getLiveMapDemandHotspots merchantId city

getLiveMapDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Environment.FlowHandler [API.Types.ProviderPlatform.Fleet.LiveMap.MapDriverInfoRes])
getLiveMapDrivers a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a5 a7 a6
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.DashboardAuth.Fleet.LiveMap.getLiveMapDrivers a7 a6 a5 a4 a3 a2 a1
    )

getLiveMapDemandHotspots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.LiveMap.DemandHotspotsResp)
getLiveMapDemandHotspots a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a1 a3 a2
        Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.DashboardAuth.Fleet.LiveMap.getLiveMapDemandHotspots a3 a2 a1
    )
