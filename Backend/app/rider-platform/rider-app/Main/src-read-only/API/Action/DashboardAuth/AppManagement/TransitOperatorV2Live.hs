{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.TransitOperatorV2Live
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperatorV2Live
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Control.Lens
import qualified Domain.Action.Dashboard.AppManagement.TransitOperatorV2Live
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("transitOperatorV2" :> (TransitOperatorV2ListLiveVehicles :<|> TransitOperatorV2GetLiveVehicle))

type TransitOperatorV2ListLiveVehicles =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_LIVE/TRANSIT_OPERATOR_V2_LIST_LIVE_VEHICLES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Live.TransitOperatorV2ListLiveVehicles
  )

type TransitOperatorV2GetLiveVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_LIVE/TRANSIT_OPERATOR_V2_GET_LIVE_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Live.TransitOperatorV2GetLiveVehicle
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = transitOperatorV2ListLiveVehicles merchantId city :<|> transitOperatorV2GetLiveVehicle merchantId city

transitOperatorV2ListLiveVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperatorV2Live.LiveVehicleList)
transitOperatorV2ListLiveVehicles a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a7 a9 a8
        Tools.ActorInfo.withDashboardUserActorInfo a7 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Live.transitOperatorV2ListLiveVehicles a9 a8 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7)) a1
    )

transitOperatorV2GetLiveVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperatorV2Live.LiveVehicleDetail)
transitOperatorV2GetLiveVehicle a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Live.transitOperatorV2GetLiveVehicle a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )
