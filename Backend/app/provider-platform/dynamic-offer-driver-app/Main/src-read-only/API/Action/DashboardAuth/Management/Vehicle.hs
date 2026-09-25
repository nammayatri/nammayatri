{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Vehicle
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Vehicle
import qualified Dashboard.Common.Driver
import qualified Domain.Action.Dashboard.Management.Vehicle
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("vehicle" :> (GetVehicleList :<|> PostVehicleParkingFeeExemption))

type GetVehicleList = (DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/VEHICLE/GET_VEHICLE_LIST" :> API.Types.ProviderPlatform.Management.Vehicle.GetVehicleList)

type PostVehicleParkingFeeExemption =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_MANAGEMENT/VEHICLE/POST_VEHICLE_PARKING_FEE_EXEMPTION"
      :> API.Types.ProviderPlatform.Management.Vehicle.PostVehicleParkingFeeExemption
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVehicleList merchantId city :<|> postVehicleParkingFeeExemption merchantId city

getVehicleList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Dashboard.Common.Driver.ApprovalStatusFilter -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Vehicle.VehicleListRes)
getVehicleList a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a10 $ Domain.Action.Dashboard.Management.Vehicle.getVehicleList a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 a1

postVehicleParkingFeeExemption :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Vehicle.ParkingFeeExemptionReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVehicleParkingFeeExemption a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/VEHICLE/POST_VEHICLE_PARKING_FEE_EXEMPTION" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.Vehicle.postVehicleParkingFeeExemption a5 a4 a2 a1
    )
