{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.DriverVehicleQuality
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverVehicleQuality
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.DriverVehicleQuality
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("driverVehicleQuality" :> (GetDriverVehicleQualityList :<|> GetDriverVehicleQualitySearch :<|> PostDriverVehicleQualityUpdateVehicleRating))

type GetDriverVehicleQualityList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_VEHICLE_QUALITY/GET_DRIVER_VEHICLE_QUALITY_LIST"
      :> API.Types.ProviderPlatform.Management.DriverVehicleQuality.GetDriverVehicleQualityList
  )

type GetDriverVehicleQualitySearch =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_VEHICLE_QUALITY/GET_DRIVER_VEHICLE_QUALITY_SEARCH"
      :> API.Types.ProviderPlatform.Management.DriverVehicleQuality.GetDriverVehicleQualitySearch
  )

type PostDriverVehicleQualityUpdateVehicleRating =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_VEHICLE_QUALITY/POST_DRIVER_VEHICLE_QUALITY_UPDATE_VEHICLE_RATING"
      :> API.Types.ProviderPlatform.Management.DriverVehicleQuality.PostDriverVehicleQualityUpdateVehicleRating
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverVehicleQualityList merchantId city :<|> getDriverVehicleQualitySearch merchantId city :<|> postDriverVehicleQualityUpdateVehicleRating merchantId city

getDriverVehicleQualityList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Dashboard.Common.VehicleVariant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe ([Dashboard.Common.VehicleVariant]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Int -> Kernel.Prelude.Double -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverVehicleQuality.DriverVehicleQualityListRes)
getDriverVehicleQualityList a11 a10 _a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverVehicleQuality.getDriverVehicleQualityList a11 a10 a8 a7 a6 a5 a4 a3 a2 a1

getDriverVehicleQualitySearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.DriverVehicleQuality.DriverVehicleQualityResp])
getDriverVehicleQualitySearch a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverVehicleQuality.getDriverVehicleQualitySearch a5 a4 a2 a1

postDriverVehicleQualityUpdateVehicleRating :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DriverVehicleQuality.UpdateVehicleRatingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverVehicleQualityUpdateVehicleRating a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverVehicleQuality.postDriverVehicleQualityUpdateVehicleRating a4 a3 a1
