{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.DriverVehicleQuality
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.DriverVehicleQuality
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.DriverVehicleQuality
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driverVehicleQuality" :> (GetDriverVehicleQualityList :<|> GetDriverVehicleQualitySearch :<|> PostDriverVehicleQualityUpdateVehicleRating))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverVehicleQualityList merchantId city :<|> getDriverVehicleQualitySearch merchantId city :<|> postDriverVehicleQualityUpdateVehicleRating merchantId city

type GetDriverVehicleQualityList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_VEHICLE_QUALITY / 'API.Types.ProviderPlatform.Management.DriverVehicleQuality.GET_DRIVER_VEHICLE_QUALITY_LIST)
      :> API.Types.ProviderPlatform.Management.DriverVehicleQuality.GetDriverVehicleQualityList
  )

type GetDriverVehicleQualitySearch =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_VEHICLE_QUALITY / 'API.Types.ProviderPlatform.Management.DriverVehicleQuality.GET_DRIVER_VEHICLE_QUALITY_SEARCH)
      :> API.Types.ProviderPlatform.Management.DriverVehicleQuality.GetDriverVehicleQualitySearch
  )

type PostDriverVehicleQualityUpdateVehicleRating =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_VEHICLE_QUALITY / 'API.Types.ProviderPlatform.Management.DriverVehicleQuality.POST_DRIVER_VEHICLE_QUALITY_UPDATE_VEHICLE_RATING)
      :> API.Types.ProviderPlatform.Management.DriverVehicleQuality.PostDriverVehicleQualityUpdateVehicleRating
  )

getDriverVehicleQualityList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Double -> Dashboard.Common.VehicleVariant -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverVehicleQuality.DriverVehicleQualityListRes)
getDriverVehicleQualityList merchantShortId opCity apiTokenInfo limit maxVehicleRating offset maxVehicleAge minDriverRating vehicleVariant = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverVehicleQuality.getDriverVehicleQualityList merchantShortId opCity apiTokenInfo limit maxVehicleRating offset maxVehicleAge minDriverRating vehicleVariant

getDriverVehicleQualitySearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.DriverVehicleQuality.DriverVehicleQualityResp])
getDriverVehicleQualitySearch merchantShortId opCity apiTokenInfo phoneNumber vehicleNumber = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverVehicleQuality.getDriverVehicleQualitySearch merchantShortId opCity apiTokenInfo phoneNumber vehicleNumber

postDriverVehicleQualityUpdateVehicleRating :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverVehicleQuality.UpdateVehicleRatingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverVehicleQualityUpdateVehicleRating merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverVehicleQuality.postDriverVehicleQualityUpdateVehicleRating merchantShortId opCity apiTokenInfo req
