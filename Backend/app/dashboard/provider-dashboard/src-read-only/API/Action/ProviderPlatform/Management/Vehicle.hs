{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Vehicle
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Vehicle
import qualified Dashboard.Common.Driver
import qualified Domain.Action.ProviderPlatform.Management.Vehicle
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("vehicle" :> (GetVehicleList :<|> GetVehicleInfo))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVehicleList merchantId city :<|> getVehicleInfo merchantId city

type GetVehicleList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.VEHICLE / 'API.Types.ProviderPlatform.Management.Vehicle.GET_VEHICLE_LIST)
      :> API.Types.ProviderPlatform.Management.Vehicle.GetVehicleList
  )

type GetVehicleInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.VEHICLE / 'API.Types.ProviderPlatform.Management.Vehicle.GET_VEHICLE_INFO)
      :> API.Types.ProviderPlatform.Management.Vehicle.GetVehicleInfo
  )

getVehicleList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Dashboard.Common.Driver.ApprovalStatusFilter -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Vehicle.VehicleListRes)
getVehicleList merchantShortId opCity apiTokenInfo limit offset fleetOwnerId vehicleNumber verified approvalStatus from to requestorId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Vehicle.getVehicleList merchantShortId opCity apiTokenInfo limit offset fleetOwnerId vehicleNumber verified approvalStatus from to requestorId

getVehicleInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Vehicle.VehicleListItem)
getVehicleInfo merchantShortId opCity apiTokenInfo vehicleNumber = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Vehicle.getVehicleInfo merchantShortId opCity apiTokenInfo vehicleNumber
