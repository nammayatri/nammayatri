{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.DeviceVehicleMapping
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.DeviceVehicleMapping
import qualified Domain.Action.RiderPlatform.Management.DeviceVehicleMapping
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("deviceVehicleMapping" :> (GetDeviceVehicleMappingDeviceVehicleMappingList :<|> PostDeviceVehicleMappingDeviceVehicleMappingUpsert))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDeviceVehicleMappingDeviceVehicleMappingList merchantId city :<|> postDeviceVehicleMappingDeviceVehicleMappingUpsert merchantId city

type GetDeviceVehicleMappingDeviceVehicleMappingList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.DEVICE_VEHICLE_MAPPING) / ('API.Types.RiderPlatform.Management.DeviceVehicleMapping.GET_DEVICE_VEHICLE_MAPPING_DEVICE_VEHICLE_MAPPING_LIST))
      :> API.Types.RiderPlatform.Management.DeviceVehicleMapping.GetDeviceVehicleMappingDeviceVehicleMappingList
  )

type PostDeviceVehicleMappingDeviceVehicleMappingUpsert =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.DEVICE_VEHICLE_MAPPING) / ('API.Types.RiderPlatform.Management.DeviceVehicleMapping.POST_DEVICE_VEHICLE_MAPPING_DEVICE_VEHICLE_MAPPING_UPSERT))
      :> API.Types.RiderPlatform.Management.DeviceVehicleMapping.PostDeviceVehicleMappingDeviceVehicleMappingUpsert
  )

getDeviceVehicleMappingDeviceVehicleMappingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.RiderPlatform.Management.DeviceVehicleMapping.DeviceVehicleMappingListRes)
getDeviceVehicleMappingDeviceVehicleMappingList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.DeviceVehicleMapping.getDeviceVehicleMappingDeviceVehicleMappingList merchantShortId opCity apiTokenInfo

postDeviceVehicleMappingDeviceVehicleMappingUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.DeviceVehicleMapping.UpsertDeviceVehicleMappingReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.DeviceVehicleMapping.UpsertDeviceVehicleMappingResp)
postDeviceVehicleMappingDeviceVehicleMappingUpsert merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.DeviceVehicleMapping.postDeviceVehicleMappingDeviceVehicleMappingUpsert merchantShortId opCity apiTokenInfo req
