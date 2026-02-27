{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.DeviceVehicleMapping
  ( API.Types.RiderPlatform.Management.DeviceVehicleMapping.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.DeviceVehicleMapping
import qualified Domain.Action.Dashboard.DeviceVehicleMapping
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.DeviceVehicleMapping.API)
handler merchantId city = getDeviceVehicleMappingDeviceVehicleMappingList merchantId city :<|> postDeviceVehicleMappingDeviceVehicleMappingUpsert merchantId city

getDeviceVehicleMappingDeviceVehicleMappingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.RiderPlatform.Management.DeviceVehicleMapping.DeviceVehicleMappingListRes)
getDeviceVehicleMappingDeviceVehicleMappingList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.DeviceVehicleMapping.getDeviceVehicleMappingDeviceVehicleMappingList a2 a1

postDeviceVehicleMappingDeviceVehicleMappingUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.DeviceVehicleMapping.UpsertDeviceVehicleMappingReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.DeviceVehicleMapping.UpsertDeviceVehicleMappingResp)
postDeviceVehicleMappingDeviceVehicleMappingUpsert a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.DeviceVehicleMapping.postDeviceVehicleMappingDeviceVehicleMappingUpsert a3 a2 a1
