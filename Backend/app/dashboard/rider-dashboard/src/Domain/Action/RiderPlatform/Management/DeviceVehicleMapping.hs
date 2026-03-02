{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.DeviceVehicleMapping
  ( getDeviceVehicleMappingDeviceVehicleMappingList,
    postDeviceVehicleMappingDeviceVehicleMappingReplace,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.DeviceVehicleMapping
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getDeviceVehicleMappingDeviceVehicleMappingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity -> Environment.Flow API.Types.RiderPlatform.Management.DeviceVehicleMapping.DeviceVehicleMappingListRes)
getDeviceVehicleMappingDeviceVehicleMappingList merchantShortId opCity apiTokenInfo merchantOperatingCityId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.deviceVehicleMappingDSL.getDeviceVehicleMappingDeviceVehicleMappingList) merchantOperatingCityId

postDeviceVehicleMappingDeviceVehicleMappingUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity -> API.Types.RiderPlatform.Management.DeviceVehicleMapping.UpsertDeviceVehicleMappingReq -> Environment.Flow API.Types.RiderPlatform.Management.DeviceVehicleMapping.UpsertDeviceVehicleMappingResp)
postDeviceVehicleMappingDeviceVehicleMappingUpsert merchantShortId opCity apiTokenInfo merchantOperatingCityId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.deviceVehicleMappingDSL.postDeviceVehicleMappingDeviceVehicleMappingUpsert)) merchantOperatingCityId req)
