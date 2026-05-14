{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.DriverVehicleQuality
  ( getDriverVehicleQualityList,
    getDriverVehicleQualitySearch,
    postDriverVehicleQualityUpdateVehicleRating,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.DriverVehicleQuality
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getDriverVehicleQualityList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Double -> Dashboard.Common.VehicleVariant -> Environment.Flow API.Types.ProviderPlatform.Management.DriverVehicleQuality.DriverVehicleQualityListRes)
getDriverVehicleQualityList merchantShortId opCity apiTokenInfo limit maxVehicleRating offset maxVehicleAge minDriverRating vehicleVariant = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.driverVehicleQualityDSL.getDriverVehicleQualityList) limit maxVehicleRating offset maxVehicleAge minDriverRating vehicleVariant

getDriverVehicleQualitySearch :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow [API.Types.ProviderPlatform.Management.DriverVehicleQuality.DriverVehicleQualityResp])
getDriverVehicleQualitySearch merchantShortId opCity apiTokenInfo phoneNumber vehicleNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.driverVehicleQualityDSL.getDriverVehicleQualitySearch) phoneNumber vehicleNumber

postDriverVehicleQualityUpdateVehicleRating :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DriverVehicleQuality.UpdateVehicleRatingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverVehicleQualityUpdateVehicleRating merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.driverVehicleQualityDSL.postDriverVehicleQualityUpdateVehicleRating) req)
