{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Merchant
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Merchant as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, throwError, withFlowHandlerAPI')
import Kernel.Utils.Validation (runRequestValidation)
import qualified RiderPlatformClient.RiderApp.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "merchant"
    :> ( MerchantUpdateAPI
           :<|> ServiceUsageConfigAPI
           :<|> MapsServiceConfigUpdateAPI
           :<|> MapsServiceUsageConfigUpdateAPI
           :<|> SmsServiceConfigUpdateAPI
           :<|> SmsServiceUsageConfigUpdateAPI
       )

type MerchantUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'MERCHANT_UPDATE
    :> Common.MerchantUpdateAPI

type ServiceUsageConfigAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'SERVICE_USAGE_CONFIG
    :> Common.ServiceUsageConfigAPI

type MapsServiceConfigUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'MAPS_SERVICE_CONFIG_UPDATE
    :> Common.MapsServiceConfigUpdateAPI

type MapsServiceUsageConfigUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'MAPS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.MapsServiceUsageConfigUpdateAPI

type SmsServiceConfigUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'SMS_SERVICE_CONFIG_UPDATE
    :> Common.SmsServiceConfigUpdateAPI

type SmsServiceUsageConfigUpdateAPI =
  ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'SMS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.SmsServiceUsageConfigUpdateAPI

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  merchantUpdate merchantId city
    :<|> serviceUsageConfig merchantId city
    :<|> mapsServiceConfigUpdate merchantId city
    :<|> mapsServiceUsageConfigUpdate merchantId city
    :<|> smsServiceConfigUpdate merchantId city
    :<|> smsServiceUsageConfigUpdate merchantId city

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.MerchantEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.MerchantAPI endpoint) (Just APP_BACKEND_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

merchantUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MerchantUpdateReq ->
  FlowHandler APISuccess
merchantUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMerchantUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MerchantUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.merchantUpdate) req

serviceUsageConfig ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderAppOperations checkedMerchantId opCity (.merchant.serviceUsageConfig)

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MapsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.mapsServiceConfigUpdate) req

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.MapsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.mapsServiceUsageConfigUpdate) req

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.SmsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.smsServiceConfigUpdate) req

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.SmsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderAppOperations checkedMerchantId opCity (.merchant.smsServiceUsageConfigUpdate) req
