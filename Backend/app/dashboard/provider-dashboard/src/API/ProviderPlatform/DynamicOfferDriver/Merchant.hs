{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Merchant
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "merchant"
    :> ( MerchantUpdateAPI
           :<|> MapsServiceConfigUpdateAPI
           :<|> MapsServiceUsageConfigUpdateAPI
           :<|> SmsServiceConfigUpdateAPI
           :<|> SmsServiceUsageConfigUpdateAPI
           :<|> VerificationServiceConfigUpdateAPI
       )

type MerchantUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'MERCHANT_UPDATE
    :> Common.MerchantUpdateAPI

type MapsServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'MAPS_SERVICE_CONFIG_UPDATE
    :> Common.MapsServiceConfigUpdateAPI

type MapsServiceUsageConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'MAPS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.MapsServiceUsageConfigUpdateAPI

type SmsServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'SMS_SERVICE_CONFIG_UPDATE
    :> Common.SmsServiceConfigUpdateAPI

type SmsServiceUsageConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'SMS_SERVICE_USAGE_CONFIG_UPDATE
    :> Common.SmsServiceUsageConfigUpdateAPI

type VerificationServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'MERCHANT 'VERIFICATION_SERVICE_CONFIG_UPDATE
    :> Common.VerificationServiceConfigUpdateAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> mapsServiceConfigUpdate merchantId
    :<|> mapsServiceUsageConfigUpdate merchantId
    :<|> smsServiceConfigUpdate merchantId
    :<|> smsServiceUsageConfigUpdate merchantId
    :<|> verificationServiceConfigUpdate merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.MerchantEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.MerchantAPI endpoint) (Just DRIVER_OFFER_BPP) (Just apiTokenInfo) Nothing Nothing

merchantUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MerchantUpdateReq ->
  FlowHandler Common.MerchantUpdateRes
merchantUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMerchantUpdateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MerchantUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.merchantUpdate) req

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MapsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.mapsServiceConfigUpdate) req

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MapsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.mapsServiceUsageConfigUpdate) req

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.SmsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.smsServiceConfigUpdate) req

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.SmsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.smsServiceUsageConfigUpdate) req

verificationServiceConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.VerificationServiceConfigUpdateReq ->
  FlowHandler APISuccess
verificationServiceConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.VerificationServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.verificationServiceConfigUpdate) req
