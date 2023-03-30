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
import Kernel.Utils.Common (Meters, MonadFlow, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "merchant"
    :> ( MerchantUpdateAPI
           :<|> MerchantCommonConfigUpdateAPI
           :<|> DriverPoolConfigUpdateAPI
           :<|> DriverPoolConfigCreateAPI
           :<|> DriverIntelligentPoolConfigUpdateAPI
           :<|> OnboardingDocumentConfigUpdateAPI
           :<|> OnboardingDocumentConfigCreateAPI
           :<|> MapsServiceConfigUpdateAPI
           :<|> MapsServiceUsageConfigUpdateAPI
           :<|> SmsServiceConfigUpdateAPI
           :<|> SmsServiceUsageConfigUpdateAPI
           :<|> VerificationServiceConfigUpdateAPI
       )

type MerchantUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.MerchantUpdateAPI

type MerchantCommonConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.MerchantCommonConfigUpdateAPI

type DriverPoolConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.DriverPoolConfigUpdateAPI

type DriverPoolConfigCreateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.DriverPoolConfigCreateAPI

type DriverIntelligentPoolConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.DriverIntelligentPoolConfigUpdateAPI

type OnboardingDocumentConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.OnboardingDocumentConfigUpdateAPI

type OnboardingDocumentConfigCreateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.OnboardingDocumentConfigCreateAPI

type MapsServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.MapsServiceConfigUpdateAPI

type MapsServiceUsageConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.MapsServiceUsageConfigUpdateAPI

type SmsServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.SmsServiceConfigUpdateAPI

type SmsServiceUsageConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.SmsServiceUsageConfigUpdateAPI

type VerificationServiceConfigUpdateAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MERCHANT
    :> Common.VerificationServiceConfigUpdateAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> merchantCommonConfigUpdate merchantId
    :<|> driverPoolConfigUpdate merchantId
    :<|> driverPoolConfigCreate merchantId
    :<|> driverIntelligentPoolConfigUpdate merchantId
    :<|> onboardingDocumentConfigUpdate merchantId
    :<|> onboardingDocumentConfigCreate merchantId
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

merchantCommonConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MerchantCommonConfigUpdateReq ->
  FlowHandler APISuccess
merchantCommonConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMerchantCommonConfigUpdateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MerchantCommonConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.merchantCommonConfigUpdate) req

driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Meters ->
  Common.DriverPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverPoolConfigUpdate merchantShortId apiTokenInfo tripDistance req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateDriverPoolConfigUpdateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.DriverPoolConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.driverPoolConfigUpdate) tripDistance req

driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  FlowHandler APISuccess
driverPoolConfigCreate merchantShortId apiTokenInfo tripDistance req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateDriverPoolConfigCreateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.DriverPoolConfigCreateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.driverPoolConfigCreate) tripDistance req

driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  FlowHandler APISuccess
driverIntelligentPoolConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateDriverIntelligentPoolConfigUpdateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.DriverIntelligentPoolConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.driverIntelligentPoolConfigUpdate) req

onboardingDocumentConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigUpdateReq ->
  FlowHandler APISuccess
onboardingDocumentConfigUpdate merchantShortId apiTokenInfo documentType req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateOnboardingDocumentConfigUpdateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.OnboardingDocumentConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.onboardingDocumentConfigUpdate) documentType req

onboardingDocumentConfigCreate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigCreateReq ->
  FlowHandler APISuccess
onboardingDocumentConfigCreate merchantShortId apiTokenInfo documentType req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateOnboardingDocumentConfigCreateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.OnboardingDocumentConfigCreateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.merchant.onboardingDocumentConfigCreate) documentType req

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
