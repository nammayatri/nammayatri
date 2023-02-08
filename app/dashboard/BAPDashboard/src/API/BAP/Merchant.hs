module API.BAP.Merchant
  ( API,
    handler,
  )
where

import qualified BAPClient.AppBackend as Client
import qualified "dashboard-bpp-helper-api" Dashboard.BAP.Merchant as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, throwError, withFlowHandlerAPI)
import Kernel.Utils.Validation (runRequestValidation)
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
       )

type MerchantUpdateAPI =
  ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'MERCHANT
    :> Common.MerchantUpdateAPI

type MapsServiceConfigUpdateAPI =
  ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'MERCHANT
    :> Common.MapsServiceConfigUpdateAPI

type MapsServiceUsageConfigUpdateAPI =
  ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'MERCHANT
    :> Common.MapsServiceUsageConfigUpdateAPI

type SmsServiceConfigUpdateAPI =
  ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'MERCHANT
    :> Common.SmsServiceConfigUpdateAPI

type SmsServiceUsageConfigUpdateAPI =
  ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'MERCHANT
    :> Common.SmsServiceUsageConfigUpdateAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> mapsServiceConfigUpdate merchantId
    :<|> mapsServiceUsageConfigUpdate merchantId
    :<|> smsServiceConfigUpdate merchantId
    :<|> smsServiceUsageConfigUpdate merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.MerchantEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.MerchantAPI endpoint) apiTokenInfo Nothing Nothing

merchantUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MerchantUpdateReq ->
  FlowHandler APISuccess
merchantUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMerchantUpdateReq req
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MerchantUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callAppBackendBAP checkedMerchantId (.merchant.merchantUpdate) req

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MapsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callAppBackendBAP checkedMerchantId (.merchant.mapsServiceConfigUpdate) req

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MapsServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callAppBackendBAP checkedMerchantId (.merchant.mapsServiceUsageConfigUpdate) req

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.SmsServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callAppBackendBAP checkedMerchantId (.merchant.smsServiceConfigUpdate) req

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
    Client.callAppBackendBAP checkedMerchantId (.merchant.smsServiceUsageConfigUpdate) req
