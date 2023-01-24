module API.BAP.Merchant
  ( API,
    handler,
  )
where

import qualified BAPClient.AppBackend as Client
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common (MonadFlow, throwError, withFlowHandlerAPI)
import Beckn.Utils.Validation (runRequestValidation)
import qualified "dashboard-bpp-helper-api" Dashboard.BAP.Merchant as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "merchant"
    :> ( MerchantUpdateAPI
           :<|> MerchantServiceConfigUpdateAPI
           :<|> MerchantServiceConfigUsageUpdateAPI
       )

type MerchantUpdateAPI =
  ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'MERCHANT
    :> Common.MerchantUpdateAPI

type MerchantServiceConfigUpdateAPI =
  ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'MERCHANT
    :> Common.MerchantServiceConfigUpdateAPI

type MerchantServiceConfigUsageUpdateAPI =
  ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'MERCHANT
    :> Common.MerchantServiceConfigUsageUpdateAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> merchantServiceConfigUpdate merchantId
    :<|> merchantServiceConfigUsageUpdate merchantId

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

merchantServiceConfigUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MerchantServiceConfigUpdateReq ->
  FlowHandler APISuccess
merchantServiceConfigUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MerchantServiceConfigUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callAppBackendBAP checkedMerchantId (.merchant.merchantServiceConfigUpdate) req

merchantServiceConfigUsageUpdate ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Common.MerchantServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
merchantServiceConfigUsageUpdate merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  runRequestValidation Common.validateMerchantServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.MerchantServiceConfigUsageUpdateEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callAppBackendBAP checkedMerchantId (.merchant.merchantServiceConfigUsageUpdate) req
