module Domain.Action.ProviderPlatform.Operator.Driver
  ( getDriverOperatorFetchHubRequests,
    postDriverOperatorRespondHubRequest,
    postDriverOperatorSendJoiningOtp,
    postDriverOperatorVerifyJoiningOtp,
    postDriverOperatorAddDrivers,
  )
where

import qualified API.Client.ProviderPlatform.Operator as Client
import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified Dashboard.Common
import qualified Dashboard.ProviderPlatform.Fleet.Driver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Data.ByteString.Lazy as LBS
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

getDriverOperatorFetchHubRequests ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestStatus ->
  Kernel.Prelude.Maybe API.Types.ProviderPlatform.Operator.Driver.RequestType ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.OperationHub) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
  Environment.Flow API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp
getDriverOperatorFetchHubRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbOperationHubId mbRegistrationNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.getDriverOperatorFetchHubRequests) mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbOperationHubId mbRegistrationNo

postDriverOperatorRespondHubRequest :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postDriverOperatorRespondHubRequest merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.postDriverOperatorRespondHubRequest) req{operatorId = apiTokenInfo.personId.getId}

postDriverOperatorSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.Flow Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverOperatorSendJoiningOtp merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.postDriverOperatorSendJoiningOtp) (Just apiTokenInfo.personId.getId) req

postDriverOperatorVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Operator.Driver.VerifyOperatorJoiningOtpReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverOperatorVerifyJoiningOtp merchantShortId opCity apiTokenInfo authId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.postDriverOperatorVerifyJoiningOtp) authId (Just apiTokenInfo.personId.getId) req

postDriverOperatorAddDrivers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Driver.CreateDriversReq -> Environment.Flow Dashboard.ProviderPlatform.Fleet.Driver.APISuccessWithUnprocessedEntities)
postDriverOperatorAddDrivers merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ Client.callOperatorAPI checkedMerchantId opCity (addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverOperatorAddDrivers)) (Just apiTokenInfo.personId.getId) req
  where
    addMultipartBoundary :: LBS.ByteString -> (Maybe Text -> (LBS.ByteString, req) -> res) -> Maybe Text -> req -> res
    addMultipartBoundary boundary clientFn requestorId reqBody = clientFn requestorId (boundary, reqBody)
