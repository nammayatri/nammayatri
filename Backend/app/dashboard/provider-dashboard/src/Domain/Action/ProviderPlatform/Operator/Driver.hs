module Domain.Action.ProviderPlatform.Operator.Driver
  ( getDriverOperatorFetchHubRequests,
    postDriverOperatorRespondHubRequest,
    postDriverOperatorCreateRequest,
    getDriverOperationGetAllHubs,
    getDriverOperatorList,
    postDriverOperatorSendJoiningOtp,
    postDriverOperatorVerifyJoiningOtp,
    getDriverOperatorDashboardAnalyticsAllTime,
    getDriverOperatorDashboardAnalytics,
  )
where

import qualified API.Client.ProviderPlatform.Operator as Client
import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified API.Types.ProviderPlatform.Operator.Endpoints.Driver as CommonDriver
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import Data.Time.Calendar
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
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id CommonDriver.OperationHub) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Text) ->
  Environment.Flow API.Types.ProviderPlatform.Operator.Driver.OperationHubReqResp
getDriverOperatorFetchHubRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbOperationHubId mbOperationHubName mbRegistrationNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.getDriverOperatorFetchHubRequests) mbFrom mbTo mbStatus mbReqType mbLimit mbOffset mbDriverId mbMobileNumber mbOperationHubId mbOperationHubName mbRegistrationNo

postDriverOperatorRespondHubRequest :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Driver.RespondHubRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postDriverOperatorRespondHubRequest merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.postDriverOperatorRespondHubRequest) req{operatorId = apiTokenInfo.personId.getId}

postDriverOperatorCreateRequest :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Driver.DriverOperationHubRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postDriverOperatorCreateRequest merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.postDriverOperatorCreateRequest) req{creatorId = apiTokenInfo.personId.getId}

getDriverOperationGetAllHubs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [CommonDriver.OperationHub])
getDriverOperationGetAllHubs merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.getDriverOperationGetAllHubs)

getDriverOperatorList ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    ApiTokenInfo ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow API.Types.ProviderPlatform.Operator.Driver.DriverInfoResp
  )
getDriverOperatorList merchantShortId opCity apiTokenInfo mbIsActive mbLimit mbOffset mbVehicleNo mbSearchString onlyMandatoryDocs = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.getDriverOperatorList) mbIsActive mbLimit mbOffset mbVehicleNo mbSearchString onlyMandatoryDocs apiTokenInfo.personId.getId

postDriverOperatorSendJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.Flow Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverOperatorSendJoiningOtp merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.postDriverOperatorSendJoiningOtp) apiTokenInfo.personId.getId req

postDriverOperatorVerifyJoiningOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.ProviderPlatform.Operator.Driver.VerifyOperatorJoiningOtpReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverOperatorVerifyJoiningOtp merchantShortId opCity apiTokenInfo authId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.postDriverOperatorVerifyJoiningOtp) authId apiTokenInfo.personId.getId req

getDriverOperatorDashboardAnalyticsAllTime :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow API.Types.ProviderPlatform.Operator.Driver.AllTimeOperatorAnalyticsRes)
getDriverOperatorDashboardAnalyticsAllTime merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.getDriverOperatorDashboardAnalyticsAllTime) apiTokenInfo.personId.getId

getDriverOperatorDashboardAnalytics :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> Environment.Flow API.Types.ProviderPlatform.Operator.Driver.FilteredOperatorAnalyticsRes)
getDriverOperatorDashboardAnalytics merchantShortId opCity apiTokenInfo from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callOperatorAPI checkedMerchantId opCity (.driverDSL.getDriverOperatorDashboardAnalytics) apiTokenInfo.personId.getId from to
