module Domain.Action.RiderPlatform.AppManagement.Payment
  ( getPaymentRefundRequestList,
    getPaymentRefundRequestInfo,
    postPaymentRefundRequestRespond,
    postPaymentRefundRequestInitiate,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified API.Types.Dashboard.AppManagement.Payment
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.RefundRequest
import qualified "rider-app" Domain.Types.Ride
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getPaymentRefundRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestCode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.Dashboard.AppManagement.Payment.RefundRequestResp)
getPaymentRefundRequestList merchantShortId opCity apiTokenInfo limit offset status code customerId orderId from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.paymentDSL.getPaymentRefundRequestList) limit offset status code customerId orderId from to

getPaymentRefundRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow API.Types.Dashboard.AppManagement.Payment.RefundRequestInfoResp)
getPaymentRefundRequestInfo merchantShortId opCity apiTokenInfo refundRequestId refreshRefunds = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.paymentDSL.getPaymentRefundRequestInfo) refundRequestId refreshRefunds

postPaymentRefundRequestRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondReq -> Environment.Flow API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestRespond merchantShortId opCity apiTokenInfo refundRequestId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withResponseTransactionStoring transaction (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.paymentDSL.postPaymentRefundRequestRespond) refundRequestId req)

postPaymentRefundRequestInitiate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> API.Types.Dashboard.AppManagement.Payment.RefundRequestInitiateReq -> Environment.Flow API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestInitiate merchantShortId opCity apiTokenInfo rideId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing (Kernel.Prelude.Just (Kernel.Types.Id.cast rideId)) (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withResponseTransactionStoring transaction (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.paymentDSL.postPaymentRefundRequestInitiate) rideId req)
