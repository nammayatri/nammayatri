{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.Payment
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Payment
import qualified "rider-app" API.Types.UI.RidePayment
import qualified Domain.Action.RiderPlatform.AppManagement.Payment
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.RefundRequest
import qualified "rider-app" Domain.Types.Ride
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("payment" :> (GetPaymentRefundRequestList :<|> GetPaymentRefundRequestInfo :<|> PostPaymentRefundRequestRespond :<|> PostPaymentRefundRequestInitiate :<|> GetPaymentFareBreakup))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPaymentRefundRequestList merchantId city :<|> getPaymentRefundRequestInfo merchantId city :<|> postPaymentRefundRequestRespond merchantId city :<|> postPaymentRefundRequestInitiate merchantId city :<|> getPaymentFareBreakup merchantId city

type GetPaymentRefundRequestList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PAYMENT / 'API.Types.Dashboard.AppManagement.Payment.GET_PAYMENT_REFUND_REQUEST_LIST)
      :> API.Types.Dashboard.AppManagement.Payment.GetPaymentRefundRequestList
  )

type GetPaymentRefundRequestInfo =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PAYMENT / 'API.Types.Dashboard.AppManagement.Payment.GET_PAYMENT_REFUND_REQUEST_INFO)
      :> API.Types.Dashboard.AppManagement.Payment.GetPaymentRefundRequestInfo
  )

type PostPaymentRefundRequestRespond =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PAYMENT / 'API.Types.Dashboard.AppManagement.Payment.POST_PAYMENT_REFUND_REQUEST_RESPOND)
      :> API.Types.Dashboard.AppManagement.Payment.PostPaymentRefundRequestRespond
  )

type PostPaymentRefundRequestInitiate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PAYMENT / 'API.Types.Dashboard.AppManagement.Payment.POST_PAYMENT_REFUND_REQUEST_INITIATE)
      :> API.Types.Dashboard.AppManagement.Payment.PostPaymentRefundRequestInitiate
  )

type GetPaymentFareBreakup =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PAYMENT / 'API.Types.Dashboard.AppManagement.Payment.GET_PAYMENT_FARE_BREAKUP)
      :> API.Types.Dashboard.AppManagement.Payment.GetPaymentFareBreakup
  )

getPaymentRefundRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestCode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestResp)
getPaymentRefundRequestList merchantShortId opCity apiTokenInfo limit offset status code customerId orderId from to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Payment.getPaymentRefundRequestList merchantShortId opCity apiTokenInfo limit offset status code customerId orderId from to

getPaymentRefundRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestInfoResp)
getPaymentRefundRequestInfo merchantShortId opCity apiTokenInfo refundRequestId refreshRefunds = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Payment.getPaymentRefundRequestInfo merchantShortId opCity apiTokenInfo refundRequestId refreshRefunds

postPaymentRefundRequestRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestRespond merchantShortId opCity apiTokenInfo refundRequestId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Payment.postPaymentRefundRequestRespond merchantShortId opCity apiTokenInfo refundRequestId req

postPaymentRefundRequestInitiate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> API.Types.Dashboard.AppManagement.Payment.RefundRequestInitiateReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestInitiate merchantShortId opCity apiTokenInfo rideId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Payment.postPaymentRefundRequestInitiate merchantShortId opCity apiTokenInfo rideId req

getPaymentFareBreakup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler API.Types.UI.RidePayment.FareBreakupRes)
getPaymentFareBreakup merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Payment.getPaymentFareBreakup merchantShortId opCity apiTokenInfo rideId
