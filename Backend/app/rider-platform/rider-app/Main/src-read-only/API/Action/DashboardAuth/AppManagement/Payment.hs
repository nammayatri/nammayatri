{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Payment
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Payment
import qualified "this" API.Types.UI.RidePayment
import qualified Domain.Action.Dashboard.AppManagement.Payment
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.RefundRequest
import qualified "this" Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("payment" :> (GetPaymentRefundRequestList :<|> GetPaymentRefundRequestInfo :<|> PostPaymentRefundRequestRespond :<|> PostPaymentRefundRequestInitiate :<|> GetPaymentFareBreakup))

type GetPaymentRefundRequestList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PAYMENT/GET_PAYMENT_REFUND_REQUEST_LIST"
      :> API.Types.Dashboard.AppManagement.Payment.GetPaymentRefundRequestList
  )

type GetPaymentRefundRequestInfo =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PAYMENT/GET_PAYMENT_REFUND_REQUEST_INFO"
      :> API.Types.Dashboard.AppManagement.Payment.GetPaymentRefundRequestInfo
  )

type PostPaymentRefundRequestRespond =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PAYMENT/POST_PAYMENT_REFUND_REQUEST_RESPOND"
      :> API.Types.Dashboard.AppManagement.Payment.PostPaymentRefundRequestRespond
  )

type PostPaymentRefundRequestInitiate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PAYMENT/POST_PAYMENT_REFUND_REQUEST_INITIATE"
      :> API.Types.Dashboard.AppManagement.Payment.PostPaymentRefundRequestInitiate
  )

type GetPaymentFareBreakup = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/PAYMENT/GET_PAYMENT_FARE_BREAKUP" :> API.Types.Dashboard.AppManagement.Payment.GetPaymentFareBreakup)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPaymentRefundRequestList merchantId city :<|> getPaymentRefundRequestInfo merchantId city :<|> postPaymentRefundRequestRespond merchantId city :<|> postPaymentRefundRequestInitiate merchantId city :<|> getPaymentFareBreakup merchantId city

getPaymentRefundRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.RefundRequest.RefundRequestStatus) -> Kernel.Prelude.Maybe (Domain.Types.RefundRequest.RefundRequestCode) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestResp)
getPaymentRefundRequestList a11 a10 _a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentRefundRequestList a11 a10 a8 a7 a6 a5 a4 a3 a2 a1

getPaymentRefundRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestInfoResp)
getPaymentRefundRequestInfo a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentRefundRequestInfo a5 a4 a2 a1

postPaymentRefundRequestRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestRespond a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.postPaymentRefundRequestRespond a5 a4 a2 a1

postPaymentRefundRequestInitiate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> API.Types.Dashboard.AppManagement.Payment.RefundRequestInitiateReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestInitiate a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.postPaymentRefundRequestInitiate a6 a5 a3 a2 a1

getPaymentFareBreakup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler API.Types.UI.RidePayment.FareBreakupRes)
getPaymentFareBreakup a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentFareBreakup a4 a3 a1
