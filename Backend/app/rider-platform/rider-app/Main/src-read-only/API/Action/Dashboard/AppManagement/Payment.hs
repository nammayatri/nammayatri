{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Payment
  ( API.Types.Dashboard.AppManagement.Payment.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Payment
import qualified "this" API.Types.UI.RidePayment
import qualified Dashboard.Common
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
import qualified Tools.ActorInfo
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Payment.API)
handler merchantId city = getPaymentRefundRequestList merchantId city :<|> getPaymentRefundRequestInfo merchantId city :<|> postPaymentRefundRequestRespond merchantId city :<|> postPaymentRefundRequestInitiate merchantId city :<|> postPaymentRefundRequestBookingInitiate merchantId city :<|> getPaymentFareBreakup merchantId city

getPaymentRefundRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestCode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestResp)
getPaymentRefundRequestList a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a1) $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentRefundRequestList a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getPaymentRefundRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestInfoResp)
getPaymentRefundRequestInfo a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a1) $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentRefundRequestInfo a5 a4 a3 a2 a1

postPaymentRefundRequestRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestRespond a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.Payment.postPaymentRefundRequestRespond a5 a4 a3 a2 a1

postPaymentRefundRequestInitiate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.Payment.RefundRequestInitiateReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestInitiate a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.Payment.postPaymentRefundRequestInitiate a6 a5 a4 a3 a2 a1

postPaymentRefundRequestBookingInitiate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Booking -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestBookingInitiate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a1) $ Domain.Action.Dashboard.AppManagement.Payment.postPaymentRefundRequestBookingInitiate a4 a3 a2 a1

getPaymentFareBreakup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.RidePayment.FareBreakupRes)
getPaymentFareBreakup a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a1) $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentFareBreakup a4 a3 a2 a1
