{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Payment
  ( API.Types.Dashboard.AppManagement.Payment.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Payment
import qualified Domain.Action.Dashboard.AppManagement.Payment
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.RefundRequest
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Payment.API)
handler merchantId city = getPaymentRefundRequestList merchantId city :<|> getPaymentRefundRequestInfo merchantId city :<|> postPaymentRefundRequestRespond merchantId city

getPaymentRefundRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestCode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestResp)
getPaymentRefundRequestList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentRefundRequestList a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getPaymentRefundRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestInfoResp)
getPaymentRefundRequestInfo a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentRefundRequestInfo a4 a3 a2 a1

postPaymentRefundRequestRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestRespond a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.postPaymentRefundRequestRespond a4 a3 a2 a1
