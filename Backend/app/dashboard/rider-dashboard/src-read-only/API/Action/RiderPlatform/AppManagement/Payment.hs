{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.Payment
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Payment
import qualified Domain.Action.RiderPlatform.AppManagement.Payment
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.RefundRequest
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

type API = ("payment" :> (GetPaymentRefundRequestList :<|> GetPaymentRefundRequestInfo :<|> PostPaymentRefundRequestRespond))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPaymentRefundRequestList merchantId city :<|> getPaymentRefundRequestInfo merchantId city :<|> postPaymentRefundRequestRespond merchantId city

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

getPaymentRefundRequestList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestStatus -> Kernel.Prelude.Maybe Domain.Types.RefundRequest.RefundRequestCode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestResp)
getPaymentRefundRequestList merchantShortId opCity apiTokenInfo limit offset status code customerId orderId from to = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Payment.getPaymentRefundRequestList merchantShortId opCity apiTokenInfo limit offset status code customerId orderId from to

getPaymentRefundRequestInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestInfoResp)
getPaymentRefundRequestInfo merchantShortId opCity apiTokenInfo orderId refreshRefunds = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Payment.getPaymentRefundRequestInfo merchantShortId opCity apiTokenInfo orderId refreshRefunds

postPaymentRefundRequestRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Payment.RefundRequestRespondResp)
postPaymentRefundRequestRespond merchantShortId opCity apiTokenInfo orderId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Payment.postPaymentRefundRequestRespond merchantShortId opCity apiTokenInfo orderId req
