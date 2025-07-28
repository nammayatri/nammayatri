{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.Payment
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Payment
import qualified Domain.Action.ProviderPlatform.AppManagement.Payment
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Payment
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("payment" :> (CreatePaymentOrder :<|> GetPaymentOrder :<|> GetPaymentOrderStatus))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = createPaymentOrder merchantId city :<|> getPaymentOrder merchantId city :<|> getPaymentOrderStatus merchantId city

type CreatePaymentOrder =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PAYMENT / 'API.Types.Dashboard.AppManagement.Payment.CREATE_PAYMENT_ORDER)
      :> API.Types.Dashboard.AppManagement.Payment.CreatePaymentOrder
  )

type GetPaymentOrder =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PAYMENT / 'API.Types.Dashboard.AppManagement.Payment.GET_PAYMENT_ORDER)
      :> API.Types.Dashboard.AppManagement.Payment.GetPaymentOrder
  )

type GetPaymentOrderStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PAYMENT / 'API.Types.Dashboard.AppManagement.Payment.GET_PAYMENT_ORDER_STATUS)
      :> API.Types.Dashboard.AppManagement.Payment.GetPaymentOrderStatus
  )

createPaymentOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp)
createPaymentOrder merchantShortId opCity apiTokenInfo invoiceId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Payment.createPaymentOrder merchantShortId opCity apiTokenInfo invoiceId

getPaymentOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Lib.Payment.Domain.Types.PaymentOrder.PaymentOrderAPIEntity)
getPaymentOrder merchantShortId opCity apiTokenInfo orderId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Payment.getPaymentOrder merchantShortId opCity apiTokenInfo orderId

getPaymentOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Domain.Action.UI.Payment.PaymentStatusResp)
getPaymentOrderStatus merchantShortId opCity apiTokenInfo orderId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Payment.getPaymentOrderStatus merchantShortId opCity apiTokenInfo orderId
