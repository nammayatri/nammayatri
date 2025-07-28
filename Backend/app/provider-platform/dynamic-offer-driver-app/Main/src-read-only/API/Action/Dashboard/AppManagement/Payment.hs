{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Payment
  ( API.Types.Dashboard.AppManagement.Payment.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Payment
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.AppManagement.Payment as Domain.Action.Dashboard.AppManagement.Payment
import qualified "this" Domain.Action.UI.Payment
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Payment.API)
handler merchantId city = createPaymentOrder merchantId city :<|> getPaymentOrder merchantId city :<|> getPaymentOrderStatus merchantId city

createPaymentOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp)
createPaymentOrder a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.createPaymentOrder a4 a3 a2 a1

getPaymentOrder :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler Lib.Payment.Domain.Types.PaymentOrder.PaymentOrderAPIEntity)
getPaymentOrder a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentOrder a4 a3 a2 a1

getPaymentOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler Domain.Action.UI.Payment.PaymentStatusResp)
getPaymentOrderStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Payment.getPaymentOrderStatus a4 a3 a2 a1
