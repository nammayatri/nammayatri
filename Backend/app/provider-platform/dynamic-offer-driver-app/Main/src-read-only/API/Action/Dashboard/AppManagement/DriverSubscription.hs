{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.DriverSubscription
  ( API.Types.Dashboard.AppManagement.DriverSubscription.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.DriverSubscription
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.AppManagement.DriverSubscription
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.DriverSubscription.API)
handler merchantId city = postDriverSubscriptionSendSms merchantId city :<|> postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo merchantId city

postDriverSubscriptionSendSms :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> API.Types.Dashboard.AppManagement.DriverSubscription.SendSmsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSubscriptionSendSms a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverSubscription.postDriverSubscriptionSendSms a5 a4 a3 a2 a1

postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.ServiceNames -> API.Types.Dashboard.AppManagement.DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate)
postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverSubscription.postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo a5 a4 a3 a2 a1
