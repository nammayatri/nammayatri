{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.DriverSubscription
  ( API,
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
import Tools.Auth.DashboardUserAuth

-- provider-dashboard mounts this folder under "driver"; "plan" is the internal prefix.
type API = ("driver" :> (PostDriverSubscriptionSendSms :<|> PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo))

type PostDriverSubscriptionSendSms =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_SEND_SMS"
      :> API.Types.Dashboard.AppManagement.DriverSubscription.PostDriverSubscriptionSendSms
  )

type PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_UPDATE_DRIVER_FEE_AND_INVOICE_INFO"
      :> API.Types.Dashboard.AppManagement.DriverSubscription.PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverSubscriptionSendSms merchantId city :<|> postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo merchantId city

postDriverSubscriptionSendSms :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.Dashboard.AppManagement.DriverSubscription.SendSmsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSubscriptionSendSms a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverSubscription.postDriverSubscriptionSendSms a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1

postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.ServiceNames -> API.Types.Dashboard.AppManagement.DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate)
postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DriverSubscription.postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo a6 a5 a3 a2 a1
