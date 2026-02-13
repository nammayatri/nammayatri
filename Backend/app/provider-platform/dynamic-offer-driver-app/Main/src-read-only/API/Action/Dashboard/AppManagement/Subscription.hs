{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Subscription
  ( API.Types.Dashboard.AppManagement.Subscription.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Subscription
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Domain.Action.Dashboard.AppManagement.Subscription
import qualified Domain.Action.UI.Driver
import qualified "this" Domain.Action.UI.Payment
import qualified "this" Domain.Action.UI.Plan
import qualified Domain.Types.Invoice
import qualified Domain.Types.Merchant
import qualified Domain.Types.Plan
import qualified "this" Domain.Types.SubscriptionPurchase
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Subscription.API)
handler merchantId city = getSubscriptionListPlan merchantId city :<|> putSubscriptionSelectPlan merchantId city :<|> putSubscriptionSuspendPlan merchantId city :<|> postSubscriptionSubscribePlan merchantId city :<|> getSubscriptionCurrentPlan merchantId city :<|> getSubscriptionListPlanV2 merchantId city :<|> putSubscriptionSelectPlanV2 merchantId city :<|> putSubscriptionSuspendPlanV2 merchantId city :<|> postSubscriptionSubscribePlanV2 merchantId city :<|> getSubscriptionCurrentPlanV2 merchantId city :<|> getSubscriptionOrderStatus merchantId city :<|> getSubscriptionDriverPaymentHistoryAPIV2 merchantId city :<|> getSubscriptionDriverPaymentHistoryEntityDetailsV2 merchantId city :<|> postSubscriptionCollectManualPayments merchantId city :<|> postSubscriptionFeeWaiveOff merchantId city :<|> getSubscriptionPurchaseList merchantId city

getSubscriptionListPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Domain.Action.UI.Plan.PlanListAPIRes)
getSubscriptionListPlan a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionListPlan a3 a2 a1

putSubscriptionSelectPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSelectPlan a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSelectPlan a4 a3 a2 a1

putSubscriptionSuspendPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSuspendPlan a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSuspendPlan a3 a2 a1

postSubscriptionSubscribePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postSubscriptionSubscribePlan a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionSubscribePlan a4 a3 a2 a1

getSubscriptionCurrentPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Domain.Action.UI.Plan.CurrentPlanRes)
getSubscriptionCurrentPlan a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionCurrentPlan a3 a2 a1

getSubscriptionListPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Domain.Action.UI.Plan.PlanListAPIRes)
getSubscriptionListPlanV2 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionListPlanV2 a4 a3 a2 a1

putSubscriptionSelectPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSelectPlanV2 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSelectPlanV2 a5 a4 a3 a2 a1

putSubscriptionSuspendPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSuspendPlanV2 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSuspendPlanV2 a4 a3 a2 a1

postSubscriptionSubscribePlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> API.Types.Dashboard.AppManagement.Subscription.PlanSubscribeReq -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postSubscriptionSubscribePlanV2 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionSubscribePlanV2 a6 a5 a4 a3 a2 a1

getSubscriptionCurrentPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Domain.Action.UI.Plan.CurrentPlanRes)
getSubscriptionCurrentPlanV2 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionCurrentPlanV2 a4 a3 a2 a1

getSubscriptionOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Environment.FlowHandler Domain.Action.UI.Payment.PaymentStatusResp)
getSubscriptionOrderStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionOrderStatus a4 a3 a2 a1

getSubscriptionDriverPaymentHistoryAPIV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoicePaymentMode -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler Domain.Action.UI.Driver.HistoryEntityV2)
getSubscriptionDriverPaymentHistoryAPIV2 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionDriverPaymentHistoryAPIV2 a7 a6 a5 a4 a3 a2 a1

getSubscriptionDriverPaymentHistoryEntityDetailsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Environment.FlowHandler Domain.Action.UI.Driver.HistoryEntryDetailsEntityV2)
getSubscriptionDriverPaymentHistoryEntityDetailsV2 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionDriverPaymentHistoryEntityDetailsV2 a5 a4 a3 a2 a1

postSubscriptionCollectManualPayments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> API.Types.Dashboard.AppManagement.Subscription.CollectManualPaymentsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSubscriptionCollectManualPayments a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionCollectManualPayments a5 a4 a3 a2 a1

postSubscriptionFeeWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Subscription.WaiveOffReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSubscriptionFeeWaiveOff a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionFeeWaiveOff a3 a2 a1

getSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus -> Environment.FlowHandler Domain.Action.UI.Plan.SubscriptionPurchaseListRes)
getSubscriptionPurchaseList a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionPurchaseList a6 a5 a4 a3 a2 a1
