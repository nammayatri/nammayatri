{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Subscription
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Subscription
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Domain.Action.Dashboard.AppManagement.Subscription
import qualified Domain.Action.UI.Driver
import qualified "this" Domain.Action.UI.Payment
import qualified "this" Domain.Action.UI.Plan
import qualified "this" Domain.Types.Invoice
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
import Tools.Auth.DashboardUserAuth

type API = ("plan" :> (GetSubscriptionListPlan :<|> PutSubscriptionSelectPlan :<|> PutSubscriptionSuspendPlan :<|> PostSubscriptionSubscribePlan :<|> GetSubscriptionCurrentPlan :<|> GetSubscriptionListPlanV2 :<|> PutSubscriptionSelectPlanV2 :<|> PutSubscriptionSuspendPlanV2 :<|> PostSubscriptionSubscribePlanV2 :<|> GetSubscriptionCurrentPlanV2 :<|> GetSubscriptionOrderStatus :<|> GetSubscriptionDriverPaymentHistoryAPIV2 :<|> GetSubscriptionDriverPaymentHistoryEntityDetailsV2 :<|> GetSubscriptionCancellationChargeHistory :<|> PostSubscriptionCollectManualPayments :<|> PostSubscriptionFeeWaiveOff :<|> GetSubscriptionPurchaseList))

type GetSubscriptionListPlan =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_LIST_PLAN"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionListPlan
  )

type PutSubscriptionSelectPlan =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN"
      :> API.Types.Dashboard.AppManagement.Subscription.PutSubscriptionSelectPlan
  )

type PutSubscriptionSuspendPlan =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN"
      :> API.Types.Dashboard.AppManagement.Subscription.PutSubscriptionSuspendPlan
  )

type PostSubscriptionSubscribePlan =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN"
      :> API.Types.Dashboard.AppManagement.Subscription.PostSubscriptionSubscribePlan
  )

type GetSubscriptionCurrentPlan =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_CURRENT_PLAN"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionCurrentPlan
  )

type GetSubscriptionListPlanV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_LIST_PLAN_V2"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionListPlanV2
  )

type PutSubscriptionSelectPlanV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN_V2"
      :> API.Types.Dashboard.AppManagement.Subscription.PutSubscriptionSelectPlanV2
  )

type PutSubscriptionSuspendPlanV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN_V2"
      :> API.Types.Dashboard.AppManagement.Subscription.PutSubscriptionSuspendPlanV2
  )

type PostSubscriptionSubscribePlanV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN_V2"
      :> API.Types.Dashboard.AppManagement.Subscription.PostSubscriptionSubscribePlanV2
  )

type GetSubscriptionCurrentPlanV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_CURRENT_PLAN_V2"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionCurrentPlanV2
  )

type GetSubscriptionOrderStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_ORDER_STATUS"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionOrderStatus
  )

type GetSubscriptionDriverPaymentHistoryAPIV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_API_V2"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionDriverPaymentHistoryAPIV2
  )

type GetSubscriptionDriverPaymentHistoryEntityDetailsV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_ENTITY_DETAILS_V2"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionDriverPaymentHistoryEntityDetailsV2
  )

type GetSubscriptionCancellationChargeHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_CANCELLATION_CHARGE_HISTORY"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionCancellationChargeHistory
  )

type PostSubscriptionCollectManualPayments =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_COLLECT_MANUAL_PAYMENTS"
      :> API.Types.Dashboard.AppManagement.Subscription.PostSubscriptionCollectManualPayments
  )

type PostSubscriptionFeeWaiveOff =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_FEE_WAIVE_OFF"
      :> API.Types.Dashboard.AppManagement.Subscription.PostSubscriptionFeeWaiveOff
  )

type GetSubscriptionPurchaseList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_PURCHASE_LIST"
      :> API.Types.Dashboard.AppManagement.Subscription.GetSubscriptionPurchaseList
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSubscriptionListPlan merchantId city :<|> putSubscriptionSelectPlan merchantId city :<|> putSubscriptionSuspendPlan merchantId city :<|> postSubscriptionSubscribePlan merchantId city :<|> getSubscriptionCurrentPlan merchantId city :<|> getSubscriptionListPlanV2 merchantId city :<|> putSubscriptionSelectPlanV2 merchantId city :<|> putSubscriptionSuspendPlanV2 merchantId city :<|> postSubscriptionSubscribePlanV2 merchantId city :<|> getSubscriptionCurrentPlanV2 merchantId city :<|> getSubscriptionOrderStatus merchantId city :<|> getSubscriptionDriverPaymentHistoryAPIV2 merchantId city :<|> getSubscriptionDriverPaymentHistoryEntityDetailsV2 merchantId city :<|> getSubscriptionCancellationChargeHistory merchantId city :<|> postSubscriptionCollectManualPayments merchantId city :<|> postSubscriptionFeeWaiveOff merchantId city :<|> getSubscriptionPurchaseList merchantId city

getSubscriptionListPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Domain.Action.UI.Plan.PlanListAPIRes)
getSubscriptionListPlan a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionListPlan a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

putSubscriptionSelectPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSelectPlan a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSelectPlan a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

putSubscriptionSuspendPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSuspendPlan a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSuspendPlan a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

postSubscriptionSubscribePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postSubscriptionSubscribePlan a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionSubscribePlan a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

getSubscriptionCurrentPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.FlowHandler Domain.Action.UI.Plan.CurrentPlanRes)
getSubscriptionCurrentPlan a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionCurrentPlan a4 a3 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2))

getSubscriptionListPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Domain.Action.UI.Plan.PlanListAPIRes)
getSubscriptionListPlanV2 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionListPlanV2 a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

putSubscriptionSelectPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSelectPlanV2 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSelectPlanV2 a6 a5 a3 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4))

putSubscriptionSuspendPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putSubscriptionSuspendPlanV2 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSuspendPlanV2 a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

postSubscriptionSubscribePlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> API.Types.Dashboard.AppManagement.Subscription.PlanSubscribeReq -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postSubscriptionSubscribePlanV2 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionSubscribePlanV2 a7 a6 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a1

getSubscriptionCurrentPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.FlowHandler Domain.Action.UI.Plan.CurrentPlanRes)
getSubscriptionCurrentPlanV2 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionCurrentPlanV2 a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

getSubscriptionOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Environment.FlowHandler Domain.Action.UI.Payment.PaymentStatusResp)
getSubscriptionOrderStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionOrderStatus a5 a4 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3))

getSubscriptionDriverPaymentHistoryAPIV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe (Domain.Types.Invoice.InvoicePaymentMode) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler Domain.Action.UI.Driver.HistoryEntityV2)
getSubscriptionDriverPaymentHistoryAPIV2 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionDriverPaymentHistoryAPIV2 a8 a7 a5 a4 a3 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6))

getSubscriptionDriverPaymentHistoryEntityDetailsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Environment.FlowHandler Domain.Action.UI.Driver.HistoryEntryDetailsEntityV2)
getSubscriptionDriverPaymentHistoryEntityDetailsV2 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionDriverPaymentHistoryEntityDetailsV2 a6 a5 a3 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4))

getSubscriptionCancellationChargeHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler Domain.Action.UI.Plan.CancellationChargeHistoryRes)
getSubscriptionCancellationChargeHistory a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionCancellationChargeHistory a7 a6 a4 a3 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5))

postSubscriptionCollectManualPayments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> API.Types.Dashboard.AppManagement.Subscription.CollectManualPaymentsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSubscriptionCollectManualPayments a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionCollectManualPayments a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1

postSubscriptionFeeWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.Subscription.WaiveOffReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSubscriptionFeeWaiveOff a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionFeeWaiveOff a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1

getSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus) -> Environment.FlowHandler Domain.Action.UI.Plan.SubscriptionPurchaseListRes)
getSubscriptionPurchaseList a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionPurchaseList a7 a6 a4 a3 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5))
