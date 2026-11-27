{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for the plan and subscription routes of direct-dashboard. They name a driver, or the
-- fleet itself, in the path. Once fleet scope is enforced the subject is checked against the caller's scope here
-- ('SharedLogic.FleetScope.subjectRequestor') and the shared function is told there is no requestor, so the fleet
-- member association is not consulted; otherwise the dashboard person is the requestor, as before.
module Domain.Action.DashboardAuth.AppManagement.Subscription
  ( getSubscriptionListPlan,
    putSubscriptionSelectPlan,
    putSubscriptionSuspendPlan,
    postSubscriptionSubscribePlan,
    getSubscriptionCurrentPlan,
    getSubscriptionListPlanV2,
    putSubscriptionSelectPlanV2,
    putSubscriptionSuspendPlanV2,
    postSubscriptionSubscribePlanV2,
    getSubscriptionCurrentPlanV2,
    getSubscriptionOrderStatus,
    getSubscriptionDriverPaymentHistoryAPIV2,
    getSubscriptionDriverPaymentHistoryEntityDetailsV2,
    getSubscriptionCancellationChargeHistory,
    postSubscriptionCollectManualPayments,
    getSubscriptionPurchaseList,
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
import qualified SharedLogic.FleetScope
import Tools.Auth
import Tools.Auth.DashboardUserAuth

getSubscriptionListPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.Flow Domain.Action.UI.Plan.PlanListAPIRes)
getSubscriptionListPlan a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a2 a1.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionListPlan a4 a3 a1 mbRequestorId

putSubscriptionSelectPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putSubscriptionSelectPlan a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a3 a2.getId
  Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSelectPlan a5 a4 a2 a1 mbRequestorId

putSubscriptionSuspendPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putSubscriptionSuspendPlan a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a2 a1.getId
  Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSuspendPlan a4 a3 a1 mbRequestorId

postSubscriptionSubscribePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.Flow Domain.Action.UI.Plan.PlanSubscribeRes)
postSubscriptionSubscribePlan a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a3 a2.getId
  Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionSubscribePlan a5 a4 a2 a1 mbRequestorId

getSubscriptionCurrentPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Environment.Flow Domain.Action.UI.Plan.CurrentPlanRes)
getSubscriptionCurrentPlan a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a2 a1.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionCurrentPlan a4 a3 a1 mbRequestorId

getSubscriptionListPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.Flow Domain.Action.UI.Plan.PlanListAPIRes)
getSubscriptionListPlanV2 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a3 a2.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionListPlanV2 a5 a4 a2 a1 mbRequestorId

putSubscriptionSelectPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putSubscriptionSelectPlanV2 a6 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a4 a3.getId
  Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSelectPlanV2 a6 a5 a3 a2 a1 mbRequestorId

putSubscriptionSuspendPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putSubscriptionSuspendPlanV2 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a3 a2.getId
  Domain.Action.Dashboard.AppManagement.Subscription.putSubscriptionSuspendPlanV2 a5 a4 a2 a1 mbRequestorId

postSubscriptionSubscribePlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Domain.Types.Plan.ServiceNames -> API.Types.Dashboard.AppManagement.Subscription.PlanSubscribeReq -> Environment.Flow Domain.Action.UI.Plan.PlanSubscribeRes)
postSubscriptionSubscribePlanV2 a7 a6 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a5 a4.getId
  Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionSubscribePlanV2 a7 a6 a4 a3 a2 mbRequestorId a1

getSubscriptionCurrentPlanV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Environment.Flow Domain.Action.UI.Plan.CurrentPlanRes)
getSubscriptionCurrentPlanV2 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a3 a2.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionCurrentPlanV2 a5 a4 a2 a1 mbRequestorId

getSubscriptionOrderStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Environment.Flow Domain.Action.UI.Payment.PaymentStatusResp)
getSubscriptionOrderStatus a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a3 a2.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionOrderStatus a5 a4 a2 a1 mbRequestorId

getSubscriptionDriverPaymentHistoryAPIV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoicePaymentMode -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow Domain.Action.UI.Driver.HistoryEntityV2)
getSubscriptionDriverPaymentHistoryAPIV2 a8 a7 a6 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a6 a5.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionDriverPaymentHistoryAPIV2 a8 a7 a5 a4 a3 a2 a1 mbRequestorId

getSubscriptionDriverPaymentHistoryEntityDetailsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Types.Id.Id Domain.Types.Invoice.Invoice -> Environment.Flow Domain.Action.UI.Driver.HistoryEntryDetailsEntityV2)
getSubscriptionDriverPaymentHistoryEntityDetailsV2 a6 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a4 a3.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionDriverPaymentHistoryEntityDetailsV2 a6 a5 a3 a2 a1 mbRequestorId

getSubscriptionCancellationChargeHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow Domain.Action.UI.Plan.CancellationChargeHistoryRes)
getSubscriptionCancellationChargeHistory a7 a6 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a5 a4.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionCancellationChargeHistory a7 a6 a4 a3 a2 a1 mbRequestorId

postSubscriptionCollectManualPayments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Domain.Types.Plan.ServiceNames -> API.Types.Dashboard.AppManagement.Subscription.CollectManualPaymentsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSubscriptionCollectManualPayments a6 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a4 a3.getId
  Domain.Action.Dashboard.AppManagement.Subscription.postSubscriptionCollectManualPayments a6 a5 a3 a2 mbRequestorId a1

getSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.SubscriptionPurchase.SubscriptionPurchaseStatus -> Environment.Flow Domain.Action.UI.Plan.SubscriptionPurchaseListRes)
getSubscriptionPurchaseList a7 a6 a5 a4 a3 a2 a1 = do
  mbRequestorId <- SharedLogic.FleetScope.subjectRequestor a5 a4.getId
  Domain.Action.Dashboard.AppManagement.Subscription.getSubscriptionPurchaseList a7 a6 a4 a3 a2 a1 mbRequestorId
