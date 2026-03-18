{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Dashboard.Management.SubscriptionPlan
  ( Dashboard.ProviderPlatform.Management.SubscriptionPlan.API,
    handler,
  )
where

import qualified Dashboard.ProviderPlatform.Management.SubscriptionPlan as SubscriptionPlan
import qualified Domain.Action.Dashboard.Management.SubscriptionPlan as DSubscriptionPlan
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

handler ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.FlowServer SubscriptionPlan.API
handler merchantId city =
  getSubscriptionPlanList merchantId city
    :<|> getSubscriptionPlanDetails merchantId city
    :<|> postSubscriptionPlanCreate merchantId city
    :<|> postSubscriptionPlanUpdate merchantId city
    :<|> postSubscriptionPlanDeactivate merchantId city
    :<|> postSubscriptionPlanActivate merchantId city
    :<|> getSubscriptionPlanAnalytics merchantId city

getSubscriptionPlanList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Kernel.Prelude.Text ->
  Maybe Kernel.Prelude.Text ->
  Maybe Kernel.Prelude.Text ->
  Maybe Kernel.Prelude.Int ->
  Maybe Kernel.Prelude.Int ->
  Environment.FlowHandler SubscriptionPlan.SubscriptionPlanListResp
getSubscriptionPlanList a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $ DSubscriptionPlan.getSubscriptionPlanList a7 a6 a5 a4 a3 a2 a1

getSubscriptionPlanDetails ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.FlowHandler SubscriptionPlan.SubscriptionPlanDetailsResp
getSubscriptionPlanDetails a3 a2 a1 =
  withDashboardFlowHandlerAPI $ DSubscriptionPlan.getSubscriptionPlanDetails a3 a2 a1

postSubscriptionPlanCreate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  SubscriptionPlan.CreateSubscriptionPlanReq ->
  Environment.FlowHandler SubscriptionPlan.CreateSubscriptionPlanResp
postSubscriptionPlanCreate a3 a2 a1 =
  withDashboardFlowHandlerAPI $ DSubscriptionPlan.postSubscriptionPlanCreate a3 a2 a1

postSubscriptionPlanUpdate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  SubscriptionPlan.UpdateSubscriptionPlanReq ->
  Environment.FlowHandler SubscriptionPlan.CreateSubscriptionPlanResp
postSubscriptionPlanUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $ DSubscriptionPlan.postSubscriptionPlanUpdate a4 a3 a2 a1

postSubscriptionPlanDeactivate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  SubscriptionPlan.DeactivatePlanReq ->
  Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postSubscriptionPlanDeactivate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $ DSubscriptionPlan.postSubscriptionPlanDeactivate a4 a3 a2 a1

postSubscriptionPlanActivate ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  SubscriptionPlan.ActivatePlanReq ->
  Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postSubscriptionPlanActivate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $ DSubscriptionPlan.postSubscriptionPlanActivate a4 a3 a2 a1

getSubscriptionPlanAnalytics ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.FlowHandler SubscriptionPlan.PlanAnalyticsResp
getSubscriptionPlanAnalytics a3 a2 a1 =
  withDashboardFlowHandlerAPI $ DSubscriptionPlan.getSubscriptionPlanAnalytics a3 a2 a1
