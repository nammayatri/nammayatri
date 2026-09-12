{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.PlanManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.PlanManagement
import qualified Domain.Action.Dashboard.Management.PlanManagement
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

type API = ("planManagement" :> (PostPlanManagementCreate :<|> PostPlanManagementDeletePlan :<|> PostPlanManagementActivatePlan :<|> GetPlanManagementListPlans :<|> GetPlanManagementPlanTranslations))

type PostPlanManagementCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/POST_PLAN_MANAGEMENT_CREATE"
      :> API.Types.ProviderPlatform.Management.PlanManagement.PostPlanManagementCreate
  )

type PostPlanManagementDeletePlan =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/POST_PLAN_MANAGEMENT_DELETE_PLAN"
      :> API.Types.ProviderPlatform.Management.PlanManagement.PostPlanManagementDeletePlan
  )

type PostPlanManagementActivatePlan =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/POST_PLAN_MANAGEMENT_ACTIVATE_PLAN"
      :> API.Types.ProviderPlatform.Management.PlanManagement.PostPlanManagementActivatePlan
  )

type GetPlanManagementListPlans =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/GET_PLAN_MANAGEMENT_LIST_PLANS"
      :> API.Types.ProviderPlatform.Management.PlanManagement.GetPlanManagementListPlans
  )

type GetPlanManagementPlanTranslations =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/GET_PLAN_MANAGEMENT_PLAN_TRANSLATIONS"
      :> API.Types.ProviderPlatform.Management.PlanManagement.GetPlanManagementPlanTranslations
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPlanManagementCreate merchantId city :<|> postPlanManagementDeletePlan merchantId city :<|> postPlanManagementActivatePlan merchantId city :<|> getPlanManagementListPlans merchantId city :<|> getPlanManagementPlanTranslations merchantId city

postPlanManagementCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.PlanManagement.CreatePlanReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PlanManagement.CreatePlanResp)
postPlanManagementCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PlanManagement.postPlanManagementCreate a4 a3 a1

postPlanManagementDeletePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPlanManagementDeletePlan a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PlanManagement.postPlanManagementDeletePlan a4 a3 a1

postPlanManagementActivatePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPlanManagementActivatePlan a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PlanManagement.postPlanManagementActivatePlan a4 a3 a1

getPlanManagementListPlans :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PlanManagement.ListPlansResp)
getPlanManagementListPlans a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PlanManagement.getPlanManagementListPlans a4 a3 a1

getPlanManagementPlanTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.PlanManagement.PlanTranslationAPIEntity])
getPlanManagementPlanTranslations a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PlanManagement.getPlanManagementPlanTranslations a4 a3 a1
