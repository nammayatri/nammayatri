{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.PlanManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.PlanManagement
import qualified Domain.Action.ProviderPlatform.Management.PlanManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("planManagement" :> (PostPlanManagementCreate :<|> PostPlanManagementDeletePlan :<|> GetPlanManagementListPlans))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPlanManagementCreate merchantId city :<|> postPlanManagementDeletePlan merchantId city :<|> getPlanManagementListPlans merchantId city

type PostPlanManagementCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PLAN_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PlanManagement.POST_PLAN_MANAGEMENT_CREATE))
      :> API.Types.ProviderPlatform.Management.PlanManagement.PostPlanManagementCreate
  )

type PostPlanManagementDeletePlan =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PLAN_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PlanManagement.POST_PLAN_MANAGEMENT_DELETE_PLAN))
      :> API.Types.ProviderPlatform.Management.PlanManagement.PostPlanManagementDeletePlan
  )

type GetPlanManagementListPlans =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PLAN_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PlanManagement.GET_PLAN_MANAGEMENT_LIST_PLANS))
      :> API.Types.ProviderPlatform.Management.PlanManagement.GetPlanManagementListPlans
  )

postPlanManagementCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.PlanManagement.CreatePlanReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PlanManagement.CreatePlanResp)
postPlanManagementCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PlanManagement.postPlanManagementCreate merchantShortId opCity apiTokenInfo req

postPlanManagementDeletePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPlanManagementDeletePlan merchantShortId opCity apiTokenInfo planId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PlanManagement.postPlanManagementDeletePlan merchantShortId opCity apiTokenInfo planId

getPlanManagementListPlans :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PlanManagement.ListPlansResp)
getPlanManagementListPlans merchantShortId opCity apiTokenInfo serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PlanManagement.getPlanManagementListPlans merchantShortId opCity apiTokenInfo serviceName
