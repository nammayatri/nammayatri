{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.PlanManagement
  ( API.Types.ProviderPlatform.Management.PlanManagement.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.PlanManagement.API)
handler merchantId city = postPlanManagementCreate merchantId city :<|> postPlanManagementDeletePlan merchantId city :<|> getPlanManagementListPlans merchantId city

postPlanManagementCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.PlanManagement.CreatePlanReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PlanManagement.CreatePlanResp)
postPlanManagementCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PlanManagement.postPlanManagementCreate a3 a2 a1

postPlanManagementDeletePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPlanManagementDeletePlan a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PlanManagement.postPlanManagementDeletePlan a3 a2 a1

getPlanManagementListPlans :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PlanManagement.ListPlansResp)
getPlanManagementListPlans a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PlanManagement.getPlanManagementListPlans a3 a2 a1
