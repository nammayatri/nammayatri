{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.PlanManagement
  ( postPlanManagementCreate,
    getPlanManagementPlan,
    postPlanManagementDeletePlan,
    getPlanManagementListPlans,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Management.PlanManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Plan
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postPlanManagementCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Domain.Action.Dashboard.Management.PlanManagement.CreatePlanReq -> Environment.Flow Domain.Action.Dashboard.Management.PlanManagement.CreatePlanResp)
postPlanManagementCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.postPlanManagementCreate) req)

getPlanManagementPlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.Flow Domain.Types.Plan.Plan)
getPlanManagementPlan merchantShortId opCity apiTokenInfo planId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.getPlanManagementPlan) planId

postPlanManagementDeletePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPlanManagementDeletePlan merchantShortId opCity apiTokenInfo planId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.postPlanManagementDeletePlan) planId)

getPlanManagementListPlans :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Domain.Types.Plan.ServiceNames) -> Environment.Flow Domain.Action.Dashboard.Management.PlanManagement.ListPlansResp)
getPlanManagementListPlans merchantShortId opCity apiTokenInfo serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.getPlanManagementListPlans) serviceName
