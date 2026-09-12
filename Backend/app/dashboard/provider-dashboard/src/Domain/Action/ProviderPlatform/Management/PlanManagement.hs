{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.PlanManagement
  ( postPlanManagementCreate,
    postPlanManagementDeletePlan,
    postPlanManagementActivatePlan,
    getPlanManagementListPlans,
    getPlanManagementPlanTranslations,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified "dynamic-offer-driver-app" API.Types.ProviderPlatform.Management.Endpoints.PlanManagement as Common
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

postPlanManagementCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Common.CreatePlanReq -> Environment.Flow Common.CreatePlanResp)
postPlanManagementCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.postPlanManagementCreate) req)

postPlanManagementDeletePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPlanManagementDeletePlan merchantShortId opCity apiTokenInfo planId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.postPlanManagementDeletePlan) planId)

getPlanManagementListPlans :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Text -> Environment.Flow Common.ListPlansResp)
getPlanManagementListPlans merchantShortId opCity apiTokenInfo serviceName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.getPlanManagementListPlans) serviceName

postPlanManagementActivatePlan :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPlanManagementActivatePlan merchantShortId opCity apiTokenInfo planId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.postPlanManagementActivatePlan) planId)

getPlanManagementPlanTranslations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow [Common.PlanTranslationAPIEntity])
getPlanManagementPlanTranslations merchantShortId opCity apiTokenInfo planId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.planManagementDSL.getPlanManagementPlanTranslations) planId
