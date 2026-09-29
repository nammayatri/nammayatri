{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.PricingAdjustment
  ( getPricingAdjustmentList,
    postPricingAdjustmentCreate,
    postPricingAdjustmentUpdate,
    postPricingAdjustmentStatus,
    postPricingAdjustmentPreview,
    getPricingAdjustmentResults,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.PricingAdjustment
import qualified Dashboard.Common
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

getPricingAdjustmentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.Flow API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentListRes)
getPricingAdjustmentList merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingAdjustmentDSL.getPricingAdjustmentList)

-- author identity is stamped from the authenticated token
postPricingAdjustmentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq -> Environment.Flow API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentRes)
postPricingAdjustmentCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let req' = req {API.Types.ProviderPlatform.Management.PricingAdjustment.createdBy = Kernel.Prelude.Just apiTokenInfo.personId.getId} :: API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req')
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingAdjustmentDSL.postPricingAdjustmentCreate) req')

postPricingAdjustmentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPricingAdjustmentUpdate merchantShortId opCity apiTokenInfo adjustmentId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingAdjustmentDSL.postPricingAdjustmentUpdate) adjustmentId req)

postPricingAdjustmentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPricingAdjustmentStatus merchantShortId opCity apiTokenInfo adjustmentId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingAdjustmentDSL.postPricingAdjustmentStatus) adjustmentId req)

postPricingAdjustmentPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentPreviewReq -> Environment.Flow API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentPreviewRes)
postPricingAdjustmentPreview merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingAdjustmentDSL.postPricingAdjustmentPreview) req

getPricingAdjustmentResults :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> Environment.Flow API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentResultsRes)
getPricingAdjustmentResults merchantShortId opCity apiTokenInfo adjustmentId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingAdjustmentDSL.getPricingAdjustmentResults) adjustmentId
