{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.Pricing
  ( getPricingSurgeList,
    postPricingSurgeCreate,
    postPricingSurgeUpdate,
    postPricingSurgeStatus,
    postPricingSurgePreview,
    getPricingObservabilityEstimate,
    getPricingObservabilityHealth,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Pricing
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
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

getPricingSurgeList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Dashboard.Common.ServiceTierType -> Environment.Flow API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigListRes)
getPricingSurgeList merchantShortId opCity apiTokenInfo serviceTier = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingDSL.getPricingSurgeList) serviceTier

-- author identity is stamped from the authenticated token
postPricingSurgeCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq -> Environment.Flow API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigRes)
postPricingSurgeCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let req' = req {API.Types.ProviderPlatform.Management.Pricing.createdBy = Kernel.Prelude.Just apiTokenInfo.personId.getId} :: API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req')
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingDSL.postPricingSurgeCreate) req')

postPricingSurgeUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPricingSurgeUpdate merchantShortId opCity apiTokenInfo surgeConfigId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingDSL.postPricingSurgeUpdate) surgeConfigId req)

postPricingSurgeStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPricingSurgeStatus merchantShortId opCity apiTokenInfo surgeConfigId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingDSL.postPricingSurgeStatus) surgeConfigId req)

postPricingSurgePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgePreviewReq -> Environment.Flow API.Types.ProviderPlatform.Management.Pricing.PricingSurgePreviewRes)
postPricingSurgePreview merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingDSL.postPricingSurgePreview) req

getPricingObservabilityEstimate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.Pricing.PricingEstimateExplainRes)
getPricingObservabilityEstimate merchantShortId opCity apiTokenInfo estimateId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingDSL.getPricingObservabilityEstimate) estimateId

getPricingObservabilityHealth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.Management.Pricing.PricingHealthRes)
getPricingObservabilityHealth merchantShortId opCity apiTokenInfo hours = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.pricingDSL.getPricingObservabilityHealth) hours
