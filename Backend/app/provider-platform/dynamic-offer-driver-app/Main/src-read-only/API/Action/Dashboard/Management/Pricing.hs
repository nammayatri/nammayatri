{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Pricing
  ( API.Types.ProviderPlatform.Management.Pricing.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Pricing
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Pricing
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Pricing.API)
handler merchantId city = getPricingSurgeList merchantId city :<|> postPricingSurgeCreate merchantId city :<|> postPricingSurgeUpdate merchantId city :<|> postPricingSurgeStatus merchantId city :<|> postPricingSurgePreview merchantId city :<|> getPricingObservabilityEstimate merchantId city :<|> getPricingObservabilityCustomer merchantId city :<|> getPricingObservabilityHealth merchantId city

getPricingSurgeList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Dashboard.Common.ServiceTierType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigListRes)
getPricingSurgeList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.getPricingSurgeList a3 a2 a1

postPricingSurgeCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigRes)
postPricingSurgeCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.postPricingSurgeCreate a3 a2 a1

postPricingSurgeUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingSurgeUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.postPricingSurgeUpdate a4 a3 a2 a1

postPricingSurgeStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingSurgeStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.postPricingSurgeStatus a4 a3 a2 a1

postPricingSurgePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgePreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgePreviewRes)
postPricingSurgePreview a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.postPricingSurgePreview a3 a2 a1

getPricingObservabilityEstimate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingEstimateExplainRes)
getPricingObservabilityEstimate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.getPricingObservabilityEstimate a3 a2 a1

getPricingObservabilityCustomer :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingCustomerSearchRes)
getPricingObservabilityCustomer a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.getPricingObservabilityCustomer a3 a2 a1

getPricingObservabilityHealth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingHealthRes)
getPricingObservabilityHealth a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.getPricingObservabilityHealth a3 a2 a1
