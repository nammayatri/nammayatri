{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Pricing
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("pricing" :> (GetPricingSurgeList :<|> PostPricingSurgeCreate :<|> PostPricingSurgeUpdate :<|> PostPricingSurgeStatus :<|> PostPricingSurgePreview :<|> GetPricingObservabilityEstimate :<|> GetPricingObservabilityCustomer :<|> GetPricingObservabilityHealth))

type GetPricingSurgeList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/PRICING/GET_PRICING_SURGE_LIST" :> API.Types.ProviderPlatform.Management.Pricing.GetPricingSurgeList)

type PostPricingSurgeCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING/POST_PRICING_SURGE_CREATE"
      :> API.Types.ProviderPlatform.Management.Pricing.PostPricingSurgeCreate
  )

type PostPricingSurgeUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING/POST_PRICING_SURGE_UPDATE"
      :> API.Types.ProviderPlatform.Management.Pricing.PostPricingSurgeUpdate
  )

type PostPricingSurgeStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING/POST_PRICING_SURGE_STATUS"
      :> API.Types.ProviderPlatform.Management.Pricing.PostPricingSurgeStatus
  )

type PostPricingSurgePreview =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING/POST_PRICING_SURGE_PREVIEW"
      :> API.Types.ProviderPlatform.Management.Pricing.PostPricingSurgePreview
  )

type GetPricingObservabilityEstimate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING/GET_PRICING_OBSERVABILITY_ESTIMATE"
      :> API.Types.ProviderPlatform.Management.Pricing.GetPricingObservabilityEstimate
  )

type GetPricingObservabilityCustomer =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING/GET_PRICING_OBSERVABILITY_CUSTOMER"
      :> API.Types.ProviderPlatform.Management.Pricing.GetPricingObservabilityCustomer
  )

type GetPricingObservabilityHealth =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING/GET_PRICING_OBSERVABILITY_HEALTH"
      :> API.Types.ProviderPlatform.Management.Pricing.GetPricingObservabilityHealth
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPricingSurgeList merchantId city :<|> postPricingSurgeCreate merchantId city :<|> postPricingSurgeUpdate merchantId city :<|> postPricingSurgeStatus merchantId city :<|> postPricingSurgePreview merchantId city :<|> getPricingObservabilityEstimate merchantId city :<|> getPricingObservabilityCustomer merchantId city :<|> getPricingObservabilityHealth merchantId city

getPricingSurgeList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Dashboard.Common.ServiceTierType) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigListRes)
getPricingSurgeList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.getPricingSurgeList a4 a3 a1

postPricingSurgeCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigRes)
postPricingSurgeCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.postPricingSurgeCreate a4 a3 a1

postPricingSurgeUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingSurgeUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.postPricingSurgeUpdate a5 a4 a2 a1

postPricingSurgeStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingSurgeStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.postPricingSurgeStatus a5 a4 a2 a1

postPricingSurgePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgePreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgePreviewRes)
postPricingSurgePreview a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.postPricingSurgePreview a4 a3 a1

getPricingObservabilityEstimate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingEstimateExplainRes)
getPricingObservabilityEstimate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.getPricingObservabilityEstimate a4 a3 a1

getPricingObservabilityHealth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingHealthRes)
getPricingObservabilityHealth a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.getPricingObservabilityHealth a4 a3 a1

getPricingObservabilityCustomer :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingCustomerSearchRes)
getPricingObservabilityCustomer a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Pricing.getPricingObservabilityCustomer a4 a3 a1
