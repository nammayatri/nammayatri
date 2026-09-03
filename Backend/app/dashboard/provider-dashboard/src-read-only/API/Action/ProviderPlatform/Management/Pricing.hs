{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Pricing
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Pricing
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.Pricing
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

type API = ("pricing" :> (GetPricingSurgeList :<|> PostPricingSurgeCreate :<|> PostPricingSurgeUpdate :<|> PostPricingSurgeStatus :<|> PostPricingSurgePreview :<|> GetPricingObservabilityEstimate :<|> GetPricingObservabilityCustomer :<|> GetPricingObservabilityHealth))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPricingSurgeList merchantId city :<|> postPricingSurgeCreate merchantId city :<|> postPricingSurgeUpdate merchantId city :<|> postPricingSurgeStatus merchantId city :<|> postPricingSurgePreview merchantId city :<|> getPricingObservabilityEstimate merchantId city :<|> getPricingObservabilityCustomer merchantId city :<|> getPricingObservabilityHealth merchantId city

type GetPricingSurgeList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PRICING / 'API.Types.ProviderPlatform.Management.Pricing.GET_PRICING_SURGE_LIST)
      :> API.Types.ProviderPlatform.Management.Pricing.GetPricingSurgeList
  )

type PostPricingSurgeCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PRICING / 'API.Types.ProviderPlatform.Management.Pricing.POST_PRICING_SURGE_CREATE)
      :> API.Types.ProviderPlatform.Management.Pricing.PostPricingSurgeCreate
  )

type PostPricingSurgeUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PRICING / 'API.Types.ProviderPlatform.Management.Pricing.POST_PRICING_SURGE_UPDATE)
      :> API.Types.ProviderPlatform.Management.Pricing.PostPricingSurgeUpdate
  )

type PostPricingSurgeStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PRICING / 'API.Types.ProviderPlatform.Management.Pricing.POST_PRICING_SURGE_STATUS)
      :> API.Types.ProviderPlatform.Management.Pricing.PostPricingSurgeStatus
  )

type PostPricingSurgePreview =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PRICING / 'API.Types.ProviderPlatform.Management.Pricing.POST_PRICING_SURGE_PREVIEW)
      :> API.Types.ProviderPlatform.Management.Pricing.PostPricingSurgePreview
  )

type GetPricingObservabilityEstimate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PRICING / 'API.Types.ProviderPlatform.Management.Pricing.GET_PRICING_OBSERVABILITY_ESTIMATE)
      :> API.Types.ProviderPlatform.Management.Pricing.GetPricingObservabilityEstimate
  )

type GetPricingObservabilityCustomer =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PRICING / 'API.Types.ProviderPlatform.Management.Pricing.GET_PRICING_OBSERVABILITY_CUSTOMER)
      :> API.Types.ProviderPlatform.Management.Pricing.GetPricingObservabilityCustomer
  )

type GetPricingObservabilityHealth =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.PRICING / 'API.Types.ProviderPlatform.Management.Pricing.GET_PRICING_OBSERVABILITY_HEALTH)
      :> API.Types.ProviderPlatform.Management.Pricing.GetPricingObservabilityHealth
  )

getPricingSurgeList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Dashboard.Common.ServiceTierType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigListRes)
getPricingSurgeList merchantShortId opCity apiTokenInfo serviceTier = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Pricing.getPricingSurgeList merchantShortId opCity apiTokenInfo serviceTier

postPricingSurgeCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigRes)
postPricingSurgeCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Pricing.postPricingSurgeCreate merchantShortId opCity apiTokenInfo req

postPricingSurgeUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingSurgeUpdate merchantShortId opCity apiTokenInfo surgeConfigId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Pricing.postPricingSurgeUpdate merchantShortId opCity apiTokenInfo surgeConfigId req

postPricingSurgeStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgeStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingSurgeStatus merchantShortId opCity apiTokenInfo surgeConfigId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Pricing.postPricingSurgeStatus merchantShortId opCity apiTokenInfo surgeConfigId req

postPricingSurgePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Pricing.PricingSurgePreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingSurgePreviewRes)
postPricingSurgePreview merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Pricing.postPricingSurgePreview merchantShortId opCity apiTokenInfo req

getPricingObservabilityEstimate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingEstimateExplainRes)
getPricingObservabilityEstimate merchantShortId opCity apiTokenInfo estimateId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Pricing.getPricingObservabilityEstimate merchantShortId opCity apiTokenInfo estimateId

getPricingObservabilityCustomer :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingCustomerSearchRes)
getPricingObservabilityCustomer merchantShortId opCity apiTokenInfo phone = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Pricing.getPricingObservabilityCustomer merchantShortId opCity apiTokenInfo phone

getPricingObservabilityHealth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Pricing.PricingHealthRes)
getPricingObservabilityHealth merchantShortId opCity apiTokenInfo hours = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Pricing.getPricingObservabilityHealth merchantShortId opCity apiTokenInfo hours
