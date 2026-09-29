{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.PricingAdjustment
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.PricingAdjustment
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.PricingAdjustment
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("pricingAdjustment" :> (GetPricingAdjustmentList :<|> PostPricingAdjustmentCreate :<|> PostPricingAdjustmentUpdate :<|> PostPricingAdjustmentStatus :<|> PostPricingAdjustmentPreview :<|> GetPricingAdjustmentResults))

type GetPricingAdjustmentList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/GET_PRICING_ADJUSTMENT_LIST"
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.GetPricingAdjustmentList
  )

type PostPricingAdjustmentCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_CREATE"
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.PostPricingAdjustmentCreate
  )

type PostPricingAdjustmentUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_UPDATE"
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.PostPricingAdjustmentUpdate
  )

type PostPricingAdjustmentStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_STATUS"
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.PostPricingAdjustmentStatus
  )

type PostPricingAdjustmentPreview =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_PREVIEW"
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.PostPricingAdjustmentPreview
  )

type GetPricingAdjustmentResults =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/GET_PRICING_ADJUSTMENT_RESULTS"
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.GetPricingAdjustmentResults
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPricingAdjustmentList merchantId city :<|> postPricingAdjustmentCreate merchantId city :<|> postPricingAdjustmentUpdate merchantId city :<|> postPricingAdjustmentStatus merchantId city :<|> postPricingAdjustmentPreview merchantId city :<|> getPricingAdjustmentResults merchantId city

getPricingAdjustmentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentListRes)
getPricingAdjustmentList a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.Management.PricingAdjustment.getPricingAdjustmentList a3 a2

postPricingAdjustmentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentRes)
postPricingAdjustmentCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_CREATE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.PricingAdjustment.postPricingAdjustmentCreate a4 a3 a1
    )

postPricingAdjustmentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingAdjustmentUpdate a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_UPDATE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.PricingAdjustment.postPricingAdjustmentUpdate a5 a4 a2 a1
    )

postPricingAdjustmentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingAdjustmentStatus a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_STATUS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.Management.PricingAdjustment.postPricingAdjustmentStatus a5 a4 a2 a1
    )

postPricingAdjustmentPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentPreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentPreviewRes)
postPricingAdjustmentPreview a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/PRICING_ADJUSTMENT/POST_PRICING_ADJUSTMENT_PREVIEW" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.PricingAdjustment.postPricingAdjustmentPreview a4 a3 a1
    )

getPricingAdjustmentResults :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentResultsRes)
getPricingAdjustmentResults a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.PricingAdjustment.getPricingAdjustmentResults a4 a3 a1
