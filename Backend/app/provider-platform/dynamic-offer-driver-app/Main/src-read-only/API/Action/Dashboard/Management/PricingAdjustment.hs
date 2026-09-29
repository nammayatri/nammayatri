{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.PricingAdjustment
  ( API.Types.ProviderPlatform.Management.PricingAdjustment.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.PricingAdjustment
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.PricingAdjustment
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.PricingAdjustment.API)
handler merchantId city = getPricingAdjustmentList merchantId city :<|> postPricingAdjustmentCreate merchantId city :<|> postPricingAdjustmentUpdate merchantId city :<|> postPricingAdjustmentStatus merchantId city :<|> postPricingAdjustmentPreview merchantId city :<|> getPricingAdjustmentResults merchantId city

getPricingAdjustmentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentListRes)
getPricingAdjustmentList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PricingAdjustment.getPricingAdjustmentList a2 a1

postPricingAdjustmentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentRes)
postPricingAdjustmentCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PricingAdjustment.postPricingAdjustmentCreate a3 a2 a1

postPricingAdjustmentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingAdjustmentUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PricingAdjustment.postPricingAdjustmentUpdate a4 a3 a2 a1

postPricingAdjustmentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingAdjustmentStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PricingAdjustment.postPricingAdjustmentStatus a4 a3 a2 a1

postPricingAdjustmentPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentPreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentPreviewRes)
postPricingAdjustmentPreview a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PricingAdjustment.postPricingAdjustmentPreview a3 a2 a1

getPricingAdjustmentResults :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentResultsRes)
getPricingAdjustmentResults a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.PricingAdjustment.getPricingAdjustmentResults a3 a2 a1
