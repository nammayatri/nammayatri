{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.PricingAdjustment
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.PricingAdjustment
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.PricingAdjustment
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()

type API = ("pricingAdjustment" :> (GetPricingAdjustmentList :<|> PostPricingAdjustmentCreate :<|> PostPricingAdjustmentUpdate :<|> PostPricingAdjustmentStatus :<|> PostPricingAdjustmentPreview :<|> GetPricingAdjustmentResults))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPricingAdjustmentList merchantId city :<|> postPricingAdjustmentCreate merchantId city :<|> postPricingAdjustmentUpdate merchantId city :<|> postPricingAdjustmentStatus merchantId city :<|> postPricingAdjustmentPreview merchantId city :<|> getPricingAdjustmentResults merchantId city

type GetPricingAdjustmentList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PRICING_ADJUSTMENT) / ('API.Types.ProviderPlatform.Management.PricingAdjustment.GET_PRICING_ADJUSTMENT_LIST))
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.GetPricingAdjustmentList
  )

type PostPricingAdjustmentCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PRICING_ADJUSTMENT) / ('API.Types.ProviderPlatform.Management.PricingAdjustment.POST_PRICING_ADJUSTMENT_CREATE))
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.PostPricingAdjustmentCreate
  )

type PostPricingAdjustmentUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PRICING_ADJUSTMENT) / ('API.Types.ProviderPlatform.Management.PricingAdjustment.POST_PRICING_ADJUSTMENT_UPDATE))
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.PostPricingAdjustmentUpdate
  )

type PostPricingAdjustmentStatus =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PRICING_ADJUSTMENT) / ('API.Types.ProviderPlatform.Management.PricingAdjustment.POST_PRICING_ADJUSTMENT_STATUS))
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.PostPricingAdjustmentStatus
  )

type PostPricingAdjustmentPreview =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PRICING_ADJUSTMENT) / ('API.Types.ProviderPlatform.Management.PricingAdjustment.POST_PRICING_ADJUSTMENT_PREVIEW))
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.PostPricingAdjustmentPreview
  )

type GetPricingAdjustmentResults =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.PRICING_ADJUSTMENT) / ('API.Types.ProviderPlatform.Management.PricingAdjustment.GET_PRICING_ADJUSTMENT_RESULTS))
      :> API.Types.ProviderPlatform.Management.PricingAdjustment.GetPricingAdjustmentResults
  )

getPricingAdjustmentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentListRes)
getPricingAdjustmentList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PricingAdjustment.getPricingAdjustmentList merchantShortId opCity apiTokenInfo

postPricingAdjustmentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentRes)
postPricingAdjustmentCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PricingAdjustment.postPricingAdjustmentCreate merchantShortId opCity apiTokenInfo req

postPricingAdjustmentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingAdjustmentUpdate merchantShortId opCity apiTokenInfo adjustmentId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PricingAdjustment.postPricingAdjustmentUpdate merchantShortId opCity apiTokenInfo adjustmentId req

postPricingAdjustmentStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPricingAdjustmentStatus merchantShortId opCity apiTokenInfo adjustmentId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PricingAdjustment.postPricingAdjustmentStatus merchantShortId opCity apiTokenInfo adjustmentId req

postPricingAdjustmentPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentPreviewReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentPreviewRes)
postPricingAdjustmentPreview merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PricingAdjustment.postPricingAdjustmentPreview merchantShortId opCity apiTokenInfo req

getPricingAdjustmentResults :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> Environment.FlowHandler API.Types.ProviderPlatform.Management.PricingAdjustment.PricingAdjustmentResultsRes)
getPricingAdjustmentResults merchantShortId opCity apiTokenInfo adjustmentId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.PricingAdjustment.getPricingAdjustmentResults merchantShortId opCity apiTokenInfo adjustmentId
