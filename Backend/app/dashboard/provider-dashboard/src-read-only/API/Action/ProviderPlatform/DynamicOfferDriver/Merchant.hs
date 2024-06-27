{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.DynamicOfferDriver.Merchant
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Merchant
import qualified Dashboard.Common.Merchant
import qualified Dashboard.ProviderPlatform.Merchant
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Validation
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import Servant
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

type API = ("merchant" :> (PostMerchantUpdate :<|> PostMerchantServiceConfigMapsUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMerchantUpdate merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city

type PostMerchantUpdate = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'MERCHANT_UPDATE :> API.Types.ProviderPlatform.Merchant.PostMerchantUpdate)

type PostMerchantServiceConfigMapsUpdate = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MERCHANT 'MAPS_SERVICE_CONFIG_UPDATE :> API.Types.ProviderPlatform.Merchant.PostMerchantServiceConfigMapsUpdate)

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Merchant.MerchantUpdateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Merchant.MerchantUpdateRes)
postMerchantUpdate merchantShortId opCity apiTokenInfo req =
  withFlowHandlerAPI' $
    ( do
        Kernel.Utils.Validation.runRequestValidation Dashboard.ProviderPlatform.Merchant.validateMerchantUpdateReq req
        checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
        transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
        SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantUpdate) req)
    )

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate merchantShortId opCity apiTokenInfo req =
  withFlowHandlerAPI' $
    ( do
        checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
        transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantServiceConfigMapsUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
        SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantServiceConfigMapsUpdate) req)
    )
