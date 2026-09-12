{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.DomainDiscountConfig
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DomainDiscountConfig
import qualified Domain.Action.Dashboard.Management.DomainDiscountConfig
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("domainDiscountConfig" :> (PostDomainDiscountConfigCreate :<|> GetDomainDiscountConfigList :<|> DeleteDomainDiscountConfigDelete))

type PostDomainDiscountConfigCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/POST_DOMAIN_DISCOUNT_CONFIG_CREATE"
      :> API.Types.ProviderPlatform.Management.DomainDiscountConfig.PostDomainDiscountConfigCreate
  )

type GetDomainDiscountConfigList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/GET_DOMAIN_DISCOUNT_CONFIG_LIST"
      :> API.Types.ProviderPlatform.Management.DomainDiscountConfig.GetDomainDiscountConfigList
  )

type DeleteDomainDiscountConfigDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/DELETE_DOMAIN_DISCOUNT_CONFIG_DELETE"
      :> API.Types.ProviderPlatform.Management.DomainDiscountConfig.DeleteDomainDiscountConfigDelete
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDomainDiscountConfigCreate merchantId city :<|> getDomainDiscountConfigList merchantId city :<|> deleteDomainDiscountConfigDelete merchantId city

postDomainDiscountConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.CreateDomainDiscountConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDomainDiscountConfigCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DomainDiscountConfig.postDomainDiscountConfigCreate a4 a3 a1

getDomainDiscountConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.BillingCategory -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.DomainDiscountConfig.DomainDiscountConfigRes])
getDomainDiscountConfigList a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DomainDiscountConfig.getDomainDiscountConfigList a4 a3 a1

deleteDomainDiscountConfigDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.DeleteDomainDiscountConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDomainDiscountConfigDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DomainDiscountConfig.deleteDomainDiscountConfigDelete a4 a3 a1
