{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Management.DomainDiscountConfig 
( API.Types.ProviderPlatform.Management.DomainDiscountConfig.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Management.DomainDiscountConfig
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified API.Types.ProviderPlatform.Management.DomainDiscountConfig
import qualified Kernel.Types.APISuccess



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.DomainDiscountConfig.API)
handler merchantId city = postDomainDiscountConfigCreate merchantId city :<|> getDomainDiscountConfigList merchantId city :<|> deleteDomainDiscountConfigDelete merchantId city
postDomainDiscountConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.CreateDomainDiscountConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDomainDiscountConfigCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DomainDiscountConfig.postDomainDiscountConfigCreate a3 a2 a1
getDomainDiscountConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.BillingCategory -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.DomainDiscountConfig.DomainDiscountConfigRes])
getDomainDiscountConfigList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DomainDiscountConfig.getDomainDiscountConfigList a3 a2 a1
deleteDomainDiscountConfigDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.DeleteDomainDiscountConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDomainDiscountConfigDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DomainDiscountConfig.deleteDomainDiscountConfigDelete a3 a2 a1



