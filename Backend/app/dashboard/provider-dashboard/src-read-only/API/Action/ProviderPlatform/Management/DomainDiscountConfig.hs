{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.ProviderPlatform.Management.DomainDiscountConfig 
( API,
handler )
where
import EulerHS.Prelude hiding (sortOn)
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common hiding (INFO)
import Storage.Beam.CommonInstances ()
import qualified API.Types.ProviderPlatform.Management.DomainDiscountConfig
import qualified API.Types.ProviderPlatform.Management
import qualified Domain.Action.ProviderPlatform.Management.DomainDiscountConfig
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.APISuccess
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("domainDiscountConfig" :> (PostDomainDiscountConfigCreate :<|> GetDomainDiscountConfigList :<|> DeleteDomainDiscountConfigDelete))
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDomainDiscountConfigCreate merchantId city :<|> getDomainDiscountConfigList merchantId city :<|> deleteDomainDiscountConfigDelete merchantId city
type PostDomainDiscountConfigCreate = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                               ('DSL)
                                               (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.DOMAIN_DISCOUNT_CONFIG) / ('API.Types.ProviderPlatform.Management.DomainDiscountConfig.POST_DOMAIN_DISCOUNT_CONFIG_CREATE)) :> API.Types.ProviderPlatform.Management.DomainDiscountConfig.PostDomainDiscountConfigCreate)
type GetDomainDiscountConfigList = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                            ('DSL)
                                            (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.DOMAIN_DISCOUNT_CONFIG) / ('API.Types.ProviderPlatform.Management.DomainDiscountConfig.GET_DOMAIN_DISCOUNT_CONFIG_LIST)) :> API.Types.ProviderPlatform.Management.DomainDiscountConfig.GetDomainDiscountConfigList)
type DeleteDomainDiscountConfigDelete = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT)
                                                 ('DSL)
                                                 (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.DOMAIN_DISCOUNT_CONFIG) / ('API.Types.ProviderPlatform.Management.DomainDiscountConfig.DELETE_DOMAIN_DISCOUNT_CONFIG_DELETE)) :> API.Types.ProviderPlatform.Management.DomainDiscountConfig.DeleteDomainDiscountConfigDelete)
postDomainDiscountConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.CreateDomainDiscountConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDomainDiscountConfigCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DomainDiscountConfig.postDomainDiscountConfigCreate merchantShortId opCity apiTokenInfo req
getDomainDiscountConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.BillingCategory -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.DomainDiscountConfig.DomainDiscountConfigRes])
getDomainDiscountConfigList merchantShortId opCity apiTokenInfo billingCategory = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DomainDiscountConfig.getDomainDiscountConfigList merchantShortId opCity apiTokenInfo billingCategory
deleteDomainDiscountConfigDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.DomainDiscountConfig.DeleteDomainDiscountConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDomainDiscountConfigDelete merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DomainDiscountConfig.deleteDomainDiscountConfigDelete merchantShortId opCity apiTokenInfo req



