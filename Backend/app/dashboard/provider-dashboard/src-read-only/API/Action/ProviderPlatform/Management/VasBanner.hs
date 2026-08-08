{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.VasBanner
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.VasBanner
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.VasBanner
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

type API = ("vasBanner" :> (GetVasBannerList :<|> PostVasBannerCreate :<|> PostVasBannerUpdate :<|> PostVasBannerDelete))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVasBannerList merchantId city :<|> postVasBannerCreate merchantId city :<|> postVasBannerUpdate merchantId city :<|> postVasBannerDelete merchantId city

type GetVasBannerList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.VAS_BANNER) / ('API.Types.ProviderPlatform.Management.VasBanner.GET_VAS_BANNER_LIST))
      :> API.Types.ProviderPlatform.Management.VasBanner.GetVasBannerList
  )

type PostVasBannerCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.VAS_BANNER) / ('API.Types.ProviderPlatform.Management.VasBanner.POST_VAS_BANNER_CREATE))
      :> API.Types.ProviderPlatform.Management.VasBanner.PostVasBannerCreate
  )

type PostVasBannerUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.VAS_BANNER) / ('API.Types.ProviderPlatform.Management.VasBanner.POST_VAS_BANNER_UPDATE))
      :> API.Types.ProviderPlatform.Management.VasBanner.PostVasBannerUpdate
  )

type PostVasBannerDelete =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.VAS_BANNER) / ('API.Types.ProviderPlatform.Management.VasBanner.POST_VAS_BANNER_DELETE))
      :> API.Types.ProviderPlatform.Management.VasBanner.PostVasBannerDelete
  )

getVasBannerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VasBanner.VasBannerListRes)
getVasBannerList merchantShortId opCity apiTokenInfo enabled = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.VasBanner.getVasBannerList merchantShortId opCity apiTokenInfo enabled

postVasBannerCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.VasBanner.VasBannerCreateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VasBanner.VasBannerRes)
postVasBannerCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.VasBanner.postVasBannerCreate merchantShortId opCity apiTokenInfo req

postVasBannerUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig -> API.Types.ProviderPlatform.Management.VasBanner.VasBannerUpdateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VasBanner.VasBannerRes)
postVasBannerUpdate merchantShortId opCity apiTokenInfo bannerId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.VasBanner.postVasBannerUpdate merchantShortId opCity apiTokenInfo bannerId req

postVasBannerDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVasBannerDelete merchantShortId opCity apiTokenInfo bannerId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.VasBanner.postVasBannerDelete merchantShortId opCity apiTokenInfo bannerId
