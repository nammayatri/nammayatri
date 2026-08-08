{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.VasBanner
  ( API.Types.ProviderPlatform.Management.VasBanner.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.VasBanner
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.VasBanner
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.VasBanner.API)
handler merchantId city = getVasBannerList merchantId city :<|> postVasBannerCreate merchantId city :<|> postVasBannerUpdate merchantId city :<|> postVasBannerDelete merchantId city

getVasBannerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VasBanner.VasBannerListRes)
getVasBannerList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VasBanner.getVasBannerList a3 a2 a1

postVasBannerCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.VasBanner.VasBannerCreateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VasBanner.VasBannerRes)
postVasBannerCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VasBanner.postVasBannerCreate a3 a2 a1

postVasBannerUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig -> API.Types.ProviderPlatform.Management.VasBanner.VasBannerUpdateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.VasBanner.VasBannerRes)
postVasBannerUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VasBanner.postVasBannerUpdate a4 a3 a2 a1

postVasBannerDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVasBannerDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.VasBanner.postVasBannerDelete a3 a2 a1
