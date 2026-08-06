module Domain.Action.Dashboard.Management.VasBanner
  ( getVasBannerList,
    postVasBannerCreate,
    postVasBannerUpdate,
    postVasBannerDelete,
  )
where

import qualified API.Types.ProviderPlatform.Management.VasBanner
import qualified Dashboard.Common
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id

getVasBannerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.Flow API.Types.ProviderPlatform.Management.VasBanner.VasBannerListRes)
getVasBannerList _merchantShortId _opCity enabled = do error "Logic yet to be decided" enabled

postVasBannerCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.VasBanner.VasBannerCreateReq -> Environment.Flow API.Types.ProviderPlatform.Management.VasBanner.VasBannerRes)
postVasBannerCreate _merchantShortId _opCity req = do error "Logic yet to be decided" req

postVasBannerUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig -> API.Types.ProviderPlatform.Management.VasBanner.VasBannerUpdateReq -> Environment.Flow API.Types.ProviderPlatform.Management.VasBanner.VasBannerRes)
postVasBannerUpdate _merchantShortId _opCity bannerId req = do error "Logic yet to be decided" bannerId req

postVasBannerDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.VasBannerConfig -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postVasBannerDelete _merchantShortId _opCity bannerId = do error "Logic yet to be decided" bannerId
