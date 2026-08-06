module Domain.Action.UI.VasBanner (getVasBannerList) where

import qualified API.Types.UI.VasBanner as API
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VasBannerConfig as DVC
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Queries.VasBannerConfig as QVBC

getVasBannerList ::
  ( ( Maybe (Id DP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Environment.Flow API.VasBannerListRes
  )
getVasBannerList (_mbPersonId, _merchantId, merchantOperatingCityId) = do
  banners <- QVBC.findAllEnabledByCity Nothing Nothing merchantOperatingCityId True
  pure $ API.VasBannerListRes {banners = map toVasBannerAPIEntity banners}

toVasBannerAPIEntity :: DVC.VasBannerConfig -> API.VasBannerAPIEntity
toVasBannerAPIEntity DVC.VasBannerConfig {..} = API.VasBannerAPIEntity {..}
