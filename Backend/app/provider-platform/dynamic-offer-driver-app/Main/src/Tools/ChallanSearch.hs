module Tools.ChallanSearch (getPendingChallanCount) where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DOSC
import qualified Kernel.External.ChallanSearch.Interface as CSI
import qualified Kernel.External.ChallanSearch.Interface.Types as CSIT
import qualified Kernel.External.ChallanSearch.Types as CST
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import Tools.Error

getPendingChallanCount :: ServiceFlow m r => Id Merchant -> Id MerchantOperatingCity -> CSIT.PendingChallanReq -> m CSIT.PendingChallanResp
getPendingChallanCount _merchantId merchantOpCityId req = do
  let serviceProvider = CST.Signzy
  merchantServiceConfig <-
    QOMSC.findByServiceAndCity (DOSC.ChallanSearchService serviceProvider) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "ChallanSearch" (show serviceProvider))
  case merchantServiceConfig.serviceConfig of
    DOSC.ChallanSearchServiceConfig msc -> CSI.getPendingChallanCount msc req
    _ -> throwError $ InternalError "Unknown Service Config"
