module Storage.Queries.Transformers.Person where

import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getMerchantOpCId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Maybe Text -> m (Kernel.Types.Id.Id MerchantOperatingCity)
getMerchantOpCId merchantId merchantOperatingCityId = do
  merchant <- CQM.findById (Kernel.Types.Id.Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
  CQMOC.getMerchantOpCityId (Kernel.Types.Id.Id <$> merchantOperatingCityId) merchant Nothing
