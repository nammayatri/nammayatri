module Storage.Queries.Transformers.RegistrationToken where

import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Kernel.Prelude.Text -> Text -> m Kernel.Prelude.Text
getMerchantOperatingCityId merchantOperatingCityId merchantId = do
  case merchantOperatingCityId of
    Just opCityId -> return opCityId
    Nothing -> do
      merchant <- CQM.findById (Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
      mOpCId <- CQMOC.getMerchantOpCityId Nothing merchant Nothing
      return $ mOpCId.getId
