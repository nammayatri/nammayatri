module Storage.Queries.Transformers.SearchRequestForDriver where

import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fromMaybeM)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.SearchRequest as QR

getMerchantOpCId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
getMerchantOpCId merchantOperatingCityId merchantId requestId = do
  merchant <- case merchantId of
    Nothing -> do
      searchReq <- QR.findById (Id requestId) >>= fromMaybeM (InternalError $ "Search request not found : " <> requestId)
      CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound searchReq.providerId.getId)
    Just mId -> CQM.findById (Id mId) >>= fromMaybeM (MerchantNotFound mId)
  CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant Nothing

getCustomerCancellationDues :: Maybe HighPrecMoney -> HighPrecMoney
getCustomerCancellationDues customerCancellationDues = fromMaybe 0.0 customerCancellationDues
