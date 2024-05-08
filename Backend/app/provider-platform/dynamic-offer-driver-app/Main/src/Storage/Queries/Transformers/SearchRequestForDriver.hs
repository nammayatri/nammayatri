{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.SearchRequestForDriver where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.SearchRequest as QR

getMerchantOpCId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity))
getMerchantOpCId merchantOperatingCityId merchantId requestId = do
  merchant <- case merchantId of
    Nothing -> do
      searchReq <- QR.findById (Id requestId) >>= fromMaybeM (InternalError $ "Search request not found : " <> requestId)
      CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound searchReq.providerId.getId)
    Just mId -> CQM.findById (Id mId) >>= fromMaybeM (MerchantNotFound mId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant Nothing
  return merchantOpCityId

getCustomerCancellationDues :: Maybe HighPrecMoney -> HighPrecMoney
getCustomerCancellationDues customerCancellationDues = fromMaybe 0.0 customerCancellationDues
