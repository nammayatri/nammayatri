module Storage.Queries.Transformers.Person where

import Data.Text
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM)
import qualified Storage.CachedQueries.Merchant as CQM

backfillCityAndMOCId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Maybe Text -> Text -> m (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity, Kernel.Types.Beckn.Context.City)
backfillCityAndMOCId mbCity mbMerchantOperatingCityId merchantId =
  case mbMerchantOperatingCityId of
    Just mocId -> do
      city <- backfillCity mbCity merchantId
      pure (Kernel.Types.Id.Id mocId, city)
    Nothing -> do
      moc <- CQM.getDefaultMerchantOperatingCity (Kernel.Types.Id.Id merchantId)
      pure (moc.id, moc.city)

backfillCity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Text -> m Kernel.Types.Beckn.Context.City
backfillCity city merchantId = case city of
  Just city' -> pure city'
  Nothing -> CQM.findById (Kernel.Types.Id.Id merchantId) >>= fmap (.defaultCity) <$> fromMaybeM (MerchantNotFound merchantId)
