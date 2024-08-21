module Storage.Queries.Transformers.SearchRequest where

import qualified Domain.Types.Location
import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.SearchRequest (RiderPreferredOption (..), SearchRequestDetails (..))
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.SearchRequestDeliveryDetails as QSRDD
import Tools.Error

getFromLocation :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Kernel.Prelude.Text -> m Domain.Types.Location.Location
getFromLocation id = do
  fromLocationMapping <- QLM.getLatestStartByEntityId id >>= fromMaybeM (FromLocationMappingNotFound id)
  fromLocation <- QL.findById fromLocationMapping.locationId >>= fromMaybeM (FromLocationNotFound fromLocationMapping.locationId.getId)
  return fromLocation

backfillMOCId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
backfillMOCId merchantId = \case
  Just mocId -> pure $ Kernel.Types.Id.Id mocId
  Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (Kernel.Types.Id.Id merchantId)

getToLocation :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Domain.Types.Location.Location)
getToLocation id = do
  mbToLocationMapping <- QLM.getLatestEndByEntityId id
  toLocation <- maybe (pure Nothing) (QL.findById . (.locationId)) mbToLocationMapping
  return toLocation

getSearchRequestDetails :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.SearchRequest.RiderPreferredOption -> m (Kernel.Prelude.Maybe SearchRequestDetails)
getSearchRequestDetails id = \case
  Just Delivery -> do
    searchRequestDetails <- QSRDD.findBySearchRequestId id
    pure $ DeliveryDetails <$> searchRequestDetails
  _ -> pure Nothing
