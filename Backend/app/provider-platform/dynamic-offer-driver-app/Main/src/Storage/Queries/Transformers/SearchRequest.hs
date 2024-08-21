module Storage.Queries.Transformers.SearchRequest where

import Domain.Types.SearchRequest
import Domain.Types.Trip
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Queries.SearchRequestDeliveryDetails as QSRDD

getSearchRequestDetails :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> Kernel.Prelude.Maybe TripCategory -> m (Maybe SearchRequestDetails)
getSearchRequestDetails searchReqId = \case
  Just (Delivery _) -> do
    searchRequestDeliveryDetails <- QSRDD.findBySearchRequestId searchReqId
    return $ DeliveryDetails <$> searchRequestDeliveryDetails
  _ -> return Nothing
