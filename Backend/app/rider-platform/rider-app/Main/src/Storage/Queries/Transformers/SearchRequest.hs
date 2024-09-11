module Storage.Queries.Transformers.SearchRequest where

import qualified Domain.Types.Location
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyPlanner.Types
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
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

mkJourneyLegInfo :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Lib.JourneyPlanner.Types.JourneySearchData)
mkJourneyLegInfo agency (Just convenienceCost) (Just journeyId_) (Just journeyLegOrder) (Just skipBooking) = Just $ Lib.JourneyPlanner.Types.JourneySearchData {journeyId = Kernel.Types.Id.Id journeyId_, ..}
mkJourneyLegInfo _ _ _ _ _ = Nothing
