module Storage.Queries.Extra.Transformers.Ride where

import Domain.Types.Location
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Ride (EstimatedEndTimeRange (..))
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM)
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

getToLocation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> Maybe Text -> Maybe Text -> m (Maybe Location)
getToLocation id bookingId merchantId merchantOperatingCityId = do
  mappings <- QLM.findByEntityId id
  mbToLocationMapping <-
    if null mappings
      then do
        void $ QBooking.findById (Id bookingId)
        mbToLocationMapping <- QLM.getLatestEndByEntityId bookingId
        mbToLocationRideMapping <- (\toLocMap -> SLM.buildDropLocationMapping toLocMap.locationId id DLM.RIDE (Id <$> merchantId) (Id <$> merchantOperatingCityId)) `mapM` mbToLocationMapping
        whenJust mbToLocationRideMapping QLM.create
        return mbToLocationRideMapping
      else QLM.getLatestEndByEntityId id
  maybe (pure Nothing) (QL.findById . (.locationId)) mbToLocationMapping

getFromLocation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> Maybe Text -> Maybe Text -> m Location
getFromLocation id bookingId merchantId merchantOperatingCityId = do
  mappings <- QLM.findByEntityId id
  fromLocationMapping <-
    if null mappings
      then do
        void $ QBooking.findById (Id bookingId)
        fromLocationMapping <- QLM.getLatestStartByEntityId bookingId >>= fromMaybeM (FromLocationMappingNotFound bookingId)
        fromLocationRideMapping <- SLM.buildPickUpLocationMapping fromLocationMapping.locationId id DLM.RIDE (Id <$> merchantId) (Id <$> merchantOperatingCityId)
        QLM.create fromLocationRideMapping
        return fromLocationRideMapping
      else QLM.getLatestStartByEntityId id >>= fromMaybeM (FromLocationMappingNotFound id)
  QL.findById fromLocationMapping.locationId >>= fromMaybeM (FromLocationNotFound fromLocationMapping.locationId.getId)

getStops :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> Maybe Bool -> m [Location]
getStops id hasStops = do
  if hasStops == Just True
    then do
      stopsLocationMapping <- QLM.getLatestStopsByEntityId id
      mapM
        ( \stopLocationMapping ->
            QL.findById stopLocationMapping.locationId
              >>= fromMaybeM (StopsLocationNotFound stopLocationMapping.locationId.getId)
        )
        stopsLocationMapping
    else return []

mkLatLong :: Maybe Double -> Maybe Double -> Maybe LatLong
mkLatLong lat lon = LatLong <$> lat <*> lon

mkEstimatedEndTimeRange :: UTCTime -> UTCTime -> EstimatedEndTimeRange
mkEstimatedEndTimeRange start end = EstimatedEndTimeRange {..}
