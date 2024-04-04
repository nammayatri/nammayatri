{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Ride where

import Domain.Types.Location
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Ride (OdometerReading (..))
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Common (Centesimal)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM)
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

getEndOdometerReadingFileId :: (Kernel.Prelude.Maybe Domain.Types.Ride.OdometerReading -> Kernel.Prelude.Maybe Kernel.Prelude.Text)
getEndOdometerReadingFileId endOdometerReading = getId <$> (endOdometerReading >>= (.fileId))

getStartOdometerReadingFileId :: (Kernel.Prelude.Maybe Domain.Types.Ride.OdometerReading -> Kernel.Prelude.Maybe Kernel.Prelude.Text)
getStartOdometerReadingFileId startOdometerReading = getId <$> (startOdometerReading >>= (.fileId))

getToLocation :: KvDbFlow m r => Text -> Text -> Maybe Text -> Maybe Text -> m (Maybe Location)
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

getFromLocation :: KvDbFlow m r => Text -> Text -> Maybe Text -> Maybe Text -> m Location
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

getMerchantOperatingCityId :: KvDbFlow m r => Text -> Maybe Text -> Maybe Text -> m (Id MerchantOperatingCity)
getMerchantOperatingCityId bookingId merchantId merchantOperatingCityId = do
  merchant <- case merchantId of
    Nothing -> do
      booking <- QBooking.findById (Id bookingId) >>= fromMaybeM (BookingNotFound bookingId)
      CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
    Just mId -> CQM.findById (Id mId) >>= fromMaybeM (MerchantNotFound mId)
  CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant Nothing

mkOdometerReading :: Maybe Text -> Maybe Centesimal -> Maybe OdometerReading
mkOdometerReading fileId odoValue = odoValue <&> (\value -> OdometerReading (Id <$> fileId) value)

mkLatLong :: Maybe Double -> Maybe Double -> Maybe LatLong
mkLatLong lat lon = LatLong <$> lat <*> lon
