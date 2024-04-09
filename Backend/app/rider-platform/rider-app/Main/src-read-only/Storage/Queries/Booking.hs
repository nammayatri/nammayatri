{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Booking (module Storage.Queries.Booking, module ReExport) where

import qualified Domain.Types.Booking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as Beam
import Storage.Queries.BookingExtra as ReExport
import Storage.Queries.Transformers.Booking

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.Booking.Booking))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Booking.Booking -> m ())
updateByPrimaryKey (Domain.Types.Booking.Booking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.distance (getDistance bookingDetails),
      Se.Set Beam.fareProductType (getFareProductType bookingDetails),
      Se.Set Beam.otpCode (getOtpCode bookingDetails),
      Se.Set Beam.stopLocationId (getStopLocationId bookingDetails),
      Se.Set Beam.toLocationId (getToLocationId bookingDetails),
      Se.Set Beam.bppBookingId (Kernel.Types.Id.getId <$> bppBookingId),
      Se.Set Beam.clientId (Kernel.Types.Id.getId <$> clientId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.discount (discount <&> (.amount)),
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.currency (Just $ (.currency) estimatedFare),
      Se.Set Beam.estimatedFare ((.amount) estimatedFare),
      Se.Set Beam.estimatedTotalFare ((.amount) estimatedTotalFare),
      Se.Set Beam.fromLocationId (Just $ Kernel.Types.Id.getId $ (.id) fromLocation),
      Se.Set Beam.fulfillmentId fulfillmentId,
      Se.Set Beam.isScheduled (Just isScheduled),
      Se.Set Beam.itemId itemId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Just $ Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.paymentMethodId (Kernel.Types.Id.getId <$> paymentMethodId),
      Se.Set Beam.paymentStatus paymentStatus,
      Se.Set Beam.paymentUrl paymentUrl,
      Se.Set Beam.primaryExophone primaryExophone,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerUrl (showBaseUrl providerUrl),
      Se.Set Beam.quoteId (Kernel.Types.Id.getId <$> quoteId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.serviceTierName serviceTierName,
      Se.Set Beam.specialLocationTag specialLocationTag,
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status status,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.tripTermsId (Kernel.Types.Id.getId <$> (tripTerms <&> (.id))),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
