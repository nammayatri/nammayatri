{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Booking where

import qualified Domain.Types.Booking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Booking as Beam
import qualified Storage.Queries.LocationMapping
import Storage.Queries.Transformers.Booking
import qualified Storage.Queries.Transformers.Booking
import qualified Storage.Queries.TripTerms

instance FromTType' Beam.Booking Domain.Types.Booking.Booking where
  fromTType' (Beam.BookingT {..}) = do
    mappings <- Storage.Queries.LocationMapping.findByEntityId id
    fromLocationAndBookingDetails' <- Storage.Queries.Transformers.Booking.fromLocationAndBookingDetails id merchantId merchantOperatingCityId mappings distance fareProductType toLocationId fromLocationId stopLocationId otpCode
    initialPickupLocation' <- Storage.Queries.Transformers.Booking.getInitialPickupLocation mappings (fst fromLocationAndBookingDetails')
    merchantOperatingCityId' <- Storage.Queries.Transformers.Booking.backfillMOCId merchantOperatingCityId merchantId
    providerUrl' <- parseBaseUrl providerUrl
    tripTerms' <- if isJust tripTermsId then Storage.Queries.TripTerms.findById'' (Kernel.Types.Id.Id (fromJust tripTermsId)) else pure Nothing
    pure $
      Just
        Domain.Types.Booking.Booking
          { bookingDetails = snd fromLocationAndBookingDetails',
            bppBookingId = Kernel.Types.Id.Id <$> bppBookingId,
            clientId = Kernel.Types.Id.Id <$> clientId,
            createdAt = createdAt,
            discount = Kernel.Types.Common.mkPrice currency <$> discount,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            fromLocation = fst fromLocationAndBookingDetails',
            fulfillmentId = fulfillmentId,
            id = Kernel.Types.Id.Id id,
            initialPickupLocation = initialPickupLocation',
            isScheduled = fromMaybe False isScheduled,
            itemId = itemId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            paymentMethodId = Kernel.Types.Id.Id <$> paymentMethodId,
            paymentStatus = paymentStatus,
            paymentUrl = paymentUrl,
            primaryExophone = primaryExophone,
            providerId = providerId,
            providerUrl = providerUrl',
            quoteId = Kernel.Types.Id.Id <$> quoteId,
            riderId = Kernel.Types.Id.Id riderId,
            serviceTierName = serviceTierName,
            specialLocationTag = specialLocationTag,
            startTime = startTime,
            status = status,
            transactionId = transactionId,
            tripTerms = tripTerms',
            updatedAt = updatedAt,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.Booking Domain.Types.Booking.Booking where
  toTType' (Domain.Types.Booking.Booking {..}) = do
    Beam.BookingT
      { Beam.distance = getDistance bookingDetails,
        Beam.fareProductType = getFareProductType bookingDetails,
        Beam.otpCode = getOtpCode bookingDetails,
        Beam.stopLocationId = getStopLocationId bookingDetails,
        Beam.toLocationId = getToLocationId bookingDetails,
        Beam.bppBookingId = Kernel.Types.Id.getId <$> bppBookingId,
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.createdAt = createdAt,
        Beam.discount = discount <&> (.amount),
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.currency = Just $ (.currency) estimatedFare,
        Beam.estimatedFare = (.amount) estimatedFare,
        Beam.estimatedTotalFare = (.amount) estimatedTotalFare,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId $ (.id) fromLocation,
        Beam.fulfillmentId = fulfillmentId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isScheduled = Just isScheduled,
        Beam.itemId = itemId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentMethodId = Kernel.Types.Id.getId <$> paymentMethodId,
        Beam.paymentStatus = paymentStatus,
        Beam.paymentUrl = paymentUrl,
        Beam.primaryExophone = primaryExophone,
        Beam.providerId = providerId,
        Beam.providerUrl = showBaseUrl providerUrl,
        Beam.quoteId = Kernel.Types.Id.getId <$> quoteId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.serviceTierName = serviceTierName,
        Beam.specialLocationTag = specialLocationTag,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.transactionId = transactionId,
        Beam.tripTermsId = Kernel.Types.Id.getId <$> (tripTerms <&> (.id)),
        Beam.updatedAt = updatedAt,
        Beam.vehicleVariant = vehicleVariant
      }
