{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Booking where

import qualified Domain.Types.Booking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.Booking as Beam
import qualified Storage.CachedQueries.Merchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity
import qualified Storage.Queries.FareParameters
import qualified Storage.Queries.LocationMapping
import Storage.Queries.Transformers.Booking
import qualified Storage.Queries.Transformers.Booking

instance FromTType' Beam.Booking Domain.Types.Booking.Booking where
  fromTType' (Beam.BookingT {..}) = do
    mappings <- Storage.Queries.LocationMapping.findByEntityId id
    fromAndToLocation' <- Storage.Queries.Transformers.Booking.fromAndToLocation mappings tripCategory id fromLocationId toLocationId providerId merchantOperatingCityId
    merchant <- Storage.CachedQueries.Merchant.findById (Kernel.Types.Id.Id providerId) >>= fromMaybeM (Kernel.Types.Error.MerchantNotFound providerId)
    senderAndReceiverDetails <- Storage.Queries.Transformers.Booking.getSenderAndReceiverDetails tripCategory senderId senderName senderPrimaryExophone receiverId receiverName receiverPrimaryExophone
    fareParams' <- Storage.Queries.FareParameters.findById (Kernel.Types.Id.Id fareParametersId) >>= fromMaybeM (Kernel.Types.Error.InternalError ("FareParameters not found for booking: " <> show id))
    merchantOperatingCityId' <- Storage.CachedQueries.Merchant.MerchantOperatingCity.getMerchantOpCityId (Kernel.Types.Id.Id <$> merchantOperatingCityId) merchant bapCity
    stops' <- Storage.Queries.Transformers.Booking.getStops id hasStops
    pure $
      Just
        Domain.Types.Booking.Booking
          { area = area,
            bapCity = bapCity,
            bapCountry = bapCountry,
            bapId = bapId,
            bapUri = bapUri,
            coinsRewardedOnGoldTierRide = coinsRewardedOnGoldTierRide,
            configInExperimentVersions = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< configInExperimentVersions),
            createdAt = createdAt,
            currency = fromMaybe Kernel.Types.Common.INR currency,
            disabilityTag = disabilityTag,
            distanceToPickup = Kernel.Prelude.roundToIntegral <$> distanceToPickup,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            dynamicPricingLogicVersion = dynamicPricingLogicVersion,
            estimateId = Kernel.Types.Id.Id <$> estimateId,
            estimatedCongestionCharge = estimatedCongestionCharge,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            estimatedFare = estimatedFare,
            fareParams = fareParams',
            fromLocGeohash = fromLocGeohash,
            fromLocation = fst fromAndToLocation',
            hasStops = hasStops,
            id = Kernel.Types.Id.Id id,
            initiatedAs = initiatedAs,
            insuredAmount = insuredAmount,
            isAirConditioned = isAirConditioned,
            isDashboardRequest = fromMaybe False isDashboardRequest,
            isInsured = fromMaybe False isInsured,
            isPetRide = fromMaybe False isPetRide,
            isReferredRide = isReferredRide,
            isSafetyPlus = fromMaybe False isSafetyPlus,
            isScheduled = fromMaybe False isScheduled,
            maxEstimatedDistance = maxEstimatedDistance,
            merchantOperatingCityId = merchantOperatingCityId',
            parcelQuantity = parcelQuantity,
            parcelType = parcelType,
            paymentId = paymentId,
            paymentMethodId = Kernel.Types.Id.Id <$> paymentMethodId,
            paymentUrl = paymentUrl,
            primaryExophone = primaryExophone,
            providerId = Kernel.Types.Id.Id providerId,
            quoteId = quoteId,
            receiverDetails = snd <$> senderAndReceiverDetails,
            returnTime = returnTime,
            riderId = Kernel.Types.Id.Id <$> riderId,
            riderName = riderName,
            roundTrip = roundTrip,
            senderDetails = fst <$> senderAndReceiverDetails,
            specialLocationTag = specialLocationTag,
            specialZoneOtpCode = specialZoneOtpCode,
            startTime = startTime,
            status = status,
            stopLocationId = Kernel.Types.Id.Id <$> stopLocationId,
            stops = stops',
            toLocGeohash = toLocGeohash,
            toLocation = snd fromAndToLocation',
            tollNames = tollNames,
            transactionId = transactionId,
            tripCategory = getTripCategory bookingType tripCategory,
            updatedAt = updatedAt,
            vehicleServiceTier = vehicleVariant,
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            vehicleServiceTierName = fromMaybe (show vehicleVariant) vehicleServiceTierName,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity
          }

instance ToTType' Beam.Booking Domain.Types.Booking.Booking where
  toTType' (Domain.Types.Booking.Booking {..}) = do
    Beam.BookingT
      { Beam.area = area,
        Beam.bapCity = bapCity,
        Beam.bapCountry = bapCountry,
        Beam.bapId = bapId,
        Beam.bapUri = bapUri,
        Beam.coinsRewardedOnGoldTierRide = coinsRewardedOnGoldTierRide,
        Beam.configInExperimentVersions = Just $ toJSON configInExperimentVersions,
        Beam.createdAt = createdAt,
        Beam.currency = Just currency,
        Beam.disabilityTag = disabilityTag,
        Beam.distanceToPickup = Kernel.Prelude.realToFrac <$> distanceToPickup,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.dynamicPricingLogicVersion = dynamicPricingLogicVersion,
        Beam.estimateId = Kernel.Types.Id.getId <$> estimateId,
        Beam.estimatedCongestionCharge = estimatedCongestionCharge,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedFare = estimatedFare,
        Beam.fareParametersId = Kernel.Types.Id.getId $ (.id) fareParams,
        Beam.fromLocGeohash = fromLocGeohash,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId $ (.id) fromLocation,
        Beam.hasStops = hasStops,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.initiatedAs = initiatedAs,
        Beam.insuredAmount = insuredAmount,
        Beam.isAirConditioned = isAirConditioned,
        Beam.isDashboardRequest = Just isDashboardRequest,
        Beam.isInsured = Just isInsured,
        Beam.isPetRide = Just isPetRide,
        Beam.isReferredRide = isReferredRide,
        Beam.isSafetyPlus = Just isSafetyPlus,
        Beam.isScheduled = Just isScheduled,
        Beam.maxEstimatedDistance = maxEstimatedDistance,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.parcelQuantity = parcelQuantity,
        Beam.parcelType = parcelType,
        Beam.paymentId = paymentId,
        Beam.paymentMethodId = Kernel.Types.Id.getId <$> paymentMethodId,
        Beam.paymentUrl = paymentUrl,
        Beam.primaryExophone = primaryExophone,
        Beam.providerId = Kernel.Types.Id.getId providerId,
        Beam.quoteId = quoteId,
        Beam.receiverId = Kernel.Types.Id.getId <$> (receiverDetails <&> (.id)),
        Beam.receiverName = receiverDetails <&> (.name),
        Beam.receiverPrimaryExophone = receiverDetails <&> (.primaryExophone),
        Beam.returnTime = returnTime,
        Beam.riderId = Kernel.Types.Id.getId <$> riderId,
        Beam.riderName = riderName,
        Beam.roundTrip = roundTrip,
        Beam.senderId = Kernel.Types.Id.getId <$> (senderDetails <&> (.id)),
        Beam.senderName = senderDetails <&> (.name),
        Beam.senderPrimaryExophone = senderDetails <&> (.primaryExophone),
        Beam.specialLocationTag = specialLocationTag,
        Beam.specialZoneOtpCode = specialZoneOtpCode,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.stopLocationId = Kernel.Types.Id.getId <$> stopLocationId,
        Beam.toLocGeohash = toLocGeohash,
        Beam.toLocationId = Kernel.Types.Id.getId . (.id) <$> toLocation,
        Beam.tollNames = tollNames,
        Beam.transactionId = transactionId,
        Beam.bookingType = Storage.Queries.Transformers.Booking.getBookingTypeFromTripCategory tripCategory,
        Beam.tripCategory = Just tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.vehicleVariant = vehicleServiceTier,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.vehicleServiceTierName = Just vehicleServiceTierName,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity
      }
