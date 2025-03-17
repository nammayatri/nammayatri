{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Booking where

import qualified Data.Text
import qualified Domain.Types.Booking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Kernel.Utils.JSON
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Booking as Beam
import qualified Storage.Queries.LocationMapping
import Storage.Queries.Transformers.Booking
import qualified Storage.Queries.Transformers.Booking
import qualified Storage.Queries.TripTerms

instance FromTType' Beam.Booking Domain.Types.Booking.Booking where
  fromTType' (Beam.BookingT {..}) = do
    mappings <- Storage.Queries.LocationMapping.findByEntityId id
    toBookingDetailsAndFromLocation' <- Storage.Queries.Transformers.Booking.toBookingDetailsAndFromLocation id merchantId merchantOperatingCityId mappings distance fareProductType tripCategory toLocationId fromLocationId stopLocationId otpCode isUpgradedToCab distanceUnit distanceValue hasStops parcelType parcelQuantity
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    initialPickupLocation' <- Storage.Queries.Transformers.Booking.getInitialPickupLocation mappings (fst toBookingDetailsAndFromLocation')
    merchantOperatingCityId' <- Storage.Queries.Transformers.Booking.backfillMOCId merchantOperatingCityId merchantId
    providerUrl' <- parseBaseUrl providerUrl
    tripTerms' <- if isJust tripTermsId then Storage.Queries.TripTerms.findById'' (Kernel.Types.Id.Id (fromJust tripTermsId)) else pure Nothing
    vehicleIconUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) vehicleIconUrl
    pure $
      Just
        Domain.Types.Booking.Booking
          { backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            bookingDetails = snd toBookingDetailsAndFromLocation',
            bppBookingId = Kernel.Types.Id.Id <$> bppBookingId,
            bppEstimateId = itemId,
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientId = Kernel.Types.Id.Id <$> clientId,
            clientSdkVersion = clientSdkVersion',
            configInExperimentVersions = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< configInExperimentVersions),
            createdAt = createdAt,
            disabilityTag = disabilityTag,
            discount = Kernel.Types.Common.mkPrice currency <$> discount,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            estimatedDistance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit estimatedDistanceValue <$> estimatedDistance,
            estimatedDuration = estimatedDuration,
            estimatedFare = Kernel.Types.Common.mkPrice currency estimatedFare,
            estimatedStaticDuration = estimatedStaticDuration,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            fromLocation = fst toBookingDetailsAndFromLocation',
            fulfillmentId = fulfillmentId,
            hasStops = hasStops,
            id = Kernel.Types.Id.Id id,
            initialPickupLocation = initialPickupLocation',
            initiatedBy = initiatedBy,
            isAirConditioned = isAirConditioned,
            isBookingUpdated = fromMaybe False isBookingUpdated,
            isDashboardRequest = isDashboardRequest,
            isDeleted = isDeleted,
            isReferredRide = isReferredRide,
            isScheduled = fromMaybe False isScheduled,
            isSkipped = isSkipped,
            journeyId = Kernel.Types.Id.Id <$> journeyId,
            journeyLegOrder = journeyLegOrder,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            paymentMethodId = paymentMethodId,
            paymentStatus = paymentStatus,
            paymentUrl = paymentUrl,
            primaryExophone = primaryExophone,
            providerId = providerId,
            providerUrl = providerUrl',
            quoteId = Kernel.Types.Id.Id <$> quoteId,
            returnTime = returnTime,
            riderId = Kernel.Types.Id.Id riderId,
            roundTrip = roundTrip,
            serviceTierName = serviceTierName,
            serviceTierShortDesc = serviceTierShortDesc,
            specialLocationName = specialLocationName,
            specialLocationTag = specialLocationTag,
            startTime = startTime,
            status = status,
            transactionId = riderTransactionId,
            tripCategory = tripCategory,
            tripTerms = tripTerms',
            updatedAt = updatedAt,
            vehicleIconUrl = vehicleIconUrl',
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
            vehicleServiceTierType = vehicleVariant
          }

instance ToTType' Beam.Booking Domain.Types.Booking.Booking where
  toTType' (Domain.Types.Booking.Booking {..}) = do
    let distance = getDistance bookingDetails
    Beam.BookingT
      { Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = Kernel.Utils.Version.versionToText <$> backendConfigVersion,
        Beam.distance = Kernel.Utils.Common.distanceToHighPrecMeters <$> distance,
        Beam.fareProductType = getFareProductType bookingDetails,
        Beam.isUpgradedToCab = getIsUpgradedToCab bookingDetails,
        Beam.otpCode = getOtpCode bookingDetails,
        Beam.parcelQuantity = getParcelQuantity bookingDetails,
        Beam.parcelType = getParcelType bookingDetails,
        Beam.stopLocationId = getStopLocationId bookingDetails,
        Beam.toLocationId = getToLocationId bookingDetails,
        Beam.bppBookingId = Kernel.Types.Id.getId <$> bppBookingId,
        Beam.itemId = bppEstimateId,
        Beam.clientBundleVersion = Kernel.Utils.Version.versionToText <$> clientBundleVersion,
        Beam.clientConfigVersion = Kernel.Utils.Version.versionToText <$> clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= (.deviceManufacturer),
        Beam.clientModelName = clientDevice <&> (.deviceModel),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.clientSdkVersion = Kernel.Utils.Version.versionToText <$> clientSdkVersion,
        Beam.configInExperimentVersions = Just $ toJSON configInExperimentVersions,
        Beam.createdAt = createdAt,
        Beam.disabilityTag = disabilityTag,
        Beam.discount = discount <&> (.amount),
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.distanceValue = Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> distance,
        Beam.estimatedDistance = Kernel.Utils.Common.distanceToHighPrecMeters <$> estimatedDistance,
        Beam.estimatedDistanceValue = Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.currency = Just $ (.currency) estimatedFare,
        Beam.estimatedFare = (.amount) estimatedFare,
        Beam.estimatedStaticDuration = estimatedStaticDuration,
        Beam.estimatedTotalFare = (.amount) estimatedTotalFare,
        Beam.fromLocationId = Just $ Kernel.Types.Id.getId $ (.id) fromLocation,
        Beam.fulfillmentId = fulfillmentId,
        Beam.hasStops = hasStops,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.initiatedBy = initiatedBy,
        Beam.isAirConditioned = isAirConditioned,
        Beam.isBookingUpdated = Just isBookingUpdated,
        Beam.isDashboardRequest = isDashboardRequest,
        Beam.isDeleted = isDeleted,
        Beam.isReferredRide = isReferredRide,
        Beam.isScheduled = Just isScheduled,
        Beam.isSkipped = isSkipped,
        Beam.journeyId = Kernel.Types.Id.getId <$> journeyId,
        Beam.journeyLegOrder = journeyLegOrder,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentMethodId = paymentMethodId,
        Beam.paymentStatus = paymentStatus,
        Beam.paymentUrl = paymentUrl,
        Beam.primaryExophone = primaryExophone,
        Beam.providerId = providerId,
        Beam.providerUrl = showBaseUrl providerUrl,
        Beam.quoteId = Kernel.Types.Id.getId <$> quoteId,
        Beam.returnTime = returnTime,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.roundTrip = roundTrip,
        Beam.serviceTierName = serviceTierName,
        Beam.serviceTierShortDesc = serviceTierShortDesc,
        Beam.specialLocationName = specialLocationName,
        Beam.specialLocationTag = specialLocationTag,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.riderTransactionId = transactionId,
        Beam.tripCategory = tripCategory,
        Beam.tripTermsId = Kernel.Types.Id.getId <$> (tripTerms <&> (.id)),
        Beam.updatedAt = updatedAt,
        Beam.vehicleIconUrl = Kernel.Prelude.fmap showBaseUrl vehicleIconUrl,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
        Beam.vehicleVariant = vehicleServiceTierType
      }
