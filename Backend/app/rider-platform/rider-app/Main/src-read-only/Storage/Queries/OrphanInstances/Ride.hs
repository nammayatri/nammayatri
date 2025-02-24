{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Ride where

import qualified Data.Text
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Ride as Beam
import qualified Storage.Queries.Extra.Transformers.Ride

instance FromTType' Beam.Ride Domain.Types.Ride.Ride where
  fromTType' (Beam.RideT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    fromLocation' <- Storage.Queries.Extra.Transformers.Ride.getFromLocation id bookingId merchantId merchantOperatingCityId
    stops' <- Storage.Queries.Extra.Transformers.Ride.getStops id hasStops
    toLocation' <- Storage.Queries.Extra.Transformers.Ride.getToLocation id bookingId merchantId merchantOperatingCityId
    trackingUrl' <- Kernel.Prelude.mapM Kernel.Prelude.parseBaseUrl trackingUrl
    pure $
      Just
        Domain.Types.Ride.Ride
          { allowedEditLocationAttempts = allowedEditLocationAttempts,
            allowedEditPickupLocationAttempts = allowedEditPickupLocationAttempts,
            backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            bookingId = Kernel.Types.Id.Id bookingId,
            bppRideId = Kernel.Types.Id.Id bppRideId,
            cancellationFeeIfCancelled = cancellationFeeIfCancelled,
            chargeableDistance = Kernel.Types.Common.mkDistanceWithDefault distanceUnit chargeableDistanceValue <$> chargeableDistance,
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientId = Kernel.Types.Id.Id <$> clientId,
            clientSdkVersion = clientSdkVersion',
            createdAt = createdAt,
            destinationReachedAt = destinationReachedAt,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverAccountId = driverAccountId,
            driverAlternateNumber = EncryptedHashed <$> (Encrypted <$> driverAlternateNumberEncrypted) <*> driverAlternateNumberHash,
            driverArrivalTime = driverArrivalTime,
            driverImage = driverImage,
            driverMobileCountryCode = driverMobileCountryCode,
            driverMobileNumber = driverMobileNumber,
            driverName = driverName,
            driverPhoneNumber = EncryptedHashed <$> (Encrypted <$> driverNumberEncrypted) <*> driverNumberHash,
            driverRating = driverRating,
            driverRegisteredAt = driverRegisteredAt,
            driversPreviousRideDropLoc = Storage.Queries.Extra.Transformers.Ride.mkLatLong driversPreviousRideDropLat driversPreviousRideDropLon,
            endOdometerReading = endOdometerReading,
            endOtp = endOtp,
            estimatedEndTimeRange = Storage.Queries.Extra.Transformers.Ride.mkEstimatedEndTimeRange <$> estimatedEndTimeRangeStart <*> estimatedEndTimeRangeEnd,
            fare = fmap (Kernel.Types.Common.mkPrice currency) fare,
            favCount = favCount,
            feedbackSkipped = Kernel.Prelude.fromMaybe False feedbackSkipped,
            fromLocation = fromLocation',
            hasStops = hasStops,
            id = Kernel.Types.Id.Id id,
            isAlreadyFav = isAlreadyFav,
            isFreeRide = isFreeRide,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            onlinePayment = Kernel.Prelude.fromMaybe False onlinePayment,
            otp = otp,
            paymentStatus = Kernel.Prelude.fromMaybe Domain.Types.Ride.Completed paymentStatus,
            pickupRouteCallCount = pickupRouteCallCount,
            rideEndTime = rideEndTime,
            rideRating = rideRating,
            rideStartTime = rideStartTime,
            safetyCheckStatus = safetyCheckStatus,
            safetyJourneyStatus = safetyJourneyStatus,
            shortId = Kernel.Types.Id.ShortId shortId,
            showDriversPreviousRideDropLoc = Kernel.Prelude.fromMaybe False showDriversPreviousRideDropLoc,
            startOdometerReading = startOdometerReading,
            status = status,
            stops = stops',
            talkedWithDriver = talkedWithDriver,
            tipAmount = fmap (Kernel.Types.Common.mkPrice currency) tipAmount,
            toLocation = toLocation',
            tollConfidence = tollConfidence,
            totalFare = fmap (Kernel.Types.Common.mkPrice currency) totalFare,
            trackingUrl = trackingUrl',
            traveledDistance = Kernel.Types.Common.mkDistanceWithDefault distanceUnit traveledDistanceValue <$> traveledDistance,
            updatedAt = updatedAt,
            vehicleAge = vehicleAge,
            vehicleColor = vehicleColor,
            vehicleModel = vehicleModel,
            vehicleNumber = vehicleNumber,
            vehicleServiceTierType = vehicleServiceTierType,
            vehicleVariant = vehicleVariant,
            wasRideSafe = wasRideSafe
          }

instance ToTType' Beam.Ride Domain.Types.Ride.Ride where
  toTType' (Domain.Types.Ride.Ride {..}) = do
    Beam.RideT
      { Beam.allowedEditLocationAttempts = allowedEditLocationAttempts,
        Beam.allowedEditPickupLocationAttempts = allowedEditPickupLocationAttempts,
        Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.bppRideId = Kernel.Types.Id.getId bppRideId,
        Beam.cancellationFeeIfCancelled = cancellationFeeIfCancelled,
        Beam.chargeableDistance = Kernel.Prelude.fmap Kernel.Types.Common.distanceToHighPrecMeters chargeableDistance,
        Beam.chargeableDistanceValue = Kernel.Prelude.fmap (Kernel.Types.Common.distanceToHighPrecDistance distanceUnit) chargeableDistance,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientManufacturer = clientDevice >>= (.deviceManufacturer),
        Beam.clientModelName = clientDevice <&> (.deviceModel),
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.destinationReachedAt = destinationReachedAt,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverAccountId = driverAccountId,
        Beam.driverAlternateNumberEncrypted = driverAlternateNumber <&> unEncrypted . (.encrypted),
        Beam.driverAlternateNumberHash = driverAlternateNumber <&> (.hash),
        Beam.driverArrivalTime = driverArrivalTime,
        Beam.driverImage = driverImage,
        Beam.driverMobileCountryCode = driverMobileCountryCode,
        Beam.driverMobileNumber = driverMobileNumber,
        Beam.driverName = driverName,
        Beam.driverNumberEncrypted = driverPhoneNumber <&> unEncrypted . (.encrypted),
        Beam.driverNumberHash = driverPhoneNumber <&> (.hash),
        Beam.driverRating = driverRating,
        Beam.driverRegisteredAt = driverRegisteredAt,
        Beam.driversPreviousRideDropLat = Kernel.Prelude.fmap (.lat) driversPreviousRideDropLoc,
        Beam.driversPreviousRideDropLon = Kernel.Prelude.fmap (.lon) driversPreviousRideDropLoc,
        Beam.endOdometerReading = endOdometerReading,
        Beam.endOtp = endOtp,
        Beam.estimatedEndTimeRangeEnd = Kernel.Prelude.fmap (.end) estimatedEndTimeRange,
        Beam.estimatedEndTimeRangeStart = Kernel.Prelude.fmap (.start) estimatedEndTimeRange,
        Beam.currency = Kernel.Prelude.fmap (.currency) fare,
        Beam.fare = Kernel.Prelude.fmap (.amount) fare,
        Beam.favCount = favCount,
        Beam.feedbackSkipped = Kernel.Prelude.Just feedbackSkipped,
        Beam.hasStops = hasStops,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAlreadyFav = isAlreadyFav,
        Beam.isFreeRide = isFreeRide,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.onlinePayment = Kernel.Prelude.Just onlinePayment,
        Beam.otp = otp,
        Beam.paymentStatus = Kernel.Prelude.Just paymentStatus,
        Beam.pickupRouteCallCount = pickupRouteCallCount,
        Beam.rideEndTime = rideEndTime,
        Beam.rideRating = rideRating,
        Beam.rideStartTime = rideStartTime,
        Beam.safetyCheckStatus = safetyCheckStatus,
        Beam.safetyJourneyStatus = safetyJourneyStatus,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.showDriversPreviousRideDropLoc = Kernel.Prelude.Just showDriversPreviousRideDropLoc,
        Beam.startOdometerReading = startOdometerReading,
        Beam.status = status,
        Beam.talkedWithDriver = talkedWithDriver,
        Beam.tipAmount = Kernel.Prelude.fmap (.amount) tipAmount,
        Beam.tollConfidence = tollConfidence,
        Beam.totalFare = Kernel.Prelude.fmap (.amount) totalFare,
        Beam.trackingUrl = Kernel.Prelude.fmap Kernel.Prelude.showBaseUrl trackingUrl,
        Beam.traveledDistance = Kernel.Prelude.fmap Kernel.Types.Common.distanceToHighPrecMeters traveledDistance,
        Beam.traveledDistanceValue = Kernel.Prelude.fmap (Kernel.Types.Common.distanceToHighPrecDistance distanceUnit) traveledDistance,
        Beam.updatedAt = updatedAt,
        Beam.vehicleAge = vehicleAge,
        Beam.vehicleColor = vehicleColor,
        Beam.vehicleModel = vehicleModel,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.vehicleVariant = vehicleVariant,
        Beam.wasRideSafe = wasRideSafe
      }
