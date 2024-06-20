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
    fromLocation' <- Storage.Queries.Extra.Transformers.Ride.getFromLocation id bookingId merchantId merchantOperatingCityId
    toLocation' <- Storage.Queries.Extra.Transformers.Ride.getToLocation id bookingId merchantId merchantOperatingCityId
    trackingUrl' <- Kernel.Prelude.mapM Kernel.Prelude.parseBaseUrl trackingUrl
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    pure $
      Just
        Domain.Types.Ride.Ride
          { id = Kernel.Types.Id.Id id,
            bppRideId = Kernel.Types.Id.Id bppRideId,
            bookingId = Kernel.Types.Id.Id bookingId,
            shortId = Kernel.Types.Id.ShortId shortId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            clientId = Kernel.Types.Id.Id <$> clientId,
            fromLocation = fromLocation',
            toLocation = toLocation',
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            status = status,
            driverName = driverName,
            driverRating = driverRating,
            driverMobileNumber = driverMobileNumber,
            driverMobileCountryCode = driverMobileCountryCode,
            driverRegisteredAt = driverRegisteredAt,
            driverImage = driverImage,
            vehicleNumber = vehicleNumber,
            vehicleModel = vehicleModel,
            vehicleColor = vehicleColor,
            vehicleVariant = vehicleVariant,
            vehicleServiceTierType = vehicleServiceTierType,
            otp = otp,
            endOtp = endOtp,
            trackingUrl = trackingUrl',
            fare = fmap (Kernel.Types.Common.mkPrice currency) fare,
            totalFare = fmap (Kernel.Types.Common.mkPrice currency) totalFare,
            chargeableDistance = Kernel.Types.Common.mkDistanceWithDefault distanceUnit chargeableDistanceValue <$> chargeableDistance,
            traveledDistance = Kernel.Types.Common.mkDistanceWithDefault distanceUnit traveledDistanceValue <$> traveledDistance,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverArrivalTime = driverArrivalTime,
            rideStartTime = rideStartTime,
            rideEndTime = rideEndTime,
            rideRating = rideRating,
            allowedEditLocationAttempts = allowedEditLocationAttempts,
            driversPreviousRideDropLoc = Storage.Queries.Extra.Transformers.Ride.mkLatLong driversPreviousRideDropLat driversPreviousRideDropLon,
            startOdometerReading = startOdometerReading,
            endOdometerReading = endOdometerReading,
            isFreeRide = isFreeRide,
            createdAt = createdAt,
            updatedAt = updatedAt,
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            clientBundleVersion = clientBundleVersion',
            clientSdkVersion = clientSdkVersion',
            clientConfigVersion = clientConfigVersion',
            backendConfigVersion = backendConfigVersion',
            backendAppVersion = backendAppVersion,
            safetyCheckStatus = safetyCheckStatus,
            showDriversPreviousRideDropLoc = Kernel.Prelude.fromMaybe False showDriversPreviousRideDropLoc,
            paymentDone = Kernel.Prelude.fromMaybe True paymentDone,
            driverAccountId = driverAccountId,
            tollConfidence = tollConfidence
          }

instance ToTType' Beam.Ride Domain.Types.Ride.Ride where
  toTType' (Domain.Types.Ride.Ride {..}) = do
    Beam.RideT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.bppRideId = Kernel.Types.Id.getId bppRideId,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.status = status,
        Beam.driverName = driverName,
        Beam.driverRating = driverRating,
        Beam.driverMobileNumber = driverMobileNumber,
        Beam.driverMobileCountryCode = driverMobileCountryCode,
        Beam.driverRegisteredAt = driverRegisteredAt,
        Beam.driverImage = driverImage,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleModel = vehicleModel,
        Beam.vehicleColor = vehicleColor,
        Beam.vehicleVariant = vehicleVariant,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.otp = otp,
        Beam.endOtp = endOtp,
        Beam.trackingUrl = Kernel.Prelude.fmap Kernel.Prelude.showBaseUrl trackingUrl,
        Beam.currency = Kernel.Prelude.fmap (.currency) fare,
        Beam.fare = Kernel.Prelude.fmap (.amount) fare,
        Beam.totalFare = Kernel.Prelude.fmap (.amount) totalFare,
        Beam.chargeableDistance = Kernel.Prelude.fmap Kernel.Types.Common.distanceToHighPrecMeters chargeableDistance,
        Beam.chargeableDistanceValue = Kernel.Prelude.fmap (Kernel.Types.Common.distanceToHighPrecDistance distanceUnit) chargeableDistance,
        Beam.traveledDistance = Kernel.Prelude.fmap Kernel.Types.Common.distanceToHighPrecMeters traveledDistance,
        Beam.traveledDistanceValue = Kernel.Prelude.fmap (Kernel.Types.Common.distanceToHighPrecDistance distanceUnit) traveledDistance,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverArrivalTime = driverArrivalTime,
        Beam.rideStartTime = rideStartTime,
        Beam.rideEndTime = rideEndTime,
        Beam.rideRating = rideRating,
        Beam.allowedEditLocationAttempts = allowedEditLocationAttempts,
        Beam.driversPreviousRideDropLat = Kernel.Prelude.fmap (.lat) driversPreviousRideDropLoc,
        Beam.driversPreviousRideDropLon = Kernel.Prelude.fmap (.lon) driversPreviousRideDropLoc,
        Beam.startOdometerReading = startOdometerReading,
        Beam.endOdometerReading = endOdometerReading,
        Beam.isFreeRide = isFreeRide,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.backendAppVersion = backendAppVersion,
        Beam.safetyCheckStatus = safetyCheckStatus,
        Beam.showDriversPreviousRideDropLoc = Kernel.Prelude.Just showDriversPreviousRideDropLoc,
        Beam.paymentDone = Kernel.Prelude.Just paymentDone,
        Beam.driverAccountId = driverAccountId,
        Beam.tollConfidence = tollConfidence
      }
