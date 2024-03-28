{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Ride where

import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Ride as Beam
import qualified Storage.Queries.Extra.Transformers.Ride

instance FromTType' Beam.Ride Domain.Types.Ride.Ride where
  fromTType' (Beam.RideT {..}) = do
    fromLocation' <- Storage.Queries.Extra.Transformers.Ride.getFromLocation id bookingId merchantId merchantOperatingCityId
    toLocation' <- Storage.Queries.Extra.Transformers.Ride.getToLocation id bookingId merchantId merchantOperatingCityId
    trackingUrl' <- Kernel.Prelude.mapM Kernel.Prelude.parseBaseUrl trackingUrl
    pure $
      Just
        Domain.Types.Ride.Ride
          { allowedEditLocationAttempts = allowedEditLocationAttempts,
            bookingId = Kernel.Types.Id.Id bookingId,
            bppRideId = Kernel.Types.Id.Id bppRideId,
            chargeableDistance = chargeableDistance,
            clientId = Kernel.Types.Id.Id <$> clientId,
            createdAt = createdAt,
            driverArrivalTime = driverArrivalTime,
            driverImage = driverImage,
            driverMobileCountryCode = driverMobileCountryCode,
            driverMobileNumber = driverMobileNumber,
            driverName = driverName,
            driverRating = driverRating,
            driverRegisteredAt = driverRegisteredAt,
            endOdometerReading = endOdometerReading,
            endOtp = endOtp,
            fare = fmap (Kernel.Types.Common.mkPrice currency) fare,
            fromLocation = fromLocation',
            id = Kernel.Types.Id.Id id,
            isFreeRide = isFreeRide,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            otp = otp,
            rideEndTime = rideEndTime,
            rideRating = rideRating,
            rideStartTime = rideStartTime,
            safetyCheckStatus = safetyCheckStatus,
            shortId = Kernel.Types.Id.ShortId shortId,
            startOdometerReading = startOdometerReading,
            status = status,
            toLocation = toLocation',
            totalFare = fmap (Kernel.Types.Common.mkPrice currency) totalFare,
            trackingUrl = trackingUrl',
            traveledDistance = traveledDistance,
            updatedAt = updatedAt,
            vehicleColor = vehicleColor,
            vehicleModel = vehicleModel,
            vehicleNumber = vehicleNumber,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.Ride Domain.Types.Ride.Ride where
  toTType' (Domain.Types.Ride.Ride {..}) = do
    Beam.RideT
      { Beam.allowedEditLocationAttempts = allowedEditLocationAttempts,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.bppRideId = Kernel.Types.Id.getId bppRideId,
        Beam.chargeableDistance = chargeableDistance,
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.createdAt = createdAt,
        Beam.driverArrivalTime = driverArrivalTime,
        Beam.driverImage = driverImage,
        Beam.driverMobileCountryCode = driverMobileCountryCode,
        Beam.driverMobileNumber = driverMobileNumber,
        Beam.driverName = driverName,
        Beam.driverRating = driverRating,
        Beam.driverRegisteredAt = driverRegisteredAt,
        Beam.endOdometerReading = endOdometerReading,
        Beam.endOtp = endOtp,
        Beam.currency = Kernel.Prelude.fmap (.currency) fare,
        Beam.fare = Kernel.Prelude.fmap (.amount) fare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isFreeRide = isFreeRide,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.otp = otp,
        Beam.rideEndTime = rideEndTime,
        Beam.rideRating = rideRating,
        Beam.rideStartTime = rideStartTime,
        Beam.safetyCheckStatus = safetyCheckStatus,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.startOdometerReading = startOdometerReading,
        Beam.status = status,
        Beam.totalFare = Kernel.Prelude.fmap (.amount) totalFare,
        Beam.trackingUrl = Kernel.Prelude.fmap Kernel.Prelude.showBaseUrl trackingUrl,
        Beam.traveledDistance = traveledDistance,
        Beam.updatedAt = updatedAt,
        Beam.vehicleColor = vehicleColor,
        Beam.vehicleModel = vehicleModel,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleVariant = vehicleVariant
      }
