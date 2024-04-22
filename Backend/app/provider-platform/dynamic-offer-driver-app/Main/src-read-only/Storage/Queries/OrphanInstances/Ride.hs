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
import Storage.Queries.Transformers.Ride
import qualified Storage.Queries.Transformers.Ride

instance FromTType' Beam.Ride Domain.Types.Ride.Ride where
  fromTType' (Beam.RideT {..}) = do
    fromLocation' <- Storage.Queries.Transformers.Ride.getFromLocation id bookingId merchantId merchantOperatingCityId
    merchantOperatingCityId' <- Storage.Queries.Transformers.Ride.getMerchantOperatingCityId bookingId merchantId merchantOperatingCityId
    toLocation' <- Storage.Queries.Transformers.Ride.getToLocation id bookingId merchantId merchantOperatingCityId
    trackingUrl' <- Kernel.Prelude.parseBaseUrl trackingUrl
    pure $
      Just
        Domain.Types.Ride.Ride
          { bookingId = Kernel.Types.Id.Id bookingId,
            chargeableDistance = chargeableDistance,
            clientId = Kernel.Types.Id.Id <$> clientId,
            createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            distanceCalculationFailed = distanceCalculationFailed,
            driverArrivalTime = driverArrivalTime,
            driverDeviatedFromRoute = driverDeviatedFromRoute,
            driverGoHomeRequestId = Kernel.Types.Id.Id <$> driverGoHomeRequestId,
            driverId = Kernel.Types.Id.Id driverId,
            enableFrequentLocationUpdates = enableFrequentLocationUpdates,
            endOdometerReading = Storage.Queries.Transformers.Ride.mkOdometerReading endOdometerReadingFileId endOdometerReadingValue,
            endOtp = endOtp,
            fare = fmap (Kernel.Types.Common.mkAmountWithDefault fareAmount) fare,
            fareParametersId = Kernel.Types.Id.Id <$> fareParametersId,
            fromLocation = fromLocation',
            id = Kernel.Types.Id.Id id,
            isFreeRide = isFreeRide,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            numberOfDeviation = numberOfDeviation,
            numberOfOsrmSnapToRoadCalls = numberOfOsrmSnapToRoadCalls,
            numberOfSnapToRoadCalls = numberOfSnapToRoadCalls,
            otp = otp,
            pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
            rideEndedBy = rideEndedBy,
            safetyAlertTriggered = safetyAlertTriggered,
            shortId = Kernel.Types.Id.ShortId shortId,
            startOdometerReading = Storage.Queries.Transformers.Ride.mkOdometerReading startOdometerReadingFileId startOdometerReadingValue,
            status = status,
            toLocation = toLocation',
            tollCharges = tollCharges,
            trackingUrl = trackingUrl',
            traveledDistance = traveledDistance,
            tripEndPos = Storage.Queries.Transformers.Ride.mkLatLong tripEndLat tripEndLon,
            tripEndTime = tripEndTime,
            tripStartPos = Storage.Queries.Transformers.Ride.mkLatLong tripStartLat tripStartLon,
            tripStartTime = tripStartTime,
            uiDistanceCalculationWithAccuracy = uiDistanceCalculationWithAccuracy,
            uiDistanceCalculationWithoutAccuracy = uiDistanceCalculationWithoutAccuracy,
            updatedAt = updatedAt,
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity
          }

instance ToTType' Beam.Ride Domain.Types.Ride.Ride where
  toTType' (Domain.Types.Ride.Ride {..}) = do
    Beam.RideT
      { Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.chargeableDistance = chargeableDistance,
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distanceCalculationFailed = distanceCalculationFailed,
        Beam.driverArrivalTime = driverArrivalTime,
        Beam.driverDeviatedFromRoute = driverDeviatedFromRoute,
        Beam.driverGoHomeRequestId = Kernel.Types.Id.getId <$> driverGoHomeRequestId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.enableFrequentLocationUpdates = enableFrequentLocationUpdates,
        Beam.endOdometerReadingFileId = getEndOdometerReadingFileId endOdometerReading,
        Beam.endOdometerReadingValue = Kernel.Prelude.fmap Domain.Types.Ride.value endOdometerReading,
        Beam.endOtp = endOtp,
        Beam.fare = Kernel.Prelude.fmap roundToIntegral fare,
        Beam.fareAmount = fare,
        Beam.fareParametersId = Kernel.Types.Id.getId <$> fareParametersId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isFreeRide = isFreeRide,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Prelude.Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.numberOfDeviation = numberOfDeviation,
        Beam.numberOfOsrmSnapToRoadCalls = numberOfOsrmSnapToRoadCalls,
        Beam.numberOfSnapToRoadCalls = numberOfSnapToRoadCalls,
        Beam.otp = otp,
        Beam.pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
        Beam.rideEndedBy = rideEndedBy,
        Beam.safetyAlertTriggered = safetyAlertTriggered,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.startOdometerReadingFileId = getStartOdometerReadingFileId startOdometerReading,
        Beam.startOdometerReadingValue = Kernel.Prelude.fmap Domain.Types.Ride.value startOdometerReading,
        Beam.status = status,
        Beam.tollCharges = tollCharges,
        Beam.trackingUrl = Kernel.Prelude.showBaseUrl trackingUrl,
        Beam.traveledDistance = traveledDistance,
        Beam.tripEndLat = Kernel.Prelude.fmap (.lat) tripEndPos,
        Beam.tripEndLon = Kernel.Prelude.fmap (.lon) tripEndPos,
        Beam.tripEndTime = tripEndTime,
        Beam.tripStartLat = Kernel.Prelude.fmap (.lat) tripStartPos,
        Beam.tripStartLon = Kernel.Prelude.fmap (.lon) tripStartPos,
        Beam.tripStartTime = tripStartTime,
        Beam.uiDistanceCalculationWithAccuracy = uiDistanceCalculationWithAccuracy,
        Beam.uiDistanceCalculationWithoutAccuracy = uiDistanceCalculationWithoutAccuracy,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity
      }
