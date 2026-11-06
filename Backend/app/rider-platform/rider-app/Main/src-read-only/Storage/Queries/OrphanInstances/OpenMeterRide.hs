{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.OpenMeterRide where

import qualified Domain.Types.OpenMeterRide
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.OpenMeterRide as Beam

instance FromTType' Beam.OpenMeterRide Domain.Types.OpenMeterRide.OpenMeterRide where
  fromTType' (Beam.OpenMeterRideT {..}) = do
    pure $
      Just
        Domain.Types.OpenMeterRide.OpenMeterRide
          { createdAt = createdAt,
            currency = currency,
            distanceMeters = distanceMeters,
            encodedPolyline = encodedPolyline,
            endLatLong = Kernel.External.Maps.Types.LatLong <$> endLat <*> endLon,
            endReason = endReason,
            endTime = endTime,
            estimatedGapMeters = estimatedGapMeters,
            fare = fare,
            id = Kernel.Types.Id.Id id,
            lastHeartbeatAt = lastHeartbeatAt,
            lastLatLong = Kernel.External.Maps.Types.LatLong <$> lastLat <*> lastLon,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            pointsCount = pointsCount,
            serverLinearDistanceMeters = serverLinearDistanceMeters,
            serverStartedAt = serverStartedAt,
            startLatLong = Kernel.External.Maps.Types.LatLong <$> startLat <*> startLon,
            startTime = startTime,
            status = status,
            tariffSnapshot = tariffSnapshot,
            trackingConfig = trackingConfig,
            updatedAt = updatedAt,
            waitingTimeSeconds = waitingTimeSeconds
          }

instance ToTType' Beam.OpenMeterRide Domain.Types.OpenMeterRide.OpenMeterRide where
  toTType' (Domain.Types.OpenMeterRide.OpenMeterRide {..}) = do
    Beam.OpenMeterRideT
      { Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.distanceMeters = distanceMeters,
        Beam.encodedPolyline = encodedPolyline,
        Beam.endLat = (endLatLong <&> (.lat)),
        Beam.endLon = (endLatLong <&> (.lon)),
        Beam.endReason = endReason,
        Beam.endTime = endTime,
        Beam.estimatedGapMeters = estimatedGapMeters,
        Beam.fare = fare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastHeartbeatAt = lastHeartbeatAt,
        Beam.lastLat = (lastLatLong <&> (.lat)),
        Beam.lastLon = (lastLatLong <&> (.lon)),
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.pointsCount = pointsCount,
        Beam.serverLinearDistanceMeters = serverLinearDistanceMeters,
        Beam.serverStartedAt = serverStartedAt,
        Beam.startLat = (startLatLong <&> (.lat)),
        Beam.startLon = (startLatLong <&> (.lon)),
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.tariffSnapshot = tariffSnapshot,
        Beam.trackingConfig = trackingConfig,
        Beam.updatedAt = updatedAt,
        Beam.waitingTimeSeconds = waitingTimeSeconds
      }
