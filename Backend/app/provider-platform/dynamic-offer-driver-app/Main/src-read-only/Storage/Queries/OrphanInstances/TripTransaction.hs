{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TripTransaction where

import qualified Domain.Types.TripTransaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TripTransaction as Beam
import qualified Storage.Queries.Transformers.Ride

instance FromTType' Beam.TripTransaction Domain.Types.TripTransaction.TripTransaction where
  fromTType' (Beam.TripTransactionT {..}) = do
    pure $
      Just
        Domain.Types.TripTransaction.TripTransaction
          { allowEndingMidRoute = allowEndingMidRoute,
            createdAt = createdAt,
            deviationCount = deviationCount,
            driverId = Kernel.Types.Id.Id driverId,
            endLocation = Storage.Queries.Transformers.Ride.mkLatLong endLocationLat endLocationLon,
            endRideApprovalRequestId = Kernel.Types.Id.Id <$> endRideApprovalRequestId,
            endStopCode = endStopCode,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            isCurrentlyDeviated = isCurrentlyDeviated,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            roundRouteCode = roundRouteCode,
            routeCode = routeCode,
            startLocation = Storage.Queries.Transformers.Ride.mkLatLong startLocationLat startLocationLon,
            startedNearStopCode = startedNearStopCode,
            status = status,
            tripCode = tripCode,
            tripEndSource = tripEndSource,
            tripEndTime = tripEndTime,
            tripStartTime = tripStartTime,
            updatedAt = updatedAt,
            vehicleNumber = vehicleNumber,
            vehicleServiceTierType = vehicleServiceTierType
          }

instance ToTType' Beam.TripTransaction Domain.Types.TripTransaction.TripTransaction where
  toTType' (Domain.Types.TripTransaction.TripTransaction {..}) = do
    Beam.TripTransactionT
      { Beam.allowEndingMidRoute = allowEndingMidRoute,
        Beam.createdAt = createdAt,
        Beam.deviationCount = deviationCount,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.endLocationLat = Kernel.Prelude.fmap (.lat) endLocation,
        Beam.endLocationLon = Kernel.Prelude.fmap (.lon) endLocation,
        Beam.endRideApprovalRequestId = Kernel.Types.Id.getId <$> endRideApprovalRequestId,
        Beam.endStopCode = endStopCode,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isCurrentlyDeviated = isCurrentlyDeviated,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.roundRouteCode = roundRouteCode,
        Beam.routeCode = routeCode,
        Beam.startLocationLat = Kernel.Prelude.fmap (.lat) startLocation,
        Beam.startLocationLon = Kernel.Prelude.fmap (.lon) startLocation,
        Beam.startedNearStopCode = startedNearStopCode,
        Beam.status = status,
        Beam.tripCode = tripCode,
        Beam.tripEndSource = tripEndSource,
        Beam.tripEndTime = tripEndTime,
        Beam.tripStartTime = tripStartTime,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleServiceTierType = vehicleServiceTierType
      }
