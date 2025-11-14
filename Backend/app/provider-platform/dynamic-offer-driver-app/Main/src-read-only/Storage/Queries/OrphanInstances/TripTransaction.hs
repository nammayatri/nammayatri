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
            conductorFleetBadgeId = Kernel.Types.Id.Id <$> conductorFleetBadgeId,
            conductorName = conductorName,
            createdAt = createdAt,
            deviationCount = deviationCount,
            driverFleetBadgeId = Kernel.Types.Id.Id <$> fleetBadgeId,
            driverId = Kernel.Types.Id.Id driverId,
            driverName = driverName,
            dutyType = dutyType,
            endAddress = endAddress,
            endLocation = Storage.Queries.Transformers.Ride.mkLatLong endLocationLat endLocationLon,
            endRideApprovalRequestId = Kernel.Types.Id.Id <$> endRideApprovalRequestId,
            endStopCode = endStopCode,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            isCurrentlyDeviated = isCurrentlyDeviated,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            pilotDestination = Storage.Queries.Transformers.Ride.mkLatLong pilotDestinationLat pilotDestinationLon,
            pilotSource = Storage.Queries.Transformers.Ride.mkLatLong pilotSourceLat pilotSourceLon,
            roundRouteCode = roundRouteCode,
            routeCode = routeCode,
            scheduledTripTime = scheduledTripTime,
            startAddress = startAddress,
            startLocation = Storage.Queries.Transformers.Ride.mkLatLong startLocationLat startLocationLon,
            startedNearStopCode = startedNearStopCode,
            status = status,
            tripCode = tripCode,
            tripEndTime = tripEndTime,
            tripStartSource = tripStartSource,
            tripStartTime = tripStartTime,
            tripTerminationSource = tripTerminationSource,
            tripType = tripType,
            updatedAt = updatedAt,
            vehicleNumber = vehicleNumber,
            vehicleServiceTierType = vehicleServiceTierType,
            vipName = vipName
          }

instance ToTType' Beam.TripTransaction Domain.Types.TripTransaction.TripTransaction where
  toTType' (Domain.Types.TripTransaction.TripTransaction {..}) = do
    Beam.TripTransactionT
      { Beam.allowEndingMidRoute = allowEndingMidRoute,
        Beam.conductorFleetBadgeId = Kernel.Types.Id.getId <$> conductorFleetBadgeId,
        Beam.conductorName = conductorName,
        Beam.createdAt = createdAt,
        Beam.deviationCount = deviationCount,
        Beam.fleetBadgeId = Kernel.Types.Id.getId <$> driverFleetBadgeId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverName = driverName,
        Beam.dutyType = dutyType,
        Beam.endAddress = endAddress,
        Beam.endLocationLat = Kernel.Prelude.fmap (.lat) endLocation,
        Beam.endLocationLon = Kernel.Prelude.fmap (.lon) endLocation,
        Beam.endRideApprovalRequestId = Kernel.Types.Id.getId <$> endRideApprovalRequestId,
        Beam.endStopCode = endStopCode,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isCurrentlyDeviated = isCurrentlyDeviated,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.pilotDestinationLat = Kernel.Prelude.fmap (.lat) pilotDestination,
        Beam.pilotDestinationLon = Kernel.Prelude.fmap (.lon) pilotDestination,
        Beam.pilotSourceLat = Kernel.Prelude.fmap (.lat) pilotSource,
        Beam.pilotSourceLon = Kernel.Prelude.fmap (.lon) pilotSource,
        Beam.roundRouteCode = roundRouteCode,
        Beam.routeCode = routeCode,
        Beam.scheduledTripTime = scheduledTripTime,
        Beam.startAddress = startAddress,
        Beam.startLocationLat = Kernel.Prelude.fmap (.lat) startLocation,
        Beam.startLocationLon = Kernel.Prelude.fmap (.lon) startLocation,
        Beam.startedNearStopCode = startedNearStopCode,
        Beam.status = status,
        Beam.tripCode = tripCode,
        Beam.tripEndTime = tripEndTime,
        Beam.tripStartSource = tripStartSource,
        Beam.tripStartTime = tripStartTime,
        Beam.tripTerminationSource = tripTerminationSource,
        Beam.tripType = tripType,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.vipName = vipName
      }
