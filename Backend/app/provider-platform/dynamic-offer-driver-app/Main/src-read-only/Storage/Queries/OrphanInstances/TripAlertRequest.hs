{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TripAlertRequest where

import qualified Domain.Types.TripAlertRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TripAlertRequest as Beam

instance FromTType' Beam.TripAlertRequest Domain.Types.TripAlertRequest.TripAlertRequest where
  fromTType' (Beam.TripAlertRequestT {..}) = do
    pure $
      Just
        Domain.Types.TripAlertRequest.TripAlertRequest
          { alertRequestId = Kernel.Types.Id.Id alertRequestId,
            alertRequestType = alertRequestType,
            alertStatus = alertStatus,
            conductorFleetBadgeId = Kernel.Types.Id.Id <$> conductorFleetBadgeId,
            createdAt = createdAt,
            driverFleetBadgeId = Kernel.Types.Id.Id <$> fleetBadgeId,
            driverId = Kernel.Types.Id.Id driverId,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            isViolated = isViolated,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeCode = routeCode,
            tripTransactionId = Kernel.Types.Id.Id tripTransactionId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TripAlertRequest Domain.Types.TripAlertRequest.TripAlertRequest where
  toTType' (Domain.Types.TripAlertRequest.TripAlertRequest {..}) = do
    Beam.TripAlertRequestT
      { Beam.alertRequestId = Kernel.Types.Id.getId alertRequestId,
        Beam.alertRequestType = alertRequestType,
        Beam.alertStatus = alertStatus,
        Beam.conductorFleetBadgeId = Kernel.Types.Id.getId <$> conductorFleetBadgeId,
        Beam.createdAt = createdAt,
        Beam.fleetBadgeId = Kernel.Types.Id.getId <$> driverFleetBadgeId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isViolated = isViolated,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeCode = routeCode,
        Beam.tripTransactionId = Kernel.Types.Id.getId tripTransactionId,
        Beam.updatedAt = updatedAt
      }
