{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VehicleTrip where

import qualified Domain.Types.VehicleTrip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VehicleTrip as Beam

instance FromTType' Beam.VehicleTrip Domain.Types.VehicleTrip.VehicleTrip where
  fromTType' (Beam.VehicleTripT {..}) = do
    pure $
      Just
        Domain.Types.VehicleTrip.VehicleTrip
          { capacity = capacity,
            createdAt = createdAt,
            driverId = driverId,
            endReason = endReason,
            endedAt = endedAt,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            missedPickups = missedPickups,
            movingAt = movingAt,
            offlineBoardings = offlineBoardings,
            reachedEndAt = reachedEndAt,
            routeCode = routeCode,
            serviceTierType = serviceTierType,
            startedAt = startedAt,
            status = status,
            updatedAt = updatedAt,
            vehicleNumber = vehicleNumber
          }

instance ToTType' Beam.VehicleTrip Domain.Types.VehicleTrip.VehicleTrip where
  toTType' (Domain.Types.VehicleTrip.VehicleTrip {..}) = do
    Beam.VehicleTripT
      { Beam.capacity = capacity,
        Beam.createdAt = createdAt,
        Beam.driverId = driverId,
        Beam.endReason = endReason,
        Beam.endedAt = endedAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.missedPickups = missedPickups,
        Beam.movingAt = movingAt,
        Beam.offlineBoardings = offlineBoardings,
        Beam.reachedEndAt = reachedEndAt,
        Beam.routeCode = routeCode,
        Beam.serviceTierType = serviceTierType,
        Beam.startedAt = startedAt,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNumber = vehicleNumber
      }
