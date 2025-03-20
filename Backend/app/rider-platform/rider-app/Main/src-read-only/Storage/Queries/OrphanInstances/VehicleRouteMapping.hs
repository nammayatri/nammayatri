{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VehicleRouteMapping where

import qualified Domain.Types.VehicleRouteMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VehicleRouteMapping as Beam

instance FromTType' Beam.VehicleRouteMapping Domain.Types.VehicleRouteMapping.VehicleRouteMapping where
  fromTType' (Beam.VehicleRouteMappingT {..}) = do
    pure $
      Just
        Domain.Types.VehicleRouteMapping.VehicleRouteMapping
          { createdAt = createdAt,
            routeId = routeId,
            service = service,
            shift = shift,
            typeOfService = typeOfService,
            updatedAt = updatedAt,
            vehicleNo = vehicleNo
          }

instance ToTType' Beam.VehicleRouteMapping Domain.Types.VehicleRouteMapping.VehicleRouteMapping where
  toTType' (Domain.Types.VehicleRouteMapping.VehicleRouteMapping {..}) = do
    Beam.VehicleRouteMappingT
      { Beam.createdAt = createdAt,
        Beam.routeId = routeId,
        Beam.service = service,
        Beam.shift = shift,
        Beam.typeOfService = typeOfService,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNo = vehicleNo
      }
