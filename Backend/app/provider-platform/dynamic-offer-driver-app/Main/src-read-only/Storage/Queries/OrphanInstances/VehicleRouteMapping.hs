{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VehicleRouteMapping where

import qualified Domain.Types.VehicleRouteMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VehicleRouteMapping as Beam

instance FromTType' Beam.VehicleRouteMapping Domain.Types.VehicleRouteMapping.VehicleRouteMapping where
  fromTType' (Beam.VehicleRouteMappingT {..}) = do
    pure $
      Just
        Domain.Types.VehicleRouteMapping.VehicleRouteMapping
          { blocked = blocked,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeCode = routeCode,
            vehicleNumber = EncryptedHashed (Encrypted vehicleNumberEncrypted) vehicleNumberHash,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleRouteMapping Domain.Types.VehicleRouteMapping.VehicleRouteMapping where
  toTType' (Domain.Types.VehicleRouteMapping.VehicleRouteMapping {..}) = do
    Beam.VehicleRouteMappingT
      { Beam.blocked = blocked,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeCode = routeCode,
        Beam.vehicleNumberEncrypted = vehicleNumber & unEncrypted . encrypted,
        Beam.vehicleNumberHash = vehicleNumber & hash,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
