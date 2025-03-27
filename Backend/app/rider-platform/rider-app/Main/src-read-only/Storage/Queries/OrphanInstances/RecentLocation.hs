{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RecentLocation where

import qualified Domain.Types.RecentLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RecentLocation as Beam

instance FromTType' Beam.RecentLocation Domain.Types.RecentLocation.RecentLocation where
  fromTType' (Beam.RecentLocationT {..}) = do
    pure $
      Just
        Domain.Types.RecentLocation.RecentLocation
          { address = address,
            createdAt = createdAt,
            entityType = entityType,
            frequency = frequency,
            fromStopCode = fromStopCode,
            fromStopName = fromStopName,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            riderId = Kernel.Types.Id.Id riderId,
            routeCode = routeCode,
            routeId = routeId,
            stopCode = stopCode,
            stopLat = stopLat,
            stopLon = stopLon,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RecentLocation Domain.Types.RecentLocation.RecentLocation where
  toTType' (Domain.Types.RecentLocation.RecentLocation {..}) = do
    Beam.RecentLocationT
      { Beam.address = address,
        Beam.createdAt = createdAt,
        Beam.entityType = entityType,
        Beam.frequency = frequency,
        Beam.fromStopCode = fromStopCode,
        Beam.fromStopName = fromStopName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeCode = routeCode,
        Beam.routeId = routeId,
        Beam.stopCode = stopCode,
        Beam.stopLat = stopLat,
        Beam.stopLon = stopLon,
        Beam.updatedAt = updatedAt
      }
