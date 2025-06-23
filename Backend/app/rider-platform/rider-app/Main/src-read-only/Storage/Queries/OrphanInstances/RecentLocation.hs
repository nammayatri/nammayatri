{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RecentLocation where

import qualified Domain.Types.RecentLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
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
            fare = fare,
            frequency = frequency,
            fromGeohash = fromGeohash,
            fromLatLong = Kernel.External.Maps.Types.LatLong <$> stopLat <*> stopLon,
            fromStopCode = fromStopCode,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            riderId = Kernel.Types.Id.Id riderId,
            routeCode = routeCode,
            toGeohash = toGeohash,
            toLatLong = Kernel.External.Maps.Types.LatLong lat lon,
            toStopCode = stopCode,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RecentLocation Domain.Types.RecentLocation.RecentLocation where
  toTType' (Domain.Types.RecentLocation.RecentLocation {..}) = do
    Beam.RecentLocationT
      { Beam.address = address,
        Beam.createdAt = createdAt,
        Beam.entityType = entityType,
        Beam.fare = fare,
        Beam.frequency = frequency,
        Beam.fromGeohash = fromGeohash,
        Beam.stopLat = fromLatLong <&> (.lat),
        Beam.stopLon = fromLatLong <&> (.lon),
        Beam.fromStopCode = fromStopCode,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeCode = routeCode,
        Beam.toGeohash = toGeohash,
        Beam.lat = (.lat) toLatLong,
        Beam.lon = (.lon) toLatLong,
        Beam.stopCode = toStopCode,
        Beam.updatedAt = updatedAt
      }
