{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CachedRouteResponse where

import qualified Domain.Types.CachedRouteResponse
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.CachedRouteResponse as Beam

instance FromTType' Beam.CachedRouteResponse Domain.Types.CachedRouteResponse.CachedRouteResponse where
  fromTType' (Beam.CachedRouteResponseT {..}) = do
    pure $
      Just
        Domain.Types.CachedRouteResponse.CachedRouteResponse
          { avoidToll = avoidToll,
            createdAt = createdAt,
            distance = distance,
            dropGeohash = dropGeohash,
            duration = duration,
            hourOfDay = hourOfDay,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            pickupGeohash = pickupGeohash,
            riderId = Kernel.Types.Id.Id riderId,
            routes = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< routes),
            updatedAt = updatedAt
          }

instance ToTType' Beam.CachedRouteResponse Domain.Types.CachedRouteResponse.CachedRouteResponse where
  toTType' (Domain.Types.CachedRouteResponse.CachedRouteResponse {..}) = do
    Beam.CachedRouteResponseT
      { Beam.avoidToll = avoidToll,
        Beam.createdAt = createdAt,
        Beam.distance = distance,
        Beam.dropGeohash = dropGeohash,
        Beam.duration = duration,
        Beam.hourOfDay = hourOfDay,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.pickupGeohash = pickupGeohash,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routes = Just $ toJSON routes,
        Beam.updatedAt = updatedAt
      }
