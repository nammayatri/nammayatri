{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Route where

import qualified Domain.Types.Route
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Route as Beam

instance FromTType' Beam.Route Domain.Types.Route.Route where
  fromTType' (Beam.RouteT {..}) = do
    pure $
      Just
        Domain.Types.Route.Route
          { code = code,
            color = color,
            endPoint = Kernel.External.Maps.Types.LatLong endLat endLon,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            longName = longName,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            polyline = polyline,
            shortName = shortName,
            startPoint = Kernel.External.Maps.Types.LatLong startLat startLon,
            timeBounds = timeBounds,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Route Domain.Types.Route.Route where
  toTType' (Domain.Types.Route.Route {..}) = do
    Beam.RouteT
      { Beam.code = code,
        Beam.color = color,
        Beam.endLat = (.lat) endPoint,
        Beam.endLon = (.lon) endPoint,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.longName = longName,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.polyline = polyline,
        Beam.shortName = shortName,
        Beam.startLat = (.lat) startPoint,
        Beam.startLon = (.lon) startPoint,
        Beam.timeBounds = timeBounds,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
