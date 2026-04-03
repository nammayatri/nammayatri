{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.Route where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.Route
import qualified Storage.Beam.Route as Beam
import qualified Kernel.External.Maps.Types
import qualified Kernel.Types.Id



instance FromTType' Beam.Route Domain.Types.Route.Route
    where fromTType' (Beam.RouteT {..}) = do pure $ Just Domain.Types.Route.Route{code = code,
                                                                                  color = color,
                                                                                  endPoint = Kernel.External.Maps.Types.LatLong endLat endLon,
                                                                                  id = Kernel.Types.Id.Id id,
                                                                                  longName = longName,
                                                                                  merchantId = Kernel.Types.Id.Id merchantId,
                                                                                  merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                  polyline = polyline,
                                                                                  roundRouteCode = roundRouteCode,
                                                                                  shortName = shortName,
                                                                                  startPoint = Kernel.External.Maps.Types.LatLong startLat startLon,
                                                                                  timeBounds = timeBounds,
                                                                                  vehicleType = vehicleType,
                                                                                  createdAt = createdAt,
                                                                                  updatedAt = updatedAt}
instance ToTType' Beam.Route Domain.Types.Route.Route
    where toTType' (Domain.Types.Route.Route {..}) = do Beam.RouteT{Beam.code = code,
                                                                    Beam.color = color,
                                                                    Beam.endLat = (.lat) endPoint,
                                                                    Beam.endLon = (.lon) endPoint,
                                                                    Beam.id = Kernel.Types.Id.getId id,
                                                                    Beam.longName = longName,
                                                                    Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                    Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                    Beam.polyline = polyline,
                                                                    Beam.roundRouteCode = roundRouteCode,
                                                                    Beam.shortName = shortName,
                                                                    Beam.startLat = (.lat) startPoint,
                                                                    Beam.startLon = (.lon) startPoint,
                                                                    Beam.timeBounds = timeBounds,
                                                                    Beam.vehicleType = vehicleType,
                                                                    Beam.createdAt = createdAt,
                                                                    Beam.updatedAt = updatedAt}



