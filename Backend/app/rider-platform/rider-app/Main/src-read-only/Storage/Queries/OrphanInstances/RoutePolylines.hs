{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.RoutePolylines where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.RoutePolylines
import qualified Storage.Beam.RoutePolylines as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.RoutePolylines Domain.Types.RoutePolylines.RoutePolylines
    where fromTType' (Beam.RoutePolylinesT {..}) = do pure $ Just Domain.Types.RoutePolylines.RoutePolylines{createdAt = createdAt,
                                                                                                             id = Kernel.Types.Id.Id id,
                                                                                                             merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                             merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                             polyline = polyline,
                                                                                                             routeId = routeId,
                                                                                                             updatedAt = updatedAt,
                                                                                                             vehicleType = vehicleType}
instance ToTType' Beam.RoutePolylines Domain.Types.RoutePolylines.RoutePolylines
    where toTType' (Domain.Types.RoutePolylines.RoutePolylines {..}) = do Beam.RoutePolylinesT{Beam.createdAt = createdAt,
                                                                                               Beam.id = Kernel.Types.Id.getId id,
                                                                                               Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                               Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                               Beam.polyline = polyline,
                                                                                               Beam.routeId = routeId,
                                                                                               Beam.updatedAt = updatedAt,
                                                                                               Beam.vehicleType = vehicleType}



