{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.Station where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.Station
import qualified Storage.Beam.Station as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound



instance FromTType' Beam.Station Domain.Types.Station.Station
    where fromTType' (Beam.StationT {..}) = do pure $ Just Domain.Types.Station.Station{address = address,
                                                                                        code = code,
                                                                                        id = Kernel.Types.Id.Id id,
                                                                                        lat = lat,
                                                                                        lon = lon,
                                                                                        merchantId = Kernel.Types.Id.Id merchantId,
                                                                                        merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                        name = name,
                                                                                        possibleTypes = possibleTypes,
                                                                                        timeBounds = fromMaybe Kernel.Types.TimeBound.Unbounded timeBounds,
                                                                                        vehicleType = vehicleType,
                                                                                        createdAt = createdAt,
                                                                                        updatedAt = updatedAt}
instance ToTType' Beam.Station Domain.Types.Station.Station
    where toTType' (Domain.Types.Station.Station {..}) = do Beam.StationT{Beam.address = address,
                                                                          Beam.code = code,
                                                                          Beam.id = Kernel.Types.Id.getId id,
                                                                          Beam.lat = lat,
                                                                          Beam.lon = lon,
                                                                          Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                          Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                          Beam.name = name,
                                                                          Beam.possibleTypes = possibleTypes,
                                                                          Beam.timeBounds = Kernel.Prelude.Just timeBounds,
                                                                          Beam.vehicleType = vehicleType,
                                                                          Beam.createdAt = createdAt,
                                                                          Beam.updatedAt = updatedAt}



