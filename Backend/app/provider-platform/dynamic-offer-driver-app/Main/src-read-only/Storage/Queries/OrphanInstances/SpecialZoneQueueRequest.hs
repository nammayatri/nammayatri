{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.SpecialZoneQueueRequest where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.SpecialZoneQueueRequest
import qualified Storage.Beam.SpecialZoneQueueRequest as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.SpecialZoneQueueRequest Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest
    where fromTType' (Beam.SpecialZoneQueueRequestT {..}) = do pure $ Just Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest{createdAt = createdAt,
                                                                                                                                        driverId = Kernel.Types.Id.Id driverId,
                                                                                                                                        gateId = gateId,
                                                                                                                                        gateName = gateName,
                                                                                                                                        id = Kernel.Types.Id.Id id,
                                                                                                                                        merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                                        merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                        response = response,
                                                                                                                                        specialLocationId = specialLocationId,
                                                                                                                                        specialLocationName = specialLocationName,
                                                                                                                                        status = status,
                                                                                                                                        updatedAt = updatedAt,
                                                                                                                                        validTill = validTill,
                                                                                                                                        vehicleType = vehicleType}
instance ToTType' Beam.SpecialZoneQueueRequest Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest
    where toTType' (Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest {..}) = do Beam.SpecialZoneQueueRequestT{Beam.createdAt = createdAt,
                                                                                                                          Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                                          Beam.gateId = gateId,
                                                                                                                          Beam.gateName = gateName,
                                                                                                                          Beam.id = Kernel.Types.Id.getId id,
                                                                                                                          Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                          Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                          Beam.response = response,
                                                                                                                          Beam.specialLocationId = specialLocationId,
                                                                                                                          Beam.specialLocationName = specialLocationName,
                                                                                                                          Beam.status = status,
                                                                                                                          Beam.updatedAt = updatedAt,
                                                                                                                          Beam.validTill = validTill,
                                                                                                                          Beam.vehicleType = vehicleType}



