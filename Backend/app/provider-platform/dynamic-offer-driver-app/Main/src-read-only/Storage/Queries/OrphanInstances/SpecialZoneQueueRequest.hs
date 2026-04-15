{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SpecialZoneQueueRequest where

import qualified Domain.Types.SpecialZoneQueueRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SpecialZoneQueueRequest as Beam

instance FromTType' Beam.SpecialZoneQueueRequest Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest where
  fromTType' (Beam.SpecialZoneQueueRequestT {..}) = do
    pure $
      Just
        Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest
          { arrivalDeadlineTime = arrivalDeadlineTime,
            createdAt = createdAt,
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
            vehicleType = vehicleType
          }

instance ToTType' Beam.SpecialZoneQueueRequest Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest where
  toTType' (Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest {..}) = do
    Beam.SpecialZoneQueueRequestT
      { Beam.arrivalDeadlineTime = arrivalDeadlineTime,
        Beam.createdAt = createdAt,
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
        Beam.vehicleType = vehicleType
      }
