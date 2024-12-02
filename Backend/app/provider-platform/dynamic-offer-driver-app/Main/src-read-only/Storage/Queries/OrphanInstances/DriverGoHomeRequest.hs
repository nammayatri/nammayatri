{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverGoHomeRequest where

import qualified Domain.Types.DriverGoHomeRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverGoHomeRequest as Beam

instance FromTType' Beam.DriverGoHomeRequest Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest where
  fromTType' (Beam.DriverGoHomeRequestT {..}) = do
    pure $
      Just
        Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest
          { createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            mbReachedHome = reachedHome,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            numCancellation = numCancellation,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverGoHomeRequest Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest where
  toTType' (Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest {..}) = do
    Beam.DriverGoHomeRequestT
      { Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.reachedHome = mbReachedHome,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.numCancellation = numCancellation,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
