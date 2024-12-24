{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverRequest where

import qualified Domain.Types.DriverRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverRequest as Beam

instance FromTType' Beam.DriverRequest Domain.Types.DriverRequest.DriverRequest where
  fromTType' (Beam.DriverRequestT {..}) = do
    pure $
      Just
        Domain.Types.DriverRequest.DriverRequest
          { description = description,
            id = Kernel.Types.Id.Id id,
            reason = reason,
            requestType = requestType,
            requesteeId = Kernel.Types.Id.Id requesteeId,
            requestorId = Kernel.Types.Id.Id requestorId,
            status = status,
            tripTransactionId = Kernel.Types.Id.Id tripTransactionId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverRequest Domain.Types.DriverRequest.DriverRequest where
  toTType' (Domain.Types.DriverRequest.DriverRequest {..}) = do
    Beam.DriverRequestT
      { Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.reason = reason,
        Beam.requestType = requestType,
        Beam.requesteeId = Kernel.Types.Id.getId requesteeId,
        Beam.requestorId = Kernel.Types.Id.getId requestorId,
        Beam.status = status,
        Beam.tripTransactionId = Kernel.Types.Id.getId tripTransactionId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
