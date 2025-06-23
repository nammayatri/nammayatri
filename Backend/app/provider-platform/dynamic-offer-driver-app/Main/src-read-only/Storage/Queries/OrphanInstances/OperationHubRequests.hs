{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.OperationHubRequests where

import qualified Domain.Types.OperationHubRequests
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.OperationHubRequests as Beam

instance FromTType' Beam.OperationHubRequests Domain.Types.OperationHubRequests.OperationHubRequests where
  fromTType' (Beam.OperationHubRequestsT {..}) = do
    pure $
      Just
        Domain.Types.OperationHubRequests.OperationHubRequests
          { creatorId = Kernel.Types.Id.Id creatorId,
            fulfilledAt = fulfilledAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            operationHubId = Kernel.Types.Id.Id operationHubId,
            operatorId = Kernel.Types.Id.Id <$> operatorId,
            registrationNo = registrationNo,
            remarks = remarks,
            requestStatus = requestStatus,
            requestType = requestType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.OperationHubRequests Domain.Types.OperationHubRequests.OperationHubRequests where
  toTType' (Domain.Types.OperationHubRequests.OperationHubRequests {..}) = do
    Beam.OperationHubRequestsT
      { Beam.creatorId = Kernel.Types.Id.getId creatorId,
        Beam.fulfilledAt = fulfilledAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.operationHubId = Kernel.Types.Id.getId operationHubId,
        Beam.operatorId = Kernel.Types.Id.getId <$> operatorId,
        Beam.registrationNo = registrationNo,
        Beam.remarks = remarks,
        Beam.requestStatus = requestStatus,
        Beam.requestType = requestType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
