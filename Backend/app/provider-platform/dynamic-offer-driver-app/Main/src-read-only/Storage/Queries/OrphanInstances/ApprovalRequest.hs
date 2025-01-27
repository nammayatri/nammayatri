{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.ApprovalRequest where

import qualified Domain.Types.ApprovalRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.ApprovalRequest as Beam

instance FromTType' Beam.ApprovalRequest Domain.Types.ApprovalRequest.ApprovalRequest where
  fromTType' (Beam.ApprovalRequestT {..}) = do
    pure $
      Just
        Domain.Types.ApprovalRequest.ApprovalRequest
          { body = body,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reason = reason,
            requestData = requestData,
            requesteeId = Kernel.Types.Id.Id requesteeId,
            requestorId = Kernel.Types.Id.Id requestorId,
            status = status,
            title = title,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ApprovalRequest Domain.Types.ApprovalRequest.ApprovalRequest where
  toTType' (Domain.Types.ApprovalRequest.ApprovalRequest {..}) = do
    Beam.ApprovalRequestT
      { Beam.body = body,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reason = reason,
        Beam.requestData = requestData,
        Beam.requesteeId = Kernel.Types.Id.getId requesteeId,
        Beam.requestorId = Kernel.Types.Id.getId requestorId,
        Beam.status = status,
        Beam.title = title,
        Beam.updatedAt = updatedAt
      }
