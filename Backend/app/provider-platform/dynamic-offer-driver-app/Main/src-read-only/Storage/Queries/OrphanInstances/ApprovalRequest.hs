{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.ApprovalRequest where

import qualified Domain.Types.ApprovalRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
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
            entityId = Kernel.Prelude.fromMaybe "-" entityId,
            entityType = Kernel.Prelude.fromMaybe Domain.Types.ApprovalRequest.TRIP entityType,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reason = reason,
            requestData = requestData,
            requestType = Kernel.Prelude.fromMaybe Domain.Types.ApprovalRequest.EndRideApproval requestType,
            requesteeId = Kernel.Types.Id.Id requesteeId,
            requesteeType = Kernel.Prelude.fromMaybe Domain.Types.ApprovalRequest.FleetOwner requesteeType,
            requestorId = Kernel.Types.Id.Id requestorId,
            requestorType = Kernel.Prelude.fromMaybe Domain.Types.ApprovalRequest.DriverGenerated requestorType,
            status = status,
            title = title,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ApprovalRequest Domain.Types.ApprovalRequest.ApprovalRequest where
  toTType' (Domain.Types.ApprovalRequest.ApprovalRequest {..}) = do
    Beam.ApprovalRequestT
      { Beam.body = body,
        Beam.createdAt = createdAt,
        Beam.entityId = Kernel.Prelude.Just entityId,
        Beam.entityType = Kernel.Prelude.Just entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reason = reason,
        Beam.requestData = requestData,
        Beam.requestType = Kernel.Prelude.Just requestType,
        Beam.requesteeId = Kernel.Types.Id.getId requesteeId,
        Beam.requesteeType = Kernel.Prelude.Just requesteeType,
        Beam.requestorId = Kernel.Types.Id.getId requestorId,
        Beam.requestorType = Kernel.Prelude.Just requestorType,
        Beam.status = status,
        Beam.title = title,
        Beam.updatedAt = updatedAt
      }
