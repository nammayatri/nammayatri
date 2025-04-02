{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.AlertRequest where

import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.AlertRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.AlertRequest as Beam

instance FromTType' Beam.AlertRequest Domain.Types.AlertRequest.AlertRequest where
  fromTType' (Beam.AlertRequestT {..}) = do
    pure $
      Just
        Domain.Types.AlertRequest.AlertRequest
          { body = body,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reason = reason,
            requestData = requestData,
            requestType = Kernel.Prelude.fromMaybe Domain.Types.Alert.AlertRequestType.EndRideApproval requestType,
            requesteeId = Kernel.Types.Id.Id requesteeId,
            requesteeType = Kernel.Prelude.fromMaybe Domain.Types.AlertRequest.FleetOwner requesteeType,
            requestorId = Kernel.Types.Id.Id requestorId,
            requestorType = Kernel.Prelude.fromMaybe Domain.Types.AlertRequest.DriverGenerated requestorType,
            status = status,
            title = title,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AlertRequest Domain.Types.AlertRequest.AlertRequest where
  toTType' (Domain.Types.AlertRequest.AlertRequest {..}) = do
    Beam.AlertRequestT
      { Beam.body = body,
        Beam.createdAt = createdAt,
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
