{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.AdditionalInfoRequest where

import qualified Domain.Types.AdditionalInfoRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.AdditionalInfoRequest as Beam

instance FromTType' Beam.AdditionalInfoRequest Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest where
  fromTType' (Beam.AdditionalInfoRequestT {..}) = do
    pure $
      Just
        Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest
          { id = Kernel.Types.Id.Id id,
            operationHubRequestId = Kernel.Types.Id.Id operationHubRequestId,
            requestedBy = Kernel.Types.Id.Id requestedBy,
            requestedFrom = Kernel.Types.Id.Id requestedFrom,
            requestedDocumentTypes = requestedDocumentTypes,
            message = message,
            status = status,
            responseRemarks = responseRemarks,
            responseDocumentIds = responseDocumentIds,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AdditionalInfoRequest Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest where
  toTType' (Domain.Types.AdditionalInfoRequest.AdditionalInfoRequest {..}) = do
    Beam.AdditionalInfoRequestT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.operationHubRequestId = Kernel.Types.Id.getId operationHubRequestId,
        Beam.requestedBy = Kernel.Types.Id.getId requestedBy,
        Beam.requestedFrom = Kernel.Types.Id.getId requestedFrom,
        Beam.requestedDocumentTypes = requestedDocumentTypes,
        Beam.message = message,
        Beam.status = status,
        Beam.responseRemarks = responseRemarks,
        Beam.responseDocumentIds = responseDocumentIds,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
