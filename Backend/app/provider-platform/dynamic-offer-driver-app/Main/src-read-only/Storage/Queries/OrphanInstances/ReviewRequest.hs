{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.ReviewRequest where

import qualified Data.Aeson
import qualified Domain.Types.ReviewRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.ReviewRequest as Beam

instance FromTType' Beam.ReviewRequest Domain.Types.ReviewRequest.ReviewRequest where
  fromTType' (Beam.ReviewRequestT {..}) = do
    pure $
      Just
        Domain.Types.ReviewRequest.ReviewRequest
          { createdAt = createdAt,
            documentDetails = Kernel.Utils.JSON.valueToMaybe =<< documentDetails,
            entityId = entityId,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rcNo = rcNo,
            requestStatus = requestStatus,
            requestType = requestType,
            reviewerId = Kernel.Types.Id.Id <$> reviewerId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ReviewRequest Domain.Types.ReviewRequest.ReviewRequest where
  toTType' (Domain.Types.ReviewRequest.ReviewRequest {..}) = do
    Beam.ReviewRequestT
      { Beam.createdAt = createdAt,
        Beam.documentDetails = Data.Aeson.toJSON <$> documentDetails,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rcNo = rcNo,
        Beam.requestStatus = requestStatus,
        Beam.requestType = requestType,
        Beam.reviewerId = Kernel.Types.Id.getId <$> reviewerId,
        Beam.updatedAt = updatedAt
      }
