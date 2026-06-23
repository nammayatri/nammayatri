{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RewardCouponUploadBatch where

import qualified Domain.Types.RewardCampaign
import qualified Domain.Types.RewardCohort
import qualified Domain.Types.RewardCouponUploadBatch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RewardCouponUploadBatch as Beam

instance FromTType' Beam.RewardCouponUploadBatch Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch where
  fromTType' (Beam.RewardCouponUploadBatchT {..}) = do
    pure $
      Just
        Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch
          { campaignId = Kernel.Types.Id.Id campaignId,
            cohortId = Kernel.Types.Id.Id cohortId,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            s3Key = s3Key,
            totalCodesUploaded = totalCodesUploaded,
            uploadBatchId = uploadBatchId,
            uploadedAt = uploadedAt,
            uploadedBy = uploadedBy,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RewardCouponUploadBatch Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch where
  toTType' (Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch {..}) = do
    Beam.RewardCouponUploadBatchT
      { Beam.campaignId = Kernel.Types.Id.getId campaignId,
        Beam.cohortId = Kernel.Types.Id.getId cohortId,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.s3Key = s3Key,
        Beam.totalCodesUploaded = totalCodesUploaded,
        Beam.uploadBatchId = uploadBatchId,
        Beam.uploadedAt = uploadedAt,
        Beam.uploadedBy = uploadedBy,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
