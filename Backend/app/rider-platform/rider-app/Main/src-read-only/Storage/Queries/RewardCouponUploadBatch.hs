{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardCouponUploadBatch where

import qualified Domain.Types.RewardCampaign
import qualified Domain.Types.RewardCouponUploadBatch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RewardCouponUploadBatch as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch] -> m ())
createMany = traverse_ create

findByCampaign :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign -> m ([Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch]))
findByCampaign campaignId = do findAllWithKV [Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch -> m (Maybe Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch -> m ())
updateByPrimaryKey (Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.campaignId (Kernel.Types.Id.getId campaignId),
      Se.Set Beam.cohortId (Kernel.Types.Id.getId cohortId),
      Se.Set Beam.s3Key s3Key,
      Se.Set Beam.totalCodesUploaded totalCodesUploaded,
      Se.Set Beam.uploadBatchId uploadBatchId,
      Se.Set Beam.uploadedAt uploadedAt,
      Se.Set Beam.uploadedBy uploadedBy,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

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
