{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardCohort where

import qualified Domain.Types.RewardCampaign
import qualified Domain.Types.RewardCohort
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RewardCohort as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardCohort.RewardCohort -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RewardCohort.RewardCohort] -> m ())
createMany = traverse_ create

findAllByCampaign :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign -> m ([Domain.Types.RewardCohort.RewardCohort]))
findAllByCampaign campaignId = do findAllWithKV [Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardCohort.RewardCohort -> m (Maybe Domain.Types.RewardCohort.RewardCohort))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardCohort.RewardCohort -> m (Maybe Domain.Types.RewardCohort.RewardCohort))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardCohort.RewardCohort -> m ())
updateByPrimaryKey (Domain.Types.RewardCohort.RewardCohort {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.campaignId (Kernel.Types.Id.getId campaignId),
      Se.Set Beam.couponValidityDays couponValidityDays,
      Se.Set Beam.description description,
      Se.Set Beam.displayOrder displayOrder,
      Se.Set Beam.eligibilityJsonLogic (Kernel.Prelude.identity eligibilityJsonLogic),
      Se.Set Beam.name name,
      Se.Set Beam.presentation (Kernel.Prelude.identity presentation),
      Se.Set Beam.rewardImageUrl rewardImageUrl,
      Se.Set Beam.rewardTitle rewardTitle,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RewardCohort Domain.Types.RewardCohort.RewardCohort where
  fromTType' (Beam.RewardCohortT {..}) = do
    pure $
      Just
        Domain.Types.RewardCohort.RewardCohort
          { campaignId = Kernel.Types.Id.Id campaignId,
            couponValidityDays = couponValidityDays,
            createdAt = createdAt,
            description = description,
            displayOrder = displayOrder,
            eligibilityJsonLogic = Kernel.Prelude.identity eligibilityJsonLogic,
            id = Kernel.Types.Id.Id id,
            name = name,
            presentation = Kernel.Prelude.identity presentation,
            rewardImageUrl = rewardImageUrl,
            rewardTitle = rewardTitle,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.RewardCohort Domain.Types.RewardCohort.RewardCohort where
  toTType' (Domain.Types.RewardCohort.RewardCohort {..}) = do
    Beam.RewardCohortT
      { Beam.campaignId = Kernel.Types.Id.getId campaignId,
        Beam.couponValidityDays = couponValidityDays,
        Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.displayOrder = displayOrder,
        Beam.eligibilityJsonLogic = Kernel.Prelude.identity eligibilityJsonLogic,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.presentation = Kernel.Prelude.identity presentation,
        Beam.rewardImageUrl = rewardImageUrl,
        Beam.rewardTitle = rewardTitle,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
