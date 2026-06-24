{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardCohort (module Storage.Queries.RewardCohort, module ReExport) where

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
import Storage.Queries.RewardCohortExtra as ReExport

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
