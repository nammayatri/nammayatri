{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardUnlock (module Storage.Queries.RewardUnlock, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.RewardCampaign
import qualified Domain.Types.RewardUnlock
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RewardUnlock as Beam
import Storage.Queries.RewardUnlockExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardUnlock.RewardUnlock -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RewardUnlock.RewardUnlock] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardUnlock.RewardUnlock -> m (Maybe Domain.Types.RewardUnlock.RewardUnlock))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPerson :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.RewardUnlock.RewardUnlock]))
findByPerson personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPersonAndCampaign ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign -> m ([Domain.Types.RewardUnlock.RewardUnlock]))
findByPersonAndCampaign personId campaignId = do findAllWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId)]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardUnlock.RewardUnlock -> m (Maybe Domain.Types.RewardUnlock.RewardUnlock))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardUnlock.RewardUnlock -> m ())
updateByPrimaryKey (Domain.Types.RewardUnlock.RewardUnlock {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.campaignId (Kernel.Types.Id.getId campaignId),
      Se.Set Beam.claimedAt claimedAt,
      Se.Set Beam.cohortId (Kernel.Types.Id.getId cohortId),
      Se.Set Beam.couponCode couponCode,
      Se.Set Beam.couponSource couponSource,
      Se.Set Beam.couponValidTill couponValidTill,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.reclaimedAt reclaimedAt,
      Se.Set Beam.redeemedAt redeemedAt,
      Se.Set Beam.status status,
      Se.Set Beam.unlockSeq unlockSeq,
      Se.Set Beam.unlockedAt unlockedAt,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.viewedAt viewedAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
