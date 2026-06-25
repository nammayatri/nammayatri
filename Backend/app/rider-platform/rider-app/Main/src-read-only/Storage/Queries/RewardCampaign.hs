{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardCampaign (module Storage.Queries.RewardCampaign, module ReExport) where

import qualified Domain.Types.RewardCampaign
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RewardCampaign as Beam
import Storage.Queries.RewardCampaignExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardCampaign.RewardCampaign -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RewardCampaign.RewardCampaign] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign -> m (Maybe Domain.Types.RewardCampaign.RewardCampaign))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign -> m (Maybe Domain.Types.RewardCampaign.RewardCampaign))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardCampaign.RewardCampaign -> m ())
updateByPrimaryKey (Domain.Types.RewardCampaign.RewardCampaign {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.claimMode claimMode,
      Se.Set Beam.couponSourceType couponSourceType,
      Se.Set Beam.couponTemplate couponTemplate,
      Se.Set Beam.createdBy createdBy,
      Se.Set Beam.description description,
      Se.Set Beam.displayOrder displayOrder,
      Se.Set Beam.endsAt endsAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.reclaimPolicy (Kernel.Prelude.identity reclaimPolicy),
      Se.Set Beam.redemptionTargetType redemptionTargetType,
      Se.Set Beam.redemptionTargetUrl redemptionTargetUrl,
      Se.Set Beam.sponsorLogoUrl sponsorLogoUrl,
      Se.Set Beam.sponsorName sponsorName,
      Se.Set Beam.sponsorType sponsorType,
      Se.Set Beam.startsAt startsAt,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
