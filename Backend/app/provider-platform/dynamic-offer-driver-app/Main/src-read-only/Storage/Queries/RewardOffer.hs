{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardOffer (module Storage.Queries.RewardOffer, module ReExport) where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RewardOffer
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RewardOffer as Beam
import Storage.Queries.RewardOfferExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardOffer.RewardOffer -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RewardOffer.RewardOffer] -> m ())
createMany = traverse_ create

findAllActiveByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.RewardOffer.RewardOffer])
findAllActiveByMerchantOpCityId merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.active $ Se.Eq True
        ]
    ]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardOffer.RewardOffer -> m (Maybe Domain.Types.RewardOffer.RewardOffer))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RewardOffer.RewardOffer -> m (Maybe Domain.Types.RewardOffer.RewardOffer))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RewardOffer.RewardOffer -> m ())
updateByPrimaryKey (Domain.Types.RewardOffer.RewardOffer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.active active,
      Se.Set Beam.description description,
      Se.Set Beam.displayOrder displayOrder,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.imageUrl imageUrl,
      Se.Set Beam.logicDomain logicDomain,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.milestoneTarget milestoneTarget,
      Se.Set Beam.requiredTags requiredTags,
      Se.Set Beam.title title,
      Se.Set Beam.triggerEvent triggerEvent,
      Se.Set Beam.validFrom validFrom,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
