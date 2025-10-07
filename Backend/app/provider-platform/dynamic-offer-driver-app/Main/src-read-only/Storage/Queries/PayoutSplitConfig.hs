{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutSplitConfig where

import qualified Domain.Types.PayoutSplitConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PayoutSplitConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutSplitConfig.PayoutSplitConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PayoutSplitConfig.PayoutSplitConfig] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PayoutSplitConfig.PayoutSplitConfig -> m (Maybe Domain.Types.PayoutSplitConfig.PayoutSplitConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutSplitConfig.PayoutSplitConfig -> m ())
updateByPrimaryKey (Domain.Types.PayoutSplitConfig.PayoutSplitConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.area area,
      Se.Set Beam.bankDetails bankDetails,
      Se.Set Beam.vendorId vendorId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PayoutSplitConfig Domain.Types.PayoutSplitConfig.PayoutSplitConfig where
  fromTType' (Beam.PayoutSplitConfigT {..}) = do
    pure $
      Just
        Domain.Types.PayoutSplitConfig.PayoutSplitConfig
          { area = area,
            bankDetails = bankDetails,
            id = Kernel.Types.Id.Id id,
            vendorId = vendorId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutSplitConfig Domain.Types.PayoutSplitConfig.PayoutSplitConfig where
  toTType' (Domain.Types.PayoutSplitConfig.PayoutSplitConfig {..}) = do
    Beam.PayoutSplitConfigT
      { Beam.area = area,
        Beam.bankDetails = bankDetails,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.vendorId = vendorId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
