{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MonetaryRewardConfig (module Storage.Queries.MonetaryRewardConfig, module ReExport) where

import qualified Domain.Types.MonetaryRewardConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MonetaryRewardConfig as Beam
import Storage.Queries.MonetaryRewardConfigExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig] -> m ())
createMany = traverse_ create

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig -> m (Maybe Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig -> m ())
updateByPrimaryKey (Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.active active,
      Se.Set Beam.eventFunction eventFunction,
      Se.Set Beam.eventName eventName,
      Se.Set Beam.expirationAt expirationAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.monetaryRewardAmount monetaryRewardAmount,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
