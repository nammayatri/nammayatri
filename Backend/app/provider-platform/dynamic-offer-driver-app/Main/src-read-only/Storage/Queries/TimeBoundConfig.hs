{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TimeBoundConfig (module Storage.Queries.TimeBoundConfig, module ReExport) where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TimeBoundConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.TimeBoundConfig as Beam
import Storage.Queries.TimeBoundConfigExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TimeBoundConfig.TimeBoundConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TimeBoundConfig.TimeBoundConfig] -> m ())
createMany = traverse_ create

findByCityAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ([Domain.Types.TimeBoundConfig.TimeBoundConfig]))
findByCityAndDomain merchantOperatingCityId timeBoundDomain = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.timeBoundDomain $ Se.Eq timeBoundDomain
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Lib.Yudhishthira.Types.LogicDomain -> m (Maybe Domain.Types.TimeBoundConfig.TimeBoundConfig))
findByPrimaryKey merchantOperatingCityId name timeBoundDomain = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.name $ Se.Eq name,
          Se.Is Beam.timeBoundDomain $ Se.Eq timeBoundDomain
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TimeBoundConfig.TimeBoundConfig -> m ())
updateByPrimaryKey (Domain.Types.TimeBoundConfig.TimeBoundConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.name $ Se.Eq name,
          Se.Is Beam.timeBoundDomain $ Se.Eq timeBoundDomain
        ]
    ]
