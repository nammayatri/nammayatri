{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AppDynamicLogicWindowConfig where

import qualified Domain.Types.AppDynamicLogicWindowConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.AppDynamicLogicWindowConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig] -> m ())
createMany = traverse_ create

findAllByDomain :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Lib.Yudhishthira.Types.LogicDomain -> m ([Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig]))
findAllByDomain domain = do findAllWithKV [Se.Is Beam.domain $ Se.Eq domain]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Int -> m (Maybe Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig))
findByPrimaryKey domain version = do findOneWithKV [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.version $ Se.Eq version]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig -> m ())
updateByPrimaryKey (Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.windowMode windowMode,
      Se.Set Beam.windowSize windowSize,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.version $ Se.Eq version]]

instance FromTType' Beam.AppDynamicLogicWindowConfig Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig where
  fromTType' (Beam.AppDynamicLogicWindowConfigT {..}) = do
    pure $
      Just
        Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig
          { domain = domain,
            version = version,
            windowMode = windowMode,
            windowSize = windowSize,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppDynamicLogicWindowConfig Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig where
  toTType' (Domain.Types.AppDynamicLogicWindowConfig.AppDynamicLogicWindowConfig {..}) = do
    Beam.AppDynamicLogicWindowConfigT
      { Beam.domain = domain,
        Beam.version = version,
        Beam.windowMode = windowMode,
        Beam.windowSize = windowSize,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
