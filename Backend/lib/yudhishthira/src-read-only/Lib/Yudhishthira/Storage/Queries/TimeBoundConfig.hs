{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.TimeBoundConfig (module Lib.Yudhishthira.Storage.Queries.TimeBoundConfig, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.TimeBoundConfig as Beam
import Lib.Yudhishthira.Storage.Queries.TimeBoundConfigExtra as ReExport
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.TimeBoundConfig
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig] -> m ())
createMany = traverse_ create

findByCityAndDomain ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig])
findByCityAndDomain merchantOperatingCityId timeBoundDomain = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.timeBoundDomain $ Se.Eq timeBoundDomain
        ]
    ]

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> Lib.Yudhishthira.Types.LogicDomain -> m (Maybe Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig))
findByPrimaryKey merchantOperatingCityId name timeBoundDomain = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.name $ Se.Eq name,
          Se.Is Beam.timeBoundDomain $ Se.Eq timeBoundDomain
        ]
    ]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.TimeBoundConfig.TimeBoundConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.timeBounds timeBounds, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.name $ Se.Eq name,
          Se.Is Beam.timeBoundDomain $ Se.Eq timeBoundDomain
        ]
    ]
