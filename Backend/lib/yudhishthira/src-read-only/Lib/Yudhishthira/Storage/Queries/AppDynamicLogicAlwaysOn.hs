{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicAlwaysOn (module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicAlwaysOn, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicAlwaysOn as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Storage.Queries.AppDynamicLogicAlwaysOnExtra as ReExport
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn] -> m ())
createMany = traverse_ create

findByMerchantOpCityAndDomain ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ([Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn]))
findByMerchantOpCityAndDomain merchantOperatingCityId domain = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.domain $ Se.Eq domain
        ]
    ]

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Yudhishthira.Types.LogicDomain -> Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Int -> m (Maybe Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn))
findByPrimaryKey domain merchantOperatingCityId version = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.version $ Se.Eq version
        ]
    ]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.order order, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.version $ Se.Eq version
        ]
    ]
