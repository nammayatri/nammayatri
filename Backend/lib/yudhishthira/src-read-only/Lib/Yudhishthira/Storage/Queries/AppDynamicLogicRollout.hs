{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRollout (module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRollout, module ReExport) where

import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRolloutExtra as ReExport
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicRollout
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> m [Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

findByMerchantOpCityAndDomain ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout])
findByMerchantOpCityAndDomain merchantOperatingCityId domain = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.domain $ Se.Eq domain
        ]
    ]

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Yudhishthira.Types.LogicDomain -> Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Data.Text.Text -> Kernel.Prelude.Int -> m (Maybe Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout))
findByPrimaryKey domain merchantOperatingCityId timeBounds version = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.version $ Se.Eq version
        ]
    ]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.experimentStatus experimentStatus,
      Se.Set Beam.isBaseVersion isBaseVersion,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.modifiedBy (Kernel.Types.Id.getId <$> modifiedBy),
      Se.Set Beam.percentageRollout percentageRollout,
      Se.Set Beam.versionDescription versionDescription,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.version $ Se.Eq version
        ]
    ]
