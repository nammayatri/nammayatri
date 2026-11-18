{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicExperimentGroup (module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicExperimentGroup, module ReExport) where

import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicExperimentGroup as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Storage.Queries.AppDynamicLogicExperimentGroupExtra as ReExport
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> m ([Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup]))
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

findByMerchantOpCityAndGroup ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Data.Text.Text -> m ([Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup]))
findByMerchantOpCityAndGroup merchantOperatingCityId groupName = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.groupName $ Se.Eq groupName
        ]
    ]

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Data.Text.Text -> Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Data.Text.Text -> m (Maybe Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup))
findByPrimaryKey groupName merchantOperatingCityId timeBounds = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.groupName $ Se.Eq groupName,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.timeBounds $ Se.Eq timeBounds
        ]
    ]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.AppDynamicLogicExperimentGroup.AppDynamicLogicExperimentGroup {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.experimentStatus experimentStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.modifiedBy (Kernel.Types.Id.getId <$> modifiedBy),
      Se.Set Beam.rolloutPercentage rolloutPercentage,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.groupName $ Se.Eq groupName,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.timeBounds $ Se.Eq timeBounds
        ]
    ]
