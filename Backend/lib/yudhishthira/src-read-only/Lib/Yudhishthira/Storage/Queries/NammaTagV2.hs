{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.NammaTagV2 (module Lib.Yudhishthira.Storage.Queries.NammaTagV2, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.NammaTagV2 as Beam
import Lib.Yudhishthira.Storage.Queries.NammaTagV2Extra as ReExport
import Lib.Yudhishthira.Storage.Queries.Transformers.NammaTagV2
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTagV2
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2 -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2] -> m ())
createMany = traverse_ create

deleteByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> m ())
deleteByPrimaryKey merchantOperatingCityId name = do deleteWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId), Se.Is Beam.name $ Se.Eq name]]

findAllByMerchantOperatingCityId ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> m ([Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2]))
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findAllByPrimaryKeys ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> m ([Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2]))
findAllByPrimaryKeys name merchantOperatingCityId = do findAllWithKV [Se.And [Se.Is Beam.name $ Se.Eq name, Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Kernel.Prelude.Text -> m (Maybe Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2))
findByPrimaryKey merchantOperatingCityId name = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId), Se.Is Beam.name $ Se.Eq name]]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2 -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2 {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actionEngine actionEngine,
      Se.Set Beam.category category,
      Se.Set Beam.description description,
      Se.Set Beam.chakra (getChakra info),
      Se.Set Beam.tagType (getTag info),
      Se.Set Beam.rangeEnd (getRangeEnd possibleValues),
      Se.Set Beam.rangeStart (getRangeStart possibleValues),
      Se.Set Beam.tags (getTags possibleValues),
      Se.Set Beam.llmContext (getLlmContext rule),
      Se.Set Beam.ruleEngine (getRuleEngine rule),
      Se.Set Beam.validity validity,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId), Se.Is Beam.name $ Se.Eq name]]
