{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.NammaTag (module Lib.Yudhishthira.Storage.Queries.NammaTag, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.NammaTag as Beam
import Lib.Yudhishthira.Storage.Queries.NammaTagExtra as ReExport
import Lib.Yudhishthira.Storage.Queries.Transformers.NammaTag
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTag
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.NammaTag.NammaTag -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.NammaTag.NammaTag] -> m ())
createMany = traverse_ create

deleteByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByPrimaryKey name = do deleteWithKV [Se.Is Beam.name $ Se.Eq name]

findAllByPrimaryKeys :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Kernel.Prelude.Text] -> m [Lib.Yudhishthira.Types.NammaTag.NammaTag])
findAllByPrimaryKeys name = do findAllWithKV [Se.And [Se.Is Beam.name $ Se.In name]]

findByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Yudhishthira.Types.NammaTag.NammaTag))
findByPrimaryKey name = do findOneWithKV [Se.And [Se.Is Beam.name $ Se.Eq name]]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.NammaTag.NammaTag -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.NammaTag.NammaTag {..}) = do
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
    [Se.And [Se.Is Beam.name $ Se.Eq name]]
