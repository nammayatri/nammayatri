{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.NammaTagTrigger where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.NammaTagTrigger as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTagTrigger
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger] -> m ())
createMany = traverse_ create

deleteAllByTagName :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ())
deleteAllByTagName tagName = do deleteWithKV [Se.Is Beam.tagName $ Se.Eq tagName]

findAllByEvent :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.ApplicationEvent -> m [Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger])
findAllByEvent event = do findAllWithKV [Se.Is Beam.event $ Se.Eq event]

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Yudhishthira.Types.ApplicationEvent -> Kernel.Prelude.Text -> m (Maybe Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger))
findByPrimaryKey event tagName = do findOneWithKV [Se.And [Se.Is Beam.event $ Se.Eq event, Se.Is Beam.tagName $ Se.Eq tagName]]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger {..}) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.event $ Se.Eq event, Se.Is Beam.tagName $ Se.Eq tagName]]

instance FromTType' Beam.NammaTagTrigger Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger where
  fromTType' (Beam.NammaTagTriggerT {..}) = do pure $ Just Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger {createdAt = createdAt, event = event, tagName = tagName, updatedAt = updatedAt}

instance ToTType' Beam.NammaTagTrigger Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger where
  toTType' (Lib.Yudhishthira.Types.NammaTagTrigger.NammaTagTrigger {..}) = do
    Beam.NammaTagTriggerT
      { Beam.createdAt = createdAt,
        Beam.event = event,
        Beam.tagName = tagName,
        Beam.updatedAt = updatedAt
      }
