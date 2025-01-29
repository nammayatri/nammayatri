{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement (module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement, module ReExport) where

import qualified Data.Aeson
import qualified Data.String.Conversions
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicElement as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElementExtra as ReExport
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement] -> m ())
createMany = traverse_ create

findByDomain :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement])
findByDomain domain = do findAllWithKV [Se.Is Beam.domain $ Se.Eq domain]

findByDomainAndVersion ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Int -> m [Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement])
findByDomainAndVersion limit offset domain version = do findAllWithOptionsKV [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.version $ Se.Eq version]] (Se.Asc Beam.order) limit offset

findLatestVersion ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement])
findLatestVersion limit offset domain = do findAllWithOptionsKV [Se.Is Beam.domain $ Se.Eq domain] (Se.Desc Beam.version) limit offset

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Yudhishthira.Types.LogicDomain -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> m (Maybe Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement))
findByPrimaryKey domain order version = do findOneWithKV [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.order $ Se.Eq order, Se.Is Beam.version $ Se.Eq version]]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.logic ((Data.String.Conversions.cs . Data.Aeson.encode) logic),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.order $ Se.Eq order, Se.Is Beam.version $ Se.Eq version]]
