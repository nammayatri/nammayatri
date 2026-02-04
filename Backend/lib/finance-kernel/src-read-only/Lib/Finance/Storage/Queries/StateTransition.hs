{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.StateTransition where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.StateTransition
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.StateTransition as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.StateTransition.StateTransition -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.StateTransition.StateTransition] -> m ())
createMany = traverse_ create

findByEntity :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.StateTransition.StateTransition]))
findByEntity entityType entityId = do findAllWithKV [Se.And [Se.Is Beam.entityType $ Se.Eq entityType, Se.Is Beam.entityId $ Se.Eq entityId]]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.StateTransition.StateTransition -> m (Maybe Lib.Finance.Domain.Types.StateTransition.StateTransition))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.StateTransition.StateTransition -> m (Maybe Lib.Finance.Domain.Types.StateTransition.StateTransition))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.StateTransition.StateTransition -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.StateTransition.StateTransition {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actorId actorId,
      Se.Set Beam.actorType actorType,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.event event,
      Se.Set Beam.eventData eventData,
      Se.Set Beam.fromState fromState,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.toState toState,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.StateTransition Lib.Finance.Domain.Types.StateTransition.StateTransition where
  fromTType' (Beam.StateTransitionT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.StateTransition.StateTransition
          { actorId = actorId,
            actorType = actorType,
            createdAt = createdAt,
            entityId = entityId,
            entityType = entityType,
            event = event,
            eventData = eventData,
            fromState = fromState,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            toState = toState,
            updatedAt = updatedAt
          }

instance ToTType' Beam.StateTransition Lib.Finance.Domain.Types.StateTransition.StateTransition where
  toTType' (Lib.Finance.Domain.Types.StateTransition.StateTransition {..}) = do
    Beam.StateTransitionT
      { Beam.actorId = actorId,
        Beam.actorType = actorType,
        Beam.createdAt = createdAt,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.event = event,
        Beam.eventData = eventData,
        Beam.fromState = fromState,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.toState = toState,
        Beam.updatedAt = updatedAt
      }
