{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Queries.PermitApplicationEvent where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Lib.Permit.Domain.Types.PermitApplicationEvent
import qualified Lib.Permit.Storage.Beam.BeamFlow
import qualified Lib.Permit.Storage.Beam.PermitApplicationEvent as Beam
import qualified Sequelize as Se

create :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent -> m ())
create = createWithKV

createMany :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent] -> m ())
createMany = traverse_ create

findAllByApplicationId ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> m ([Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent]))
findAllByApplicationId limit offset applicationId = do findAllWithOptionsKV [Se.Is Beam.applicationId $ Se.Eq (Kernel.Types.Id.getId applicationId)] (Se.Asc Beam.createdAt) limit offset

findByPrimaryKey ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent -> m (Maybe Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent -> m ())
updateByPrimaryKey (Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.action action,
      Se.Set Beam.actorId actorId,
      Se.Set Beam.actorName actorName,
      Se.Set Beam.actorType actorType,
      Se.Set Beam.applicationId (Kernel.Types.Id.getId applicationId),
      Se.Set Beam.reason reason,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PermitApplicationEvent Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent where
  fromTType' (Beam.PermitApplicationEventT {..}) = do
    pure $
      Just
        Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent
          { action = action,
            actorId = actorId,
            actorName = actorName,
            actorType = actorType,
            applicationId = Kernel.Types.Id.Id applicationId,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            reason = reason,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PermitApplicationEvent Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent where
  toTType' (Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent {..}) = do
    Beam.PermitApplicationEventT
      { Beam.action = action,
        Beam.actorId = actorId,
        Beam.actorName = actorName,
        Beam.actorType = actorType,
        Beam.applicationId = Kernel.Types.Id.getId applicationId,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.reason = reason,
        Beam.updatedAt = updatedAt
      }
