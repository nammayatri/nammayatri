{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ModuleCompletionInformation where

import qualified Domain.Types.DriverModuleCompletion
import qualified Domain.Types.ModuleCompletionInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ModuleCompletionInformation as Beam

create :: KvDbFlow m r => (Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation] -> m ())
createMany = traverse_ create

findAllByCompletionId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m [Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation])
findAllByCompletionId (Kernel.Types.Id.Id completionId) = do findAllWithKV [Se.Is Beam.completionId $ Se.Eq completionId]

findAllByCompletionIdAndEntity ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> Domain.Types.ModuleCompletionInformation.ModuleEntity -> m [Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation])
findAllByCompletionIdAndEntity (Kernel.Types.Id.Id completionId) entity = do findAllWithKV [Se.And [Se.Is Beam.completionId $ Se.Eq completionId, Se.Is Beam.entity $ Se.Eq entity]]

findAllByCompletionIdAndEntityAndStatus ::
  KvDbFlow m r =>
  (Domain.Types.ModuleCompletionInformation.ModuleEntity -> Domain.Types.ModuleCompletionInformation.EntityStatus -> Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m [Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation])
findAllByCompletionIdAndEntityAndStatus entity entityStatus (Kernel.Types.Id.Id completionId) = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.entity $ Se.Eq entity,
          Se.Is Beam.entityStatus $ Se.Eq entityStatus,
          Se.Is Beam.completionId $ Se.Eq completionId
        ]
    ]

findByCompletionIdAndEntityAndEntityId ::
  KvDbFlow m r =>
  (Maybe Int -> Maybe Int -> Domain.Types.ModuleCompletionInformation.ModuleEntity -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> m [Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation])
findByCompletionIdAndEntityAndEntityId limit offset entity entityId (Kernel.Types.Id.Id completionId) = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.entity $ Se.Eq entity,
          Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.completionId $ Se.Eq completionId
        ]
    ]
    (Se.Desc Beam.attempt)
    limit
    offset

findByPrimaryKey ::
  KvDbFlow m r =>
  (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion -> Domain.Types.ModuleCompletionInformation.ModuleEntity -> Kernel.Prelude.Text -> m (Maybe Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation))
findByPrimaryKey attempt (Kernel.Types.Id.Id completionId) entity entityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.attempt $ Se.Eq attempt,
          Se.Is Beam.completionId $ Se.Eq completionId,
          Se.Is Beam.entity $ Se.Eq entity,
          Se.Is Beam.entityId $ Se.Eq entityId
        ]
    ]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation -> m ())
updateByPrimaryKey (Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.entityStatus entityStatus,
      Se.Set Beam.selectedEntityId selectedEntityId,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.attempt $ Se.Eq attempt,
          Se.Is Beam.completionId $ Se.Eq (Kernel.Types.Id.getId completionId),
          Se.Is Beam.entity $ Se.Eq entity,
          Se.Is Beam.entityId $ Se.Eq entityId
        ]
    ]

instance FromTType' Beam.ModuleCompletionInformation Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation where
  fromTType' (Beam.ModuleCompletionInformationT {..}) = do
    pure $
      Just
        Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation
          { attempt = attempt,
            completionId = Kernel.Types.Id.Id completionId,
            createdAt = createdAt,
            entity = entity,
            entityId = entityId,
            entityStatus = entityStatus,
            selectedEntityId = selectedEntityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ModuleCompletionInformation Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation where
  toTType' (Domain.Types.ModuleCompletionInformation.ModuleCompletionInformation {..}) = do
    Beam.ModuleCompletionInformationT
      { Beam.attempt = attempt,
        Beam.completionId = Kernel.Types.Id.getId completionId,
        Beam.createdAt = createdAt,
        Beam.entity = entity,
        Beam.entityId = entityId,
        Beam.entityStatus = entityStatus,
        Beam.selectedEntityId = selectedEntityId,
        Beam.updatedAt = updatedAt
      }
