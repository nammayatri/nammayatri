{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Reminder where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Person
import qualified Domain.Types.Reminder
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Reminder as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Reminder.Reminder -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Reminder.Reminder] -> m ())
createMany = traverse_ create

findAllByEntityIdAndDocumentType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Domain.Types.DocumentVerificationConfig.DocumentType -> m [Domain.Types.Reminder.Reminder])
findAllByEntityIdAndDocumentType entityId documentType = do findAllWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.documentType $ Se.Eq documentType]]

findAllPendingByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Reminder.ReminderStatus -> m [Domain.Types.Reminder.Reminder])
findAllPendingByDriverId driverId status = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.status $ Se.Eq status]]

findAllPendingByDriverIdAndDocumentType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Reminder.ReminderStatus -> Domain.Types.DocumentVerificationConfig.DocumentType -> m [Domain.Types.Reminder.Reminder])
findAllPendingByDriverIdAndDocumentType driverId status documentType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.status $ Se.Eq status,
          Se.Is Beam.documentType $ Se.Eq documentType
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Reminder.Reminder -> m (Maybe Domain.Types.Reminder.Reminder))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Reminder.Reminder -> m ())
updateByPrimaryKey (Domain.Types.Reminder.Reminder {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currentIntervalIndex currentIntervalIndex,
      Se.Set Beam.documentType documentType,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.dueDate dueDate,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.metadata metadata,
      Se.Set Beam.reminderDate reminderDate,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Reminder Domain.Types.Reminder.Reminder where
  fromTType' (Beam.ReminderT {..}) = do
    pure $
      Just
        Domain.Types.Reminder.Reminder
          { createdAt = createdAt,
            currentIntervalIndex = currentIntervalIndex,
            documentType = documentType,
            driverId = Kernel.Types.Id.Id driverId,
            dueDate = dueDate,
            entityId = entityId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            metadata = metadata,
            reminderDate = reminderDate,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Reminder Domain.Types.Reminder.Reminder where
  toTType' (Domain.Types.Reminder.Reminder {..}) = do
    Beam.ReminderT
      { Beam.createdAt = createdAt,
        Beam.currentIntervalIndex = currentIntervalIndex,
        Beam.documentType = documentType,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.dueDate = dueDate,
        Beam.entityId = entityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.metadata = metadata,
        Beam.reminderDate = reminderDate,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
