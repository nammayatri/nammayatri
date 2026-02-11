{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DocumentReminderHistory where

import qualified Domain.Types.DocumentReminderHistory
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DocumentReminderHistory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentReminderHistory.DocumentReminderHistory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DocumentReminderHistory.DocumentReminderHistory] -> m ())
createMany = traverse_ create

findAllByDocumentTypeAndEntity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.Prelude.Text -> Domain.Types.DocumentReminderHistory.EntityType -> m ([Domain.Types.DocumentReminderHistory.DocumentReminderHistory]))
findAllByDocumentTypeAndEntity documentType entityId entityType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType
        ]
    ]

findAllByDocumentTypeAndEntityType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DocumentVerificationConfig.DocumentType -> Domain.Types.DocumentReminderHistory.EntityType -> m ([Domain.Types.DocumentReminderHistory.DocumentReminderHistory]))
findAllByDocumentTypeAndEntityType documentType entityType = do findAllWithKV [Se.And [Se.Is Beam.documentType $ Se.Eq documentType, Se.Is Beam.entityType $ Se.Eq entityType]]

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.DocumentReminderHistory.DocumentReminderHistory]))
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

findByDocumentTypeAndEntity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DocumentVerificationConfig.DocumentType -> Kernel.Prelude.Text -> Domain.Types.DocumentReminderHistory.EntityType -> m (Maybe Domain.Types.DocumentReminderHistory.DocumentReminderHistory))
findByDocumentTypeAndEntity documentType entityId entityType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.documentType $ Se.Eq documentType,
          Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DocumentReminderHistory.DocumentReminderHistory -> m (Maybe Domain.Types.DocumentReminderHistory.DocumentReminderHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentReminderHistory.DocumentReminderHistory -> m ())
updateByPrimaryKey (Domain.Types.DocumentReminderHistory.DocumentReminderHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.completionDate completionDate,
      Se.Set Beam.documentType documentType,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rideCountAtCompletion rideCountAtCompletion,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DocumentReminderHistory Domain.Types.DocumentReminderHistory.DocumentReminderHistory where
  fromTType' (Beam.DocumentReminderHistoryT {..}) = do
    pure $
      Just
        Domain.Types.DocumentReminderHistory.DocumentReminderHistory
          { completionDate = completionDate,
            documentType = documentType,
            entityId = entityId,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rideCountAtCompletion = rideCountAtCompletion,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' Beam.DocumentReminderHistory Domain.Types.DocumentReminderHistory.DocumentReminderHistory where
  toTType' (Domain.Types.DocumentReminderHistory.DocumentReminderHistory {..}) = do
    Beam.DocumentReminderHistoryT
      { Beam.completionDate = completionDate,
        Beam.documentType = documentType,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rideCountAtCompletion = rideCountAtCompletion,
        Beam.updatedAt = updatedAt,
        Beam.createdAt = createdAt
      }
