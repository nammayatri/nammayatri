{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ReminderConfig where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ReminderConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ReminderConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ReminderConfig.ReminderConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ReminderConfig.ReminderConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.ReminderConfig.ReminderConfig]))
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

findByMerchantOpCityIdAndDocumentType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.DocumentVerificationConfig.DocumentType -> m (Maybe Domain.Types.ReminderConfig.ReminderConfig))
findByMerchantOpCityIdAndDocumentType merchantOperatingCityId documentType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.documentType $ Se.Eq documentType
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ReminderConfig.ReminderConfig -> m (Maybe Domain.Types.ReminderConfig.ReminderConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ReminderConfig.ReminderConfig -> m ())
updateByPrimaryKey (Domain.Types.ReminderConfig.ReminderConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.daysThreshold daysThreshold,
      Se.Set Beam.documentType documentType,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.isMandatory isMandatory,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.reminderIntervals reminderIntervals,
      Se.Set Beam.ridesThreshold ridesThreshold,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ReminderConfig Domain.Types.ReminderConfig.ReminderConfig where
  fromTType' (Beam.ReminderConfigT {..}) = do
    pure $
      Just
        Domain.Types.ReminderConfig.ReminderConfig
          { createdAt = createdAt,
            daysThreshold = daysThreshold,
            documentType = documentType,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            isMandatory = isMandatory,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reminderIntervals = reminderIntervals,
            ridesThreshold = ridesThreshold,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ReminderConfig Domain.Types.ReminderConfig.ReminderConfig where
  toTType' (Domain.Types.ReminderConfig.ReminderConfig {..}) = do
    Beam.ReminderConfigT
      { Beam.createdAt = createdAt,
        Beam.daysThreshold = daysThreshold,
        Beam.documentType = documentType,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isMandatory = isMandatory,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reminderIntervals = reminderIntervals,
        Beam.ridesThreshold = ridesThreshold,
        Beam.updatedAt = updatedAt
      }
