{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantMessage where

import qualified Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantMessage as Beam
import Storage.Queries.Transformers.MerchantMessage

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.MerchantMessage.MerchantMessage -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.MerchantMessage.MerchantMessage] -> m ()
createMany = traverse_ create

deleteByMerchantOperatingCityIdAndMessageKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  (Domain.Types.MerchantMessage.MessageKey -> m ())
deleteByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId messageKey = do
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.messageKey $ Se.Eq messageKey
        ]
    ]

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m [Domain.Types.MerchantMessage.MerchantMessage]
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findAllByMerchantOpCityIdAndMessageKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  (Domain.Types.MerchantMessage.MessageKey -> m [Domain.Types.MerchantMessage.MerchantMessage])
findAllByMerchantOpCityIdAndMessageKey merchantOperatingCityId messageKey = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.messageKey $ Se.Eq messageKey
        ]
    ]

findByMerchantOperatingCityIdAndMessageKeyLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  (Domain.Types.MerchantMessage.MessageKey -> (Kernel.External.Types.Language -> m (Maybe Domain.Types.MerchantMessage.MerchantMessage)))
findByMerchantOperatingCityIdAndMessageKeyLanguage merchantOperatingCityId messageKey language = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.messageKey $ Se.Eq messageKey,
          Se.Is Beam.language $ Se.Eq (Just language)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.MerchantMessage.MerchantMessage -> m ()
updateByPrimaryKey (Domain.Types.MerchantMessage.MerchantMessage {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.containsUrlButton containsUrlButton,
      Se.Set Beam.jsonData (Just $ toJSON jsonData),
      Se.Set Beam.mediaUrl mediaUrl,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.message message,
      Se.Set Beam.messageType messageType,
      Se.Set Beam.senderHeader senderHeader,
      Se.Set Beam.templateId (Just templateId),
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.language $ Se.Eq (Just language),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.messageKey $ Se.Eq messageKey
        ]
    ]

instance FromTType' Beam.MerchantMessage Domain.Types.MerchantMessage.MerchantMessage where
  fromTType' (Beam.MerchantMessageT {..}) = do
    pure $
      Just
        Domain.Types.MerchantMessage.MerchantMessage
          { containsUrlButton = containsUrlButton,
            createdAt = createdAt,
            jsonData = valueToJsonData jsonData,
            language = fromMaybe Kernel.External.Types.ENGLISH language,
            mediaUrl = mediaUrl,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            message = message,
            messageKey = messageKey,
            messageType = messageType,
            senderHeader = senderHeader,
            templateId = fromMaybe "" templateId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantMessage Domain.Types.MerchantMessage.MerchantMessage where
  toTType' (Domain.Types.MerchantMessage.MerchantMessage {..}) = do
    Beam.MerchantMessageT
      { Beam.containsUrlButton = containsUrlButton,
        Beam.createdAt = createdAt,
        Beam.jsonData = Just $ toJSON jsonData,
        Beam.language = Just language,
        Beam.mediaUrl = mediaUrl,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.message = message,
        Beam.messageKey = messageKey,
        Beam.messageType = messageType,
        Beam.senderHeader = senderHeader,
        Beam.templateId = Just templateId,
        Beam.updatedAt = updatedAt
      }
