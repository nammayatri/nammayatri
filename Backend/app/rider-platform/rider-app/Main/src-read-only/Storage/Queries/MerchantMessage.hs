{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantMessage where

import qualified Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantMessage as Beam
import Storage.Queries.Transformers.MerchantMessage

create :: KvDbFlow m r => (Domain.Types.MerchantMessage.MerchantMessage -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.MerchantMessage.MerchantMessage] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.MerchantMessage.MerchantMessage])
findAllByMerchantOpCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByMerchantOperatingCityIdAndMessageKey ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.MerchantMessage.MessageKey -> m (Maybe Domain.Types.MerchantMessage.MerchantMessage))
findByMerchantOperatingCityIdAndMessageKey (Kernel.Types.Id.Id merchantOperatingCityId) messageKey = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
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
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            message = message,
            messageKey = messageKey,
            templateId = fromMaybe "" templateId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantMessage Domain.Types.MerchantMessage.MerchantMessage where
  toTType' (Domain.Types.MerchantMessage.MerchantMessage {..}) = do
    Beam.MerchantMessageT
      { Beam.containsUrlButton = containsUrlButton,
        Beam.createdAt = createdAt,
        Beam.jsonData = Just $ toJSON jsonData,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.message = message,
        Beam.messageKey = messageKey,
        Beam.templateId = Just templateId,
        Beam.updatedAt = updatedAt
      }
