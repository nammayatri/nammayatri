{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantMessage where

import qualified Data.Aeson
import qualified Data.Default.Class
import qualified Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantMessage as Beam
import qualified Storage.Queries.Transformers.MerchantMessage

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantMessage.MerchantMessage -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantMessage.MerchantMessage] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.MerchantMessage.MerchantMessage])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByMerchantOpCityIdAndMessageKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.MerchantMessage.MessageKey -> m (Maybe Domain.Types.MerchantMessage.MerchantMessage))
findByMerchantOpCityIdAndMessageKey merchantOperatingCityId messageKey = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.messageKey $ Se.Eq messageKey
        ]
    ]

findByMerchantOpCityIdAndMessageKeyVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.MerchantMessage.MessageKey -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> m (Maybe Domain.Types.MerchantMessage.MerchantMessage))
findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOperatingCityId messageKey vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.messageKey $ Se.Eq messageKey,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantMessage.MerchantMessage -> m ())
updateByPrimaryKey (Domain.Types.MerchantMessage.MerchantMessage {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.containsUrlButton containsUrlButton,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.jsonData (Just $ Data.Aeson.toJSON jsonData),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.message message,
      Se.Set Beam.senderHeader senderHeader,
      Se.Set Beam.templateId (Just templateId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId), Se.Is Beam.messageKey $ Se.Eq messageKey]]

instance FromTType' Beam.MerchantMessage Domain.Types.MerchantMessage.MerchantMessage where
  fromTType' (Beam.MerchantMessageT {..}) = do
    pure $
      Just
        Domain.Types.MerchantMessage.MerchantMessage
          { containsUrlButton = containsUrlButton,
            createdAt = createdAt,
            jsonData = fromMaybe Data.Default.Class.def (Storage.Queries.Transformers.MerchantMessage.valueToJsonData =<< jsonData),
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            message = message,
            messageKey = messageKey,
            senderHeader = senderHeader,
            templateId = fromMaybe "" templateId,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.MerchantMessage Domain.Types.MerchantMessage.MerchantMessage where
  toTType' (Domain.Types.MerchantMessage.MerchantMessage {..}) = do
    Beam.MerchantMessageT
      { Beam.containsUrlButton = containsUrlButton,
        Beam.createdAt = createdAt,
        Beam.jsonData = Just $ Data.Aeson.toJSON jsonData,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.message = message,
        Beam.messageKey = messageKey,
        Beam.senderHeader = senderHeader,
        Beam.templateId = Just templateId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
