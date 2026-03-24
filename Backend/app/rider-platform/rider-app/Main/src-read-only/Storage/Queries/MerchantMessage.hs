{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.MerchantMessage where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.Transformers.MerchantMessage
import qualified Domain.Types.MerchantMessage
import qualified Storage.Beam.MerchantMessage as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantMessage.MerchantMessage -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantMessage.MerchantMessage] -> m ())
createMany = traverse_ create
findAllByMerchantOpCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                             (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.MerchantMessage.MerchantMessage]))
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]
findByMerchantOperatingCityIdAndMessageKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                              (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.MerchantMessage.MessageKey -> m (Maybe Domain.Types.MerchantMessage.MerchantMessage))
findByMerchantOperatingCityIdAndMessageKey merchantOperatingCityId messageKey = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                          Se.Is Beam.messageKey $ Se.Eq messageKey]]



instance FromTType' Beam.MerchantMessage Domain.Types.MerchantMessage.MerchantMessage
    where fromTType' (Beam.MerchantMessageT {..}) = do pure $ Just Domain.Types.MerchantMessage.MerchantMessage{containsUrlButton = containsUrlButton,
                                                                                                                createdAt = createdAt,
                                                                                                                jsonData = valueToJsonData jsonData,
                                                                                                                merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                message = message,
                                                                                                                messageKey = messageKey,
                                                                                                                messageType = messageType,
                                                                                                                senderHeader = senderHeader,
                                                                                                                templateId = fromMaybe "" templateId,
                                                                                                                updatedAt = updatedAt}
instance ToTType' Beam.MerchantMessage Domain.Types.MerchantMessage.MerchantMessage
    where toTType' (Domain.Types.MerchantMessage.MerchantMessage {..}) = do Beam.MerchantMessageT{Beam.containsUrlButton = containsUrlButton,
                                                                                                  Beam.createdAt = createdAt,
                                                                                                  Beam.jsonData = (Just $ toJSON jsonData),
                                                                                                  Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                  Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                  Beam.message = message,
                                                                                                  Beam.messageKey = messageKey,
                                                                                                  Beam.messageType = messageType,
                                                                                                  Beam.senderHeader = senderHeader,
                                                                                                  Beam.templateId = (Just templateId),
                                                                                                  Beam.updatedAt = updatedAt}



