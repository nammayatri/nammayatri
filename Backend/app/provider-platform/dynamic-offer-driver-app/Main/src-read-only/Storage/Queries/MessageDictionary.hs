{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.MessageDictionary where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.MessageDictionary
import qualified Storage.Beam.MessageDictionary as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MessageDictionary.MessageDictionary -> m ())
create = createWithKV
findAllByMessageTypeAndMerchantOpCityIdAndMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                                        (Domain.Types.MessageDictionary.MessageType -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ([Domain.Types.MessageDictionary.MessageDictionary]))
findAllByMessageTypeAndMerchantOpCityIdAndMerchantId messageType merchantOperatingCityId merchantId = do findAllWithKV [Se.And [Se.Is Beam.messageType $ Se.Eq messageType,
                                                                                                                                Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                                                Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MessageDictionary.MessageDictionary -> m (Maybe Domain.Types.MessageDictionary.MessageDictionary))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]



instance FromTType' Beam.MessageDictionary Domain.Types.MessageDictionary.MessageDictionary
    where fromTType' (Beam.MessageDictionaryT {..}) = do pure $ Just Domain.Types.MessageDictionary.MessageDictionary{id = Kernel.Types.Id.Id id,
                                                                                                                      merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                      merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                      messageKey = messageKey,
                                                                                                                      messageType = messageType,
                                                                                                                      createdAt = createdAt,
                                                                                                                      updatedAt = updatedAt}
instance ToTType' Beam.MessageDictionary Domain.Types.MessageDictionary.MessageDictionary
    where toTType' (Domain.Types.MessageDictionary.MessageDictionary {..}) = do Beam.MessageDictionaryT{Beam.id = Kernel.Types.Id.getId id,
                                                                                                        Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                        Beam.messageKey = messageKey,
                                                                                                        Beam.messageType = messageType,
                                                                                                        Beam.createdAt = createdAt,
                                                                                                        Beam.updatedAt = updatedAt}



