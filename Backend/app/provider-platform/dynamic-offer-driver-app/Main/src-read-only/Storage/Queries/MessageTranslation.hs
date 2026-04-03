{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.MessageTranslation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.MessageTranslation
import qualified Storage.Beam.MessageTranslation as Beam
import qualified Kernel.External.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.Message
import qualified Data.Time
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MessageTranslation.MessageTranslation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MessageTranslation.MessageTranslation] -> m ())
createMany = traverse_ create
findByMessageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Message.Message -> m ([Domain.Types.MessageTranslation.MessageTranslation]))
findByMessageId messageId = do findAllWithKV [Se.Is Beam.messageId $ Se.Eq (Kernel.Types.Id.getId messageId)]
findByMessageIdAndLanguage :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                              (Kernel.Types.Id.Id Domain.Types.Message.Message -> Kernel.External.Types.Language -> m (Maybe Domain.Types.MessageTranslation.MessageTranslation))
findByMessageIdAndLanguage messageId language = do findOneWithKV [Se.And [Se.Is Beam.messageId $ Se.Eq (Kernel.Types.Id.getId messageId), Se.Is Beam.language $ Se.Eq language]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Message.Message -> m (Maybe Domain.Types.MessageTranslation.MessageTranslation))
findByPrimaryKey messageId = do findOneWithKV [Se.And [Se.Is Beam.messageId $ Se.Eq (Kernel.Types.Id.getId messageId)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MessageTranslation.MessageTranslation -> m ())
updateByPrimaryKey (Domain.Types.MessageTranslation.MessageTranslation {..}) = do updateWithKV [Se.Set Beam.description description,
                                                                                                Se.Set Beam.label label,
                                                                                                Se.Set Beam.language language,
                                                                                                Se.Set Beam.shortDescription shortDescription,
                                                                                                Se.Set Beam.title title] [Se.And [Se.Is Beam.messageId $ Se.Eq (Kernel.Types.Id.getId messageId)]]



instance FromTType' Beam.MessageTranslation Domain.Types.MessageTranslation.MessageTranslation
    where fromTType' (Beam.MessageTranslationT {..}) = do pure $ Just Domain.Types.MessageTranslation.MessageTranslation{createdAt = Data.Time.localTimeToUTC Data.Time.utc createdAt,
                                                                                                                         description = description,
                                                                                                                         label = label,
                                                                                                                         language = language,
                                                                                                                         messageId = Kernel.Types.Id.Id messageId,
                                                                                                                         shortDescription = shortDescription,
                                                                                                                         title = title}
instance ToTType' Beam.MessageTranslation Domain.Types.MessageTranslation.MessageTranslation
    where toTType' (Domain.Types.MessageTranslation.MessageTranslation {..}) = do Beam.MessageTranslationT{Beam.createdAt = Data.Time.utcToLocalTime Data.Time.utc createdAt,
                                                                                                           Beam.description = description,
                                                                                                           Beam.label = label,
                                                                                                           Beam.language = language,
                                                                                                           Beam.messageId = Kernel.Types.Id.getId messageId,
                                                                                                           Beam.shortDescription = shortDescription,
                                                                                                           Beam.title = title}



