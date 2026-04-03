{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.Translations where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.Translations
import qualified Storage.Beam.Translations as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.Translations Domain.Types.Translations.Translations
    where fromTType' (Beam.TranslationsT {..}) = do pure $ Just Domain.Types.Translations.Translations{createdAt = createdAt,
                                                                                                       id = Kernel.Types.Id.Id id,
                                                                                                       language = language,
                                                                                                       message = message,
                                                                                                       messageKey = messageKey,
                                                                                                       updatedAt = updatedAt}
instance ToTType' Beam.Translations Domain.Types.Translations.Translations
    where toTType' (Domain.Types.Translations.Translations {..}) = do Beam.TranslationsT{Beam.createdAt = createdAt,
                                                                                         Beam.id = Kernel.Types.Id.getId id,
                                                                                         Beam.language = language,
                                                                                         Beam.message = message,
                                                                                         Beam.messageKey = messageKey,
                                                                                         Beam.updatedAt = updatedAt}



