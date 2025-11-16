{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Translations where

import qualified Domain.Types.Translations
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Translations as Beam

instance FromTType' Beam.Translations Domain.Types.Translations.Translations where
  fromTType' (Beam.TranslationsT {..}) = do
    pure $
      Just
        Domain.Types.Translations.Translations
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            language = language,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            message = message,
            messageKey = messageKey,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Translations Domain.Types.Translations.Translations where
  toTType' (Domain.Types.Translations.Translations {..}) = do
    Beam.TranslationsT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.message = message,
        Beam.messageKey = messageKey,
        Beam.updatedAt = updatedAt
      }
