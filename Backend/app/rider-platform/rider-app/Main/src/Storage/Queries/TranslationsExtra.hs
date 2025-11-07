{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TranslationsExtra where

import qualified Domain.Types.Translations
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Translations as Beam
import Storage.Queries.OrphanInstances.Translations

findByMsgKeyAndLang :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Lang.Language -> m (Maybe Domain.Types.Translations.Translations)
findByMsgKeyAndLang messageKey language = do
  maybeTranslation <- findOneWithKV [Se.And [Se.Is Beam.messageKey $ Se.Eq messageKey, Se.Is Beam.language $ Se.Eq language]]
  case maybeTranslation of
    Just translation -> return (Just translation)
    Nothing -> findOneWithKV [Se.And [Se.Is Beam.messageKey $ Se.Eq messageKey, Se.Is Beam.language $ Se.Eq Lang.ENGLISH]]
