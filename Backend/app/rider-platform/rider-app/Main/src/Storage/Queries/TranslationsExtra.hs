{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TranslationsExtra where

import qualified Domain.Types.MerchantOperatingCity as DMerchantOperatingCity
import qualified Domain.Types.Translations
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Storage.InMem as IM
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Translations as Beam
import Storage.Queries.OrphanInstances.Translations

findByMerchantOpCityIdMessageKeyLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Text -> Lang.Language -> m (Maybe Domain.Types.Translations.Translations)
findByMerchantOpCityIdMessageKeyLanguage moid messageKey language = do
  -- Level 1: Try city-specific translation with requested language
  maybeCityTranslation <- findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ getId moid), Se.Is Beam.messageKey $ Se.Eq messageKey, Se.Is Beam.language $ Se.Eq language]]
  case maybeCityTranslation of
    Just translation -> return (Just translation)
    Nothing -> do
      -- Level 2: Try global translation (NULL city) with requested language
      maybeGlobalTranslation <- findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq Nothing, Se.Is Beam.messageKey $ Se.Eq messageKey, Se.Is Beam.language $ Se.Eq language]]
      case maybeGlobalTranslation of
        Just translation -> return (Just translation)
        Nothing -> do
          -- Level 3: Try global translation (NULL city) with English fallback
          findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq Nothing, Se.Is Beam.messageKey $ Se.Eq messageKey, Se.Is Beam.language $ Se.Eq Lang.ENGLISH]]
