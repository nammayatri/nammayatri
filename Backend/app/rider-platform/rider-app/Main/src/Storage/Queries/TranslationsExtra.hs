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

findAllByMessageKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [Domain.Types.Translations.Translations]
findAllByMessageKey messageKey = findAllWithKV [Se.Is Beam.messageKey $ Se.Eq messageKey]

findAllByMerchantOperatingCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.Translations.Translations]
findAllByMerchantOperatingCityId merchantOperatingCityId = findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ getId merchantOperatingCityId)]

-- | Reads straight from Postgres (findOneWithDb, not findOneWithKV) for all 3
-- levels. WhatsApp bot rows (see feature-migrations/0047, 0050) are seeded via
-- raw SQL, not the app's own create path, so they never populate the KV/Redis
-- secondary-key index that findOneWithKV checks first -- confirmed live: a
-- row verified present in Postgres still resolved to Nothing through
-- findOneWithKV. Bypassing KV here means always hitting the real table, which
-- is what a correctness-critical, no-fallback lookup (WhatsappBot.Adapter.
-- Translations.resolveField throws on Nothing) needs.
findByMerchantOpCityIdMessageKeyLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Text -> Lang.Language -> m (Maybe Domain.Types.Translations.Translations)
findByMerchantOpCityIdMessageKeyLanguage moid messageKey language = do
  -- Level 1: Try city-specific translation with requested language
  maybeCityTranslation <- findOneWithDb [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ getId moid), Se.Is Beam.messageKey $ Se.Eq messageKey, Se.Is Beam.language $ Se.Eq language]]
  case maybeCityTranslation of
    Just translation -> return (Just translation)
    Nothing -> do
      -- Level 2: Try global translation (NULL city) with requested language
      maybeGlobalTranslation <- findOneWithDb [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq Nothing, Se.Is Beam.messageKey $ Se.Eq messageKey, Se.Is Beam.language $ Se.Eq language]]
      case maybeGlobalTranslation of
        Just translation -> return (Just translation)
        Nothing ->
          -- Level 3: Try global translation (NULL city) with English fallback
          findOneWithDb [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq Nothing, Se.Is Beam.messageKey $ Se.Eq messageKey, Se.Is Beam.language $ Se.Eq Lang.ENGLISH]]
