{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.Translations (findTextByKey, findTextByKeyWithFallback, findTextByKeyOrOpsText, findByOpsKey, findByOpsKeyInTable, findTranslatedText, findOptionalTranslatedText, findTranslatedTextOrOpsText, findTranslatedTextByOpsKey, clearCache) where

import Control.Applicative ((<|>))
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMerchantOperatingCity
import Domain.Types.TranslationKey (TranslatedText, lookupNames, textReadByKey)
import Domain.Types.TranslationKeys (RiderTranslationKey)
import qualified Domain.Types.Translations
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Storage.InMem as IM
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Translations as Beam
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))
import Storage.Queries.OrphanInstances.Translations
import qualified Storage.Queries.Translations as Queries

findByMerchantOpCityIdMessageKeyLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Data.Text.Text -> Lang.Language -> m (Kernel.Prelude.Maybe Domain.Types.Translations.Translations))
findByMerchantOpCityIdMessageKeyLanguage merchantOperatingCityId messageKey language = do
  let citySpecificKey = makeKey messageKey language (Just $ Kernel.Types.Id.getId merchantOperatingCityId)
      globalKey = makeKey messageKey language Nothing
  Hedis.safeGet citySpecificKey >>= \case
    Just cached -> pure (Just cached)
    Nothing ->
      Hedis.safeGet globalKey >>= \case
        Just cached -> pure (Just cached)
        Nothing -> do
          result <- Queries.findByMerchantOpCityIdMessageKeyLanguage merchantOperatingCityId messageKey language
          whenJust result $ \dataToBeCached -> do
            expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
            let cacheKey = makeKey messageKey language (Kernel.Types.Id.getId <$> dataToBeCached.merchantOperatingCityId)
            Hedis.setExp cacheKey dataToBeCached expTime
          pure result

makeKey :: Text -> Lang.Language -> Maybe Text -> Text
makeKey messageKey language mbCityId =
  "CachedQueries:Translations:"
    <> ":MessageKey-"
    <> show messageKey
    <> ":Language-"
    <> show language
    <> ":City-"
    <> fromMaybe "" mbCityId

-- | Drops the cached rows of a key in a language, the city's and the shared one, on every pod.
clearCache :: (CacheFlow m r, MonadFlow m) => Id DMerchantOperatingCity.MerchantOperatingCity -> Text -> Lang.Language -> m ()
clearCache cityId messageKey language = do
  Hedis.runInMultiCloudRedisWrite $ mapM_ (Hedis.del . makeKey messageKey language) [Just cityId.getId, Nothing]
  IM.refreshInMem (Data.Text.intercalate ":" ["Translations", cityId.getId, messageKey])

findByMerchantOpCityIdMessageKeyLanguageWithInMemcache :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Text -> Lang.Language -> m (Maybe Domain.Types.Translations.Translations)
findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid messageKey language = do
  IM.withInMemCache ["Translations", moid.getId, messageKey, show language] 3600 $ do
    findByMerchantOpCityIdMessageKeyLanguage moid messageKey language

-- | The text of a code-owned key in the reader's language; when no name has a row in that language,
-- its English text read the same way, so a city's English wins over the shared English.
findTextByKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Lang.Language -> RiderTranslationKey -> m (Maybe Text)
findTextByKey cityId language key =
  findTextByKeyInLanguage cityId language key
    |<|>| (if language == Lang.ENGLISH then pure Nothing else findTextByKeyInLanguage cityId Lang.ENGLISH key)

-- | The text of a code-owned key in one language only: the key's own name, then its legacy name,
-- each through config pilot as the city's row, then the shared one (see
-- "Storage.ConfigPilot.Config.Translation").
findTextByKeyInLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Lang.Language -> RiderTranslationKey -> m (Maybe Text)
findTextByKeyInLanguage cityId language key = go (lookupNames key)
  where
    go [] = pure Nothing
    go (name : rest) = do
      mbRow <- findByOpsKey cityId language name
      case mbRow of
        Just row | row.language == language -> pure (Just row.message)
        _ -> go rest

-- | A text ops may have translated under the English text itself (tier names): the
-- keyed row in the reader's language, then the row stored under the text, then the keyed English
-- row, else the text.
findTextByKeyOrOpsText :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Lang.Language -> RiderTranslationKey -> Text -> m Text
findTextByKeyOrOpsText cityId language key text =
  findTextByKeyInLanguage cityId language key >>= \case
    Just keyed -> pure keyed
    Nothing ->
      findByOpsKey cityId language text >>= \case
        Just row -> pure row.message
        Nothing
          | language == Lang.ENGLISH -> pure text
          | otherwise -> fromMaybe text <$> findTextByKeyInLanguage cityId Lang.ENGLISH key

-- | 'findTextByKey' in the reader's language (English when unset), else the code's own English text.
findTextByKeyWithFallback :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Maybe Lang.Language -> RiderTranslationKey -> Text -> m Text
findTextByKeyWithFallback cityId mbLanguage key fallback = fromMaybe fallback <$> findTextByKey cityId (fromMaybe Lang.ENGLISH mbLanguage) key

-- | 'findTextByKeyWithFallback' as the translatable text of an API field.
findTranslatedText :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Maybe Lang.Language -> RiderTranslationKey -> Text -> m TranslatedText
findTranslatedText cityId mbLanguage key fallback = textReadByKey <$> findTextByKeyWithFallback cityId mbLanguage key fallback

-- | 'findTranslatedText' for an optional field: the key's text, else the given text when there is one.
findOptionalTranslatedText :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Maybe Lang.Language -> RiderTranslationKey -> Maybe Text -> m (Maybe TranslatedText)
findOptionalTranslatedText cityId mbLanguage key fallback = fmap textReadByKey . (<|> fallback) <$> findTextByKey cityId (fromMaybe Lang.ENGLISH mbLanguage) key

-- | 'findTextByKeyOrOpsText' as the translatable text of an API field.
findTranslatedTextOrOpsText :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Lang.Language -> RiderTranslationKey -> Text -> m TranslatedText
findTranslatedTextOrOpsText cityId language key text = textReadByKey <$> findTextByKeyOrOpsText cityId language key text

-- | 'findByOpsKey' as the translatable text of an API field; the given text when the key has no row.
findTranslatedTextByOpsKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Lang.Language -> Text -> Text -> m TranslatedText
findTranslatedTextByOpsKey cityId language key fallback = textReadByKey . maybe fallback (.message) <$> findByOpsKey cityId language key

-- | The row of a key that ops typed into a config row (cancellation reasons, pass texts, tier
-- names stored as text): config pilot first, else the table. Text the code owns has a
-- 'RiderTranslationKey' instead.
findByOpsKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Lang.Language -> Text -> m (Maybe Domain.Types.Translations.Translations)
findByOpsKey cityId language key =
  getConfig
    (TranslationDimensions {merchantOperatingCityId = Just cityId.getId, messageKey = key, language = Just language})
    (Just (findByMerchantOpCityIdMessageKeyLanguageWithInMemcache cityId key language))

-- | 'findByOpsKey' without config pilot: the table only, for rows seeded by SQL that config pilot
-- never sees (the WhatsApp bot, see "Storage.Queries.TranslationsExtra").
findByOpsKeyInTable :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Lang.Language -> Text -> m (Maybe Domain.Types.Translations.Translations)
findByOpsKeyInTable cityId language key = findByMerchantOpCityIdMessageKeyLanguageWithInMemcache cityId key language
