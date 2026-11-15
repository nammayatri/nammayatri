{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.Translations where

import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMerchantOperatingCity
import Domain.Types.TranslationKey (keyName)
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
import qualified Sequelize as Se
import qualified Storage.Beam.Translations as Beam
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

-- | The text of a code-owned key in the reader's language: the city's row, then the shared one.
-- When neither exists the English row is read the same way, so a city's English wins over the shared English.
findTextByKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Lang.Language -> RiderTranslationKey -> m (Maybe Text)
findTextByKey cityId language key = do
  mbRow <- byLanguage language
  if language == Lang.ENGLISH || maybe False ((== language) . (.language)) mbRow
    then pure ((.message) <$> mbRow)
    else fmap (.message) <$> byLanguage Lang.ENGLISH
  where
    byLanguage = findByMerchantOpCityIdMessageKeyLanguageWithInMemcache cityId (keyName key)

-- | 'findTextByKey' in the reader's language (English when unset), else the code's own English text.
findTextByKeyWithFallback :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Maybe Lang.Language -> RiderTranslationKey -> Text -> m Text
findTextByKeyWithFallback cityId mbLanguage key fallback = fromMaybe fallback <$> findTextByKey cityId (fromMaybe Lang.ENGLISH mbLanguage) key
