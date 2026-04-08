{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.Translations where

import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMerchantOperatingCity
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
  let makeKey mbCityId =
        "CachedQueries:Translations:"
          <> ":MessageKey-"
          <> show messageKey
          <> ":Language-"
          <> show language
          <> ":City-"
          <> fromMaybe "" mbCityId
      citySpecificKey = makeKey (Just $ Kernel.Types.Id.getId merchantOperatingCityId)
      globalKey = makeKey Nothing
  Hedis.safeGet citySpecificKey >>= \case
    Just cached -> pure (Just cached)
    Nothing ->
      Hedis.safeGet globalKey >>= \case
        Just cached -> pure (Just cached)
        Nothing -> do
          result <- Queries.findByMerchantOpCityIdMessageKeyLanguage merchantOperatingCityId messageKey language
          whenJust result $ \dataToBeCached -> do
            expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
            let cacheKey = makeKey (Kernel.Types.Id.getId <$> dataToBeCached.merchantOperatingCityId)
            Hedis.setExp cacheKey dataToBeCached expTime
          pure result

findByMerchantOpCityIdMessageKeyLanguageWithInMemcache :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMerchantOperatingCity.MerchantOperatingCity -> Text -> Lang.Language -> m (Maybe Domain.Types.Translations.Translations)
findByMerchantOpCityIdMessageKeyLanguageWithInMemcache moid messageKey language = do
  IM.withInMemCache ["Translations", moid.getId, messageKey, show language] 3600 $ do
    findByMerchantOpCityIdMessageKeyLanguage moid messageKey language
