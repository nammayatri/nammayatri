module SharedLogic.TransporterConfig where

import Beckn.External.FCM.Types
import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Text (intercalate)
import qualified Domain.Types.Merchant as DM
import Domain.Types.TransporterConfig
import Storage.CachedQueries.CacheConfig (CacheFlow, HasCacheConfig)
import Storage.CachedQueries.TransporterConfig

makeFCMConfigKey :: Id DM.Merchant -> Text
makeFCMConfigKey id = "CachedQueries:TransporterConfig:FCMConfig-" <> id.getId

findFCMConfigByMerchantId :: (MonadFlow m, HasCacheConfig r, Hedis.HedisFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m FCMConfig
findFCMConfigByMerchantId merchantId =
  Hedis.get (makeFCMConfigKey merchantId) >>= \case
    Just conf -> return conf
    Nothing -> cacheFCMConfig merchantId /=<< findFCMConfigByMerchantId' merchantId

cacheFCMConfig :: (CacheFlow m r) => Id DM.Merchant -> FCMConfig -> m ()
cacheFCMConfig merchantId fcmConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let fcmConfigKey = makeFCMConfigKey merchantId
  Hedis.setExp fcmConfigKey fcmConfig expTime

findFCMConfigByMerchantId' :: (MonadFlow m, HasCacheConfig r, Hedis.HedisFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m FCMConfig
findFCMConfigByMerchantId' merchantId = do
  mbFcmAccountFileConfig <- findValueByMerchantIdAndKey merchantId (ConfigKey "fcm_service_account")
  mbFcmUrlConfig <- findValueByMerchantIdAndKey merchantId (ConfigKey "fcm_url")
  mbFcmTokenKeyPrefix <- findValueByMerchantIdAndKey merchantId (ConfigKey "fcm_token_key_prefix")
  case (mbFcmUrlConfig, mbFcmAccountFileConfig, mbFcmTokenKeyPrefix) of
    (Just fcmUrlConfig, Just fcmAccountFileConfig, Just fcmTokenKeyPrefixConfig) -> do
      fcmUrl <- parseBaseUrl (value fcmUrlConfig)
      return $
        FCMConfig
          { fcmTokenKeyPrefix = fcmTokenKeyPrefixConfig.value,
            fcmServiceAccount = fcmAccountFileConfig.value,
            ..
          }
    _ -> do
      let errUrl = toErrorMessage mbFcmUrlConfig "fcm_url"
      let errAccountFile = toErrorMessage mbFcmAccountFileConfig "fcm_service_account"
      let errToken = toErrorMessage mbFcmTokenKeyPrefix "fcm_token_key_prefix"
      throwError $ MerchantServiceUsageConfigNotFound (intercalate ", " [errUrl, errAccountFile, errToken])
  where
    toErrorMessage (Just _) _ = ""
    toErrorMessage Nothing fieldName = fieldName
