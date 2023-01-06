module SharedLogic.TransporterConfig where

import Beckn.External.FCM.Types
import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Merchant as DM
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
  transporterConfig <- findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  return $ transporterConfig.fcmConfig
