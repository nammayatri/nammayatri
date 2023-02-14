 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.TransporterConfig where

import qualified Domain.Types.Merchant as DM
import Kernel.External.FCM.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
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
