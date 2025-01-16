{-# LANGUAGE TypeApplications #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceUsageConfig
  ( create,
    findByMerchantOperatingCityId,
    clearCache,
    updateMerchantServiceUsageConfig,
    updateSmsProvidersPriorityList,
    updateWhatsappProvidersPriorityList,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.MerchantServiceUsageConfig
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.MerchantServiceUsageConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceUsageConfig -> m ()
create = Queries.create

getMerchantServiceUsageConfigFromDB :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe MerchantServiceUsageConfig)
getMerchantServiceUsageConfigFromDB id = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOperatingCityIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceUsageConfigD 'Unsafe) @MerchantServiceUsageConfig a
    Nothing -> flip whenJust cacheMerchantServiceUsageConfig /=<< Queries.findByMerchantOperatingCityId id

findByMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m (Maybe MerchantServiceUsageConfig)
findByMerchantOperatingCityId = getMerchantServiceUsageConfigFromDB

cacheMerchantServiceUsageConfig :: (CacheFlow m r) => MerchantServiceUsageConfig -> m ()
cacheMerchantServiceUsageConfig merchantServiceUsageConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantOperatingCityIdKey merchantServiceUsageConfig.merchantOperatingCityId
  Hedis.setExp idKey (coerce @MerchantServiceUsageConfig @(MerchantServiceUsageConfigD 'Unsafe) merchantServiceUsageConfig) expTime

makeMerchantOperatingCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey id = "CachedQueries:MerchantServiceUsageConfig:MerchantOperatingCityId-" <> id.getId

updateSmsProvidersPriorityList :: (CacheFlow m r, EsqDBFlow m r) => [Kernel.External.SMS.Types.SmsService] -> Id MerchantOperatingCity -> m ()
updateSmsProvidersPriorityList smsProvidersPriorityList merchantOperatingCityId = do
  Queries.updateSmsProvidersPriorityList smsProvidersPriorityList merchantOperatingCityId
  clearCache merchantOperatingCityId

updateWhatsappProvidersPriorityList :: (CacheFlow m r, EsqDBFlow m r) => [Kernel.External.Whatsapp.Types.WhatsappService] -> Id MerchantOperatingCity -> m ()
updateWhatsappProvidersPriorityList whatsappProvidersPriorityList merchantOperatingCityId = do
  Queries.updateWhatsappProvidersPriorityList whatsappProvidersPriorityList merchantOperatingCityId
  clearCache merchantOperatingCityId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache merchantOperatingCityId = do
  Hedis.del (makeMerchantOperatingCityIdKey merchantOperatingCityId)

updateMerchantServiceUsageConfig :: (CacheFlow m r, EsqDBFlow m r) => MerchantServiceUsageConfig -> m ()
updateMerchantServiceUsageConfig = Queries.updateMerchantServiceUsageConfig
