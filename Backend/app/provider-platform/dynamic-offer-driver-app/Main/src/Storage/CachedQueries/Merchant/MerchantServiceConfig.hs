{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceConfig
  ( findByMerchantIdAndServiceWithCity,
    findOne,
    clearCache,
    cacheMerchantServiceConfig,
    upsertMerchantServiceConfig,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.MerchantServiceConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.MerchantServiceConfig as Queries

findByMerchantIdAndServiceWithCity ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  ServiceName ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe MerchantServiceConfig)
findByMerchantIdAndServiceWithCity id serviceName opCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdAndServiceWithCityKey id serviceName opCityId) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust (cacheMerchantServiceConfig opCityId) /=<< Queries.findByMerchantIdAndServiceWithCity id serviceName opCityId

-- FIXME this is temprorary solution for backward compatibility
findOne :: (CacheFlow m r, EsqDBFlow m r) => ServiceName -> m (Maybe MerchantServiceConfig)
findOne serviceName = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeServiceNameKey serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheOneServiceConfig /=<< Queries.findOne serviceName

cacheOneServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheOneServiceConfig orgServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let serviceNameKey = makeServiceNameKey (getServiceName orgServiceConfig)
  Hedis.withCrossAppRedis $ Hedis.setExp serviceNameKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) orgServiceConfig) expTime

cacheMerchantServiceConfig :: CacheFlow m r => Id DMOC.MerchantOperatingCity -> MerchantServiceConfig -> m ()
cacheMerchantServiceConfig merchantOperatingCity orgServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdAndServiceWithCityKey orgServiceConfig.merchantId (getServiceName orgServiceConfig) merchantOperatingCity
  Hedis.withCrossAppRedis $ Hedis.setExp idKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) orgServiceConfig) expTime

makeMerchantIdAndServiceWithCityKey :: Id Merchant -> ServiceName -> Id DMOC.MerchantOperatingCity -> Text
makeMerchantIdAndServiceWithCityKey id serviceName opCityId = "driver-offer:CachedQueries:MerchantServiceConfig:MerchantId-" <> id.getId <> ":ServiceName-" <> show serviceName <> ":OpCity-" <> opCityId.getId

makeServiceNameKey :: ServiceName -> Text
makeServiceNameKey serviceName = "driver-offer:CachedQueries:MerchantServiceConfig:ServiceName-" <> show serviceName

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> ServiceName -> Id DMOC.MerchantOperatingCity -> m ()
clearCache merchantId serviceName opCity = do
  Hedis.withCrossAppRedis $ Hedis.del (makeMerchantIdAndServiceWithCityKey merchantId serviceName opCity)
  Hedis.withCrossAppRedis $ Hedis.del (makeServiceNameKey serviceName)

upsertMerchantServiceConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceConfig -> Id DMOC.MerchantOperatingCity -> m ()
upsertMerchantServiceConfig = Queries.upsertMerchantServiceConfig
