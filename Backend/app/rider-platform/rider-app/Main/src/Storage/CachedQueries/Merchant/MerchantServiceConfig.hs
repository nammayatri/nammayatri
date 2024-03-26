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
  ( findByMerchantOpCityIdAndService,
    clearCache,
    cacheMerchantServiceConfig,
    upsertMerchantServiceConfig,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.MerchantServiceConfig as Queries

-- create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceConfig -> m ()
-- create = Queries.create

-- findAllMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m [MerchantServiceConfig]
-- findAllMerchantOpCityId id =
--   Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantOpCityIdKey id) >>= \case
--     Just a -> return $ fmap (coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig) a
--     Nothing -> cacheMerchantServiceConfigForCity id /=<< Queries.findAllMerchantOpCityId id

-- cacheMerchantServiceConfigForCity :: CacheFlow m r => Id MerchantOperatingCity -> [MerchantServiceConfig] -> m ()
-- cacheMerchantServiceConfigForCity merchantOperatingCityId cfg = do
--   expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
--   let merchantIdKey = makeMerchantOpCityIdKey merchantOperatingCityId
--   Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (fmap (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe)) cfg) expTime

-- makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
-- makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:MerchantServiceConfig:MerchantOperatingCityId-" <> id.getId

findByMerchantOpCityIdAndService :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantOpCityIdAndService id mocId serviceName =
  Hedis.safeGet (makeMerchantIdAndServiceKey id mocId serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheMerchantServiceConfig /=<< Queries.findByMerchantOpCityIdAndService id mocId serviceName

cacheMerchantServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheMerchantServiceConfig merchantServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdAndServiceKey merchantServiceConfig.merchantId merchantServiceConfig.merchantOperatingCityId (getServiceName merchantServiceConfig)
  Hedis.setExp idKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) merchantServiceConfig) expTime

makeMerchantIdAndServiceKey :: Id Merchant -> Id DMOC.MerchantOperatingCity -> ServiceName -> Text
makeMerchantIdAndServiceKey id mocId serviceName = "CachedQueries:MerchantServiceConfig:MerchantId-" <> id.getId <> ":MechantOperatingCityId:-" <> mocId.getId <> ":ServiceName-" <> show serviceName

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> Id DMOC.MerchantOperatingCity -> ServiceName -> m ()
clearCache merchantId mocId serviceName = do
  Hedis.del (makeMerchantIdAndServiceKey merchantId mocId serviceName)

upsertMerchantServiceConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceConfig -> m ()
upsertMerchantServiceConfig = Queries.upsertMerchantServiceConfig
