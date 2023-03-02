{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceConfig
  ( findByMerchantIdAndService,
    clearCache,
    cacheMerchantServiceConfig,
    upsertMerchantServiceConfig,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantServiceConfig
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant.MerchantServiceConfig as Queries

findByMerchantIdAndService :: forall m r. (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService id serviceName =
  Hedis.safeGet (makeMerchantIdAndServiceKey id serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheMerchantServiceConfig /=<< Queries.findByMerchantIdAndService id serviceName (Proxy @m)

cacheMerchantServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheMerchantServiceConfig merchantServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdAndServiceKey merchantServiceConfig.merchantId (getServiceName merchantServiceConfig)
  Hedis.setExp idKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) merchantServiceConfig) expTime

makeMerchantIdAndServiceKey :: Id Merchant -> ServiceName -> Text
makeMerchantIdAndServiceKey id serviceName = "CachedQueries:MerchantServiceConfig:MerchantId-" <> id.getId <> ":ServiceName-" <> show serviceName

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> ServiceName -> m ()
clearCache merchantId serviceName = do
  Hedis.del (makeMerchantIdAndServiceKey merchantId serviceName)

upsertMerchantServiceConfig :: Hedis.HedisFlow m r => MerchantServiceConfig -> Esq.SqlDB m ()
upsertMerchantServiceConfig merchantServiceConfig = do
  Queries.upsertMerchantServiceConfig merchantServiceConfig
  Esq.finalize $ clearCache (merchantServiceConfig.merchantId) (getServiceName merchantServiceConfig)
