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

module Storage.CachedQueries.Merchant.MerchantServiceUsageConfig
  ( findByMerchantId,
    clearCache,
    updateMerchantServiceUsageConfig,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantServiceUsageConfig
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant.MerchantServiceUsageConfig as Queries

findByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceUsageConfigD 'Unsafe) @MerchantServiceUsageConfig a
    Nothing -> flip whenJust cacheMerchantServiceUsageConfig /=<< Queries.findByMerchantId id

cacheMerchantServiceUsageConfig :: CacheFlow m r => MerchantServiceUsageConfig -> m ()
cacheMerchantServiceUsageConfig merchantServiceUsageConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdKey merchantServiceUsageConfig.merchantId
  Hedis.withCrossAppRedis $ Hedis.setExp idKey (coerce @MerchantServiceUsageConfig @(MerchantServiceUsageConfigD 'Unsafe) merchantServiceUsageConfig) expTime

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey id = "driver-offer:CachedQueries:MerchantServiceUsageConfig:MerchantId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> m ()
clearCache merchantId = do
  Hedis.withCrossAppRedis $ Hedis.del (makeMerchantIdKey merchantId)

updateMerchantServiceUsageConfig ::
  CacheFlow m r =>
  Finalize m ->
  MerchantServiceUsageConfig ->
  Esq.SqlDB ()
updateMerchantServiceUsageConfig finalize merchantServiceUsageConfig = do
  Queries.updateMerchantServiceUsageConfig merchantServiceUsageConfig
  finalize $ clearCache merchantServiceUsageConfig.merchantId
