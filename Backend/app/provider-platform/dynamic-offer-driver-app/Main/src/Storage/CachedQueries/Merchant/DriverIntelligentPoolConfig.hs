{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig
  ( findByMerchantId,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.DriverIntelligentPoolConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant.DriverIntelligentPoolConfig as Queries

findByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m (Maybe DriverIntelligentPoolConfig)
findByMerchantId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey id) >>= \case
    Just a -> return . Just $ coerce @(DriverIntelligentPoolConfigD 'Unsafe) @DriverIntelligentPoolConfig a
    Nothing -> flip whenJust cacheDriverIntelligentPoolConfig /=<< Queries.findByMerchantId id

cacheDriverIntelligentPoolConfig :: CacheFlow m r => DriverIntelligentPoolConfig -> m ()
cacheDriverIntelligentPoolConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantIdKey cfg.merchantId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @DriverIntelligentPoolConfig @(DriverIntelligentPoolConfigD 'Unsafe) cfg) expTime

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey id = "driver-offer:CachedQueries:DriverIntelligentPoolConfig:MerchantId-" <> id.getId
