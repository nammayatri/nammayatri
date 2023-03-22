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

module Storage.CachedQueries.Merchant.DriverPoolConfig
  ( findAllByMerchantId,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.DriverPoolConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant.DriverPoolConfig as Queries

findAllByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m [DriverPoolConfig]
findAllByMerchantId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey id) >>= \case
    Just a -> return $ fmap (coerce @(DriverPoolConfigD 'Unsafe) @DriverPoolConfig) a
    Nothing -> cacheDriverPoolConfigs id /=<< Queries.findAllByMerchantId id

cacheDriverPoolConfigs :: CacheFlow m r => Id Merchant -> [DriverPoolConfig] -> m ()
cacheDriverPoolConfigs merchantId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantIdKey merchantId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @[DriverPoolConfig] @[DriverPoolConfigD 'Unsafe] cfg) expTime

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey id = "driver-offer:CachedQueries:DriverPoolConfig:MerchantId-" <> id.getId
