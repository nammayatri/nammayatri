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

module Storage.CachedQueries.KioskLocation where

import Domain.Types.KioskLocation
import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.KioskLocation as Queries

fetchAllKioskLocationsByMerchant :: (CacheFlow m r, MonadFlow m) => Id Merchant -> m [KioskLocation]
fetchAllKioskLocationsByMerchant (Id merchantId) =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey (Id merchantId)) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantId (Id merchantId) /=<< Queries.fetchAllKioskLocationsByMerchant (Id merchantId)

cacheByMerchantId :: CacheFlow m r => Id Merchant -> [KioskLocation] -> m ()
cacheByMerchantId (Id merchantId) locations = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeMerchantIdKey (Id merchantId)) locations expTime

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey merchantId = "driver-offer:CachedQueries:KioskLocation:MerchantId-" <> merchantId.getId
