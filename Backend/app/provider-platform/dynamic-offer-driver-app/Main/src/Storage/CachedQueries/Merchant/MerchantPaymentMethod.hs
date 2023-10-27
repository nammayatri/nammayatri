{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantPaymentMethod
  ( findAllByMerchantOpCityId,
    findByIdAndMerchantOpCityId,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.MerchantPaymentMethod
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.MerchantPaymentMethod as Queries

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m [MerchantPaymentMethod]
findAllByMerchantOpCityId id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdKey id) >>= \case
    Just a -> return $ fmap (coerce @(MerchantPaymentMethodD 'Unsafe) @MerchantPaymentMethod) a
    Nothing -> cacheMerchantPaymentMethods id /=<< Queries.findAllByMerchantOpCityId id

findByIdAndMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantPaymentMethod -> Id MerchantOperatingCity -> m (Maybe MerchantPaymentMethod)
findByIdAndMerchantOpCityId id merchantId = find (\mpm -> mpm.id == id) <$> findAllByMerchantOpCityId merchantId

cacheMerchantPaymentMethods :: (CacheFlow m r) => Id MerchantOperatingCity -> [MerchantPaymentMethod] -> m ()
cacheMerchantPaymentMethods merchantId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantIdKey merchantId
  Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @[MerchantPaymentMethod] @[MerchantPaymentMethodD 'Unsafe] cfg) expTime

makeMerchantIdKey :: Id MerchantOperatingCity -> Text
makeMerchantIdKey id = "driver-offer:CachedQueries:MerchantPaymentMethod:MerchantOpCityId-" <> id.getId
