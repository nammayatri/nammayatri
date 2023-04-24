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

module Storage.CachedQueries.SpecialZoneCategoryPriority where

import Domain.Types.Merchant
import Domain.Types.SpecialZoneCategoryPriority
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.SpecialZoneCategoryPriority as Queries

findByMerchantIdAndCategory :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Merchant -> Text -> m (Maybe SpecialZoneCategoryPriority)
findByMerchantIdAndCategory merchantId category =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeByMerchantIdAndCategoryKey merchantId category) >>= \case
    Just a -> pure a
    Nothing -> cacheSpecialZoneCategoryPriorityByMerchantIdAndCategory merchantId category /=<< Queries.findByMerchantIdAndCategory merchantId category

cacheSpecialZoneCategoryPriorityByMerchantIdAndCategory :: (CacheFlow m r) => Id Merchant -> Text -> Maybe SpecialZoneCategoryPriority -> m ()
cacheSpecialZoneCategoryPriorityByMerchantIdAndCategory merchantId category specialZoneCategoryPriority = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeByMerchantIdAndCategoryKey merchantId category) specialZoneCategoryPriority expTime

makeByMerchantIdAndCategoryKey :: Id Merchant -> Text -> Text
makeByMerchantIdAndCategoryKey merchantId category = "driver-offer:CachedQueries:SpecialZoneCategoryPriority:MerchantId-" <> show merchantId <> ":Category-" <> show category
