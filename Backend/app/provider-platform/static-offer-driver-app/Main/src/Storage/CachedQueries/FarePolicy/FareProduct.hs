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

module Storage.CachedQueries.FarePolicy.FareProduct
  ( findEnabledByMerchantId,
    findEnabledByMerchantIdAndType,
    insertIfNotExist,
    delete,
    clearCache,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy.FareProduct
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy.FareProduct as Queries

findEnabledByMerchantId ::
  forall m r.
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  m [FareProduct]
findEnabledByMerchantId id =
  Hedis.safeGet (makeAllMerchantIdKey id) >>= \case
    Just a -> return $ coerce @(FareProductD 'Unsafe) @FareProduct <$> a
    Nothing -> cacheRes /=<< Queries.findEnabledByMerchantId id (Proxy @m)
  where
    cacheRes fareProds = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllMerchantIdKey id) (coerce @[FareProduct] @[FareProductD 'Unsafe] fareProds) expTime

findEnabledByMerchantIdAndType ::
  forall m r.
  (CacheFlow m r, EsqDBFlow m r) =>
  Maybe FareProductType ->
  Id Merchant ->
  m [FareProduct]
findEnabledByMerchantIdAndType mbFPType merchantId =
  Hedis.safeGet (makeAllMerchantIdTypeKey merchantId mbFPType) >>= \case
    Just a -> return $ fmap (coerce @(FareProductD 'Unsafe) @FareProduct) a
    Nothing -> cacheRes /=<< Queries.findEnabledByMerchantIdAndType mbFPType merchantId (Proxy @m)
  where
    cacheRes fareProds = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllMerchantIdTypeKey merchantId mbFPType) (coerce @[FareProduct] @[FareProductD 'Unsafe] fareProds) expTime

baseKey :: Text
baseKey = "CachedQueries:FareProduct"

makeAllMerchantIdKey :: Id Merchant -> Text
makeAllMerchantIdKey id = baseKey <> ":MerchantId-" <> id.getId

makeAllMerchantIdTypeKey :: Id Merchant -> Maybe FareProductType -> Text
makeAllMerchantIdTypeKey merchantId mbFPType = baseKey <> ":MerchantId-" <> merchantId.getId <> ":Type-" <> fpTypeStr <> ":All"
  where
    fpTypeStr = maybe "None" show mbFPType

-- Call it after any update
clearCache :: HedisFlow m r => Id Merchant -> FareProductType -> m ()
clearCache merchantId fpType = do
  Hedis.del (makeAllMerchantIdKey merchantId)
  Hedis.del (makeAllMerchantIdTypeKey merchantId (Just fpType))
  Hedis.del (makeAllMerchantIdTypeKey merchantId Nothing)

insertIfNotExist ::
  HedisFlow m r =>
  Id Merchant ->
  FareProductType ->
  Esq.SqlDB m ()
insertIfNotExist merchantId fareProductType = do
  Queries.insertIfNotExist merchantId fareProductType
  Esq.finalize $ clearCache merchantId fareProductType

delete ::
  HedisFlow m r =>
  Id Merchant ->
  FareProductType ->
  Esq.SqlDB m ()
delete merchantId fareProductType = do
  Queries.delete merchantId fareProductType
  Esq.finalize $ clearCache merchantId fareProductType
