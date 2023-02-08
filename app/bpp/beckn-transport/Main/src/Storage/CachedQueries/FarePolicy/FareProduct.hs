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
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  m [FareProduct]
findEnabledByMerchantId id =
  Hedis.safeGet (makeAllMerchantIdKey id) >>= \case
    Just a -> return $ coerce @(FareProductD 'Unsafe) @FareProduct <$> a
    Nothing -> cacheRes /=<< Queries.findEnabledByMerchantId id
  where
    cacheRes fareProds = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeAllMerchantIdKey id) (coerce @[FareProduct] @[FareProductD 'Unsafe] fareProds) expTime

findEnabledByMerchantIdAndType ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Maybe FareProductType ->
  Id Merchant ->
  m [FareProduct]
findEnabledByMerchantIdAndType mbFPType merchantId =
  Hedis.safeGet (makeAllMerchantIdTypeKey merchantId mbFPType) >>= \case
    Just a -> return $ fmap (coerce @(FareProductD 'Unsafe) @FareProduct) a
    Nothing -> cacheRes /=<< Queries.findEnabledByMerchantIdAndType mbFPType merchantId
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
  Id Merchant ->
  FareProductType ->
  Esq.SqlDB ()
insertIfNotExist = Queries.insertIfNotExist

delete ::
  Id Merchant ->
  FareProductType ->
  Esq.SqlDB ()
delete = Queries.delete
