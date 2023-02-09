{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FarePolicy
  ( findById,
    findByMerchantIdAndVariant,
    findAllByMerchantId,
    clearCache,
    update,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.FarePolicy
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.FarePolicy as Queries

findById :: (CacheFlow m r, EsqDBFlow m r) => Id FarePolicy -> m (Maybe FarePolicy)
findById id =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
    Nothing -> flip whenJust cacheFarePolicy /=<< Queries.findById id

findByMerchantIdAndVariant ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Vehicle.Variant ->
  m (Maybe FarePolicy)
findByMerchantIdAndVariant merchantId vehVar =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantIdVehVarKey merchantId vehVar) >>= \case
    Nothing -> findAndCache
    Just id ->
      Hedis.withCrossAppRedis (Hedis.safeGet $ makeIdKey id) >>= \case
        Just a -> return . Just $ coerce @(FarePolicyD 'Unsafe) @FarePolicy a
        Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheFarePolicy /=<< Queries.findByMerchantIdAndVariant merchantId vehVar

findAllByMerchantId :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> m [FarePolicy]
findAllByMerchantId merchantId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeAllMerchantIdKey merchantId) >>= \case
    Just a -> return $ fmap (coerce @(FarePolicyD 'Unsafe) @FarePolicy) a
    Nothing -> cacheRes /=<< Queries.findAllByMerchantId merchantId
  where
    cacheRes fps = do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.withCrossAppRedis $ Hedis.setExp (makeAllMerchantIdKey merchantId) (coerce @[FarePolicy] @[FarePolicyD 'Unsafe] fps) expTime

cacheFarePolicy :: (CacheFlow m r) => FarePolicy -> m ()
cacheFarePolicy fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey fp.id
  Hedis.withCrossAppRedis $ do
    Hedis.setExp idKey (coerce @FarePolicy @(FarePolicyD 'Unsafe) fp) expTime
    Hedis.setExp (makeMerchantIdVehVarKey fp.merchantId fp.vehicleVariant) idKey expTime

makeIdKey :: Id FarePolicy -> Text
makeIdKey id = "driver-offer:CachedQueries:FarePolicy:Id-" <> id.getId

makeMerchantIdVehVarKey :: Id Merchant -> Vehicle.Variant -> Text
makeMerchantIdVehVarKey merchantId vehVar = "driver-offer:CachedQueries:FarePolicy:MerchantId-" <> merchantId.getId <> ":VehVar-" <> show vehVar

makeAllMerchantIdKey :: Id Merchant -> Text
makeAllMerchantIdKey merchantId = "driver-offer:CachedQueries:FarePolicy:MerchantId-" <> merchantId.getId <> ":All"

-- Call it after any update
clearCache :: HedisFlow m r => FarePolicy -> m ()
clearCache fp = Hedis.withCrossAppRedis $ do
  Hedis.del (makeIdKey fp.id)
  Hedis.del (makeMerchantIdVehVarKey fp.merchantId fp.vehicleVariant)
  Hedis.del (makeAllMerchantIdKey fp.merchantId)

update :: FarePolicy -> Esq.SqlDB ()
update = Queries.update
