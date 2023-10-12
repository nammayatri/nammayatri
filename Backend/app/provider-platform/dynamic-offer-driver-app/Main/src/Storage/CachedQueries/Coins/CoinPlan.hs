{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Coins.CoinPlan where

import Domain.Types.Coins.CoinPlan
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Coins.CoinPlan as Queries

getCoinPlanDetails :: (MonadFlow m, CacheFlow m r) => Text -> m (Maybe CoinPlan)
getCoinPlanDetails coinPlanId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeCoinPlanIdKey coinPlanId) >>= \case
    Just a -> pure a
    Nothing -> cacheByCoinPlanId coinPlanId /=<< Queries.getCoinPlanDetails coinPlanId

cacheByCoinPlanId :: (CacheFlow m r) => Text -> Maybe CoinPlan -> m ()
cacheByCoinPlanId coinPlanId coinPlan = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCoinPlanIdKey coinPlanId) coinPlan expTime

----- CACHED KEYS -----
fetchAllCoinPlan :: (CacheFlow m r, MonadFlow m) => Id DM.Merchant -> m [CoinPlan]
fetchAllCoinPlan merchantId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeAllPlanKey merchantId) >>= \case
    Just a -> pure a
    Nothing -> cacheAllPlan merchantId /=<< Queries.getCoinPlans merchantId

cacheAllPlan :: (CacheFlow m r) => Id DM.Merchant -> [CoinPlan] -> m ()
cacheAllPlan merchantId plans = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeAllPlanKey merchantId) plans expTime

makeAllPlanKey :: Id DM.Merchant -> Text
makeAllPlanKey (Id merchantId) = "driver-offer:CachedQueries:CoinPlan:CoinPlanId-ALL:MerchantId-" <> merchantId

makeCoinPlanIdKey :: Text -> Text
makeCoinPlanIdKey id = "driver-offer:CachedQueries:CoinPlan:CoinPlanId-" <> id
