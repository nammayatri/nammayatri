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
  Hedis.safeGet (makeMerchantIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceUsageConfigD 'Unsafe) @MerchantServiceUsageConfig a
    Nothing -> flip whenJust cacheMerchantServiceUsageConfig /=<< Queries.findByMerchantId id

cacheMerchantServiceUsageConfig :: (CacheFlow m r) => MerchantServiceUsageConfig -> m ()
cacheMerchantServiceUsageConfig merchantServiceUsageConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdKey merchantServiceUsageConfig.merchantId
  Hedis.setExp idKey (coerce @MerchantServiceUsageConfig @(MerchantServiceUsageConfigD 'Unsafe) merchantServiceUsageConfig) expTime

makeMerchantIdKey :: Id Merchant -> Text
makeMerchantIdKey id = "CachedQueries:MerchantServiceUsageConfig:Id-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> m ()
clearCache merchantId = do
  Hedis.del (makeMerchantIdKey merchantId)

updateMerchantServiceUsageConfig ::
  MerchantServiceUsageConfig ->
  Esq.SqlDB ()
updateMerchantServiceUsageConfig = Queries.updateMerchantServiceUsageConfig
