{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantConfigNew
  ( findByMerchantId,
    updateConfig,
    clearCache,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantConfigNew
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.MerchantConfigNew as Queries

findByMerchantId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Merchant -> m (Maybe MerchantConfigNew)
findByMerchantId id =
  Hedis.safeGet (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(MerchantConfigNewD 'Unsafe) @MerchantConfigNew a
    Nothing -> flip whenJust cacheMerchantConfigNew /=<< Queries.findByMerchantId id

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => MerchantConfigNew -> m ()
clearCache config = do
  Hedis.del (makeIdKey config.merchantId)

cacheMerchantConfigNew :: (CacheFlow m r) => MerchantConfigNew -> m ()
cacheMerchantConfigNew config = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey config.merchantId
  Hedis.setExp idKey (coerce @MerchantConfigNew @(MerchantConfigNewD 'Unsafe) config) expTime

makeIdKey :: Id Merchant -> Text
makeIdKey id = "CachedQueries:MerchantConfigNew:Id-" <> id.getId

updateConfig :: MonadFlow m => MerchantConfigNew -> m ()
updateConfig = Queries.updateConfig
