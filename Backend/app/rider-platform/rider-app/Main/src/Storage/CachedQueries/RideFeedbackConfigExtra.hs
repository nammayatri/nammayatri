{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.CachedQueries.RideFeedbackConfigExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RideFeedbackConfig as DRFC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RideFeedbackConfig as Queries

-- | Enabled questions of a city. Polled by every in-ride screen, so it is cached in memory and in Redis,
-- and an empty list is cached too (most cities have no questions and must not hit the DB on each poll).
findAllEnabledByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m [DRFC.RideFeedbackConfig]
findAllEnabledByMerchantOpCityId merchantOpCityId = do
  let key = makeEnabledByMerchantOpCityIdKey merchantOpCityId
  IM.withInMemCache [key] inMemCacheTtl $
    Hedis.safeGet key >>= \case
      Just configs -> pure configs
      Nothing -> do
        configs <- Queries.findAllByMerchantOperatingCityIdAndEnabled merchantOpCityId True
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp key configs expTime
        pure configs

-- | Clears the Redis copy; pods drop their in-memory copy within 'inMemCacheTtl'.
clearEnabledByMerchantOpCityIdCache :: CacheFlow m r => Id DMOC.MerchantOperatingCity -> m ()
clearEnabledByMerchantOpCityIdCache = Hedis.del . makeEnabledByMerchantOpCityIdKey

makeEnabledByMerchantOpCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeEnabledByMerchantOpCityIdKey merchantOpCityId = "CachedQueries:RideFeedbackConfig:Enabled:MOCId-" <> merchantOpCityId.getId

inMemCacheTtl :: Seconds
inMemCacheTtl = 300
