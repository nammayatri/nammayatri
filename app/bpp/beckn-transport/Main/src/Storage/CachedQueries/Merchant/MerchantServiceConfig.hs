{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceConfig
  ( findByMerchantIdAndService,
    clearCache,
    create,
    updateMerchantServiceConfig,
    cacheMerchantServiceConfig,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantServiceConfig
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant.MerchantServiceConfig as Queries

findByMerchantIdAndService :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService id serviceName =
  Hedis.get (makeMerchantIdAndServiceKey id serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheMerchantServiceConfig /=<< Queries.findByMerchantIdAndService id serviceName

cacheMerchantServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheMerchantServiceConfig orgServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdAndServiceKey orgServiceConfig.merchantId (getServiceName orgServiceConfig)
  Hedis.setExp idKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) orgServiceConfig) expTime

makeMerchantIdAndServiceKey :: Id Merchant -> ServiceName -> Text
makeMerchantIdAndServiceKey id serviceName = "CachedQueries:MerchantServiceConfig:MerchantId-" <> id.getId <> ":ServiceName-" <> show serviceName

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id Merchant -> ServiceName -> m ()
clearCache merchantId serviceName = do
  Hedis.del (makeMerchantIdAndServiceKey merchantId serviceName)

create :: MerchantServiceConfig -> Esq.SqlDB ()
create = Queries.create

updateMerchantServiceConfig :: Id Merchant -> ServiceConfig -> Esq.SqlDB ()
updateMerchantServiceConfig = Queries.updateMerchantServiceConfig
