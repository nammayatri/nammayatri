{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.TransporterConfig
  ( findValueByMerchantIdAndKey,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.TransporterConfig
import GHC.Base (coerce)
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.TransporterConfig as Queries

findValueByMerchantIdAndKey :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Merchant -> ConfigKey -> m (Maybe TransporterConfig)
findValueByMerchantIdAndKey merchantId key =
  Hedis.get (makeMerchantIdKey merchantId key) >>= \case
    Just a -> return . Just $ coerce @(TransporterConfigD 'Unsafe) @TransporterConfig a
    Nothing -> flip whenJust cacheTransporterConfig /=<< Queries.findValueByMerchantIdAndKey merchantId key

cacheTransporterConfig :: (HasCacheConfig r, HedisFlow m r) => TransporterConfig -> m ()
cacheTransporterConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let merchantIdKey = makeMerchantIdKey cfg.transporterId cfg.key
  Hedis.setExp merchantIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime

makeMerchantIdKey :: Id Merchant -> ConfigKey -> Text
makeMerchantIdKey id key = "CachedQueries:TransporterConfig:MerchantId-" <> id.getId <> ":Key-" <> show key
