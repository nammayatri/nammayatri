{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.TransporterConfig
  ( findValueByOrgIdAndKey,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Common
import Domain.Types.Organization (Organization)
import Domain.Types.TransporterConfig
import GHC.Base (coerce)
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.TransporterConfig as Queries

findValueByOrgIdAndKey :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Organization -> ConfigKey -> m (Maybe TransporterConfig)
findValueByOrgIdAndKey orgId key =
  Hedis.get (makeOrgIdKey orgId key) >>= \case
    Just a -> return . Just $ coerce @(TransporterConfigD 'Unsafe) @TransporterConfig a
    Nothing -> flip whenJust cacheTransporterConfig /=<< Queries.findValueByOrgIdAndKey orgId key

cacheTransporterConfig :: (HasCacheConfig r, HedisFlow m r) => TransporterConfig -> m ()
cacheTransporterConfig cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let orgIdKey = makeOrgIdKey cfg.transporterId cfg.key
  Hedis.setExp orgIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime

makeOrgIdKey :: Id Organization -> ConfigKey -> Text
makeOrgIdKey id key = "CachedQueries:TransporterConfig:OrgId-" <> id.getId <> ":Key-" <> show key
