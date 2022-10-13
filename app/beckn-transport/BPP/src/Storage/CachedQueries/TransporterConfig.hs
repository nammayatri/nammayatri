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
import qualified Storage.Queries.TransporterConfig as Queries

findValueByOrgIdAndKey :: (HedisFlow m r, EsqDBFlow m r) => Id Organization -> ConfigKey -> m (Maybe TransporterConfig)
findValueByOrgIdAndKey orgId key =
  Hedis.get (makeOrgIdKey orgId key) >>= \case
    Just a -> return . Just $ coerce @(TransporterConfigD 'Unsafe) @TransporterConfig a
    Nothing -> flip whenJust cacheTransporterConfig /=<< Queries.findValueByOrgIdAndKey orgId key

cacheTransporterConfig :: HedisFlow m r => TransporterConfig -> m ()
cacheTransporterConfig cfg = do
  let orgIdKey = makeOrgIdKey cfg.transporterId cfg.key
  Hedis.setExp orgIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime
  where
    expTime = 60 * 60 * 24

makeOrgIdKey :: Id Organization -> ConfigKey -> Text
makeOrgIdKey id key = "CachedQueries:TransporterConfig:OrgId:" <> id.getId <> ":Key:" <> show key
