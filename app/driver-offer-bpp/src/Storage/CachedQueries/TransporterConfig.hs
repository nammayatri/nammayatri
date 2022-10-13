{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.TransporterConfig
  ( findByOrgId,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Organization (Organization)
import Domain.Types.TransporterConfig
import qualified Storage.Queries.TransporterConfig as Queries

findByOrgId :: (HedisFlow m r, EsqDBFlow m r) => Id Organization -> m (Maybe TransporterConfig)
findByOrgId id =
  Hedis.get (makeOrgIdKey id) >>= \case
    Just a -> return . Just $ coerce @(TransporterConfigD 'Unsafe) @TransporterConfig a
    Nothing -> flip whenJust cacheTransporterConfig /=<< Queries.findByOrgId id

cacheTransporterConfig :: HedisFlow m r => TransporterConfig -> m ()
cacheTransporterConfig cfg = do
  let orgIdKey = makeOrgIdKey cfg.organizationId
  Hedis.setExp orgIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime
  where
    expTime = 60 * 60 * 24

makeOrgIdKey :: Id Organization -> Text
makeOrgIdKey id = "CachedQueries:TransporterConfig:OrgId-" <> id.getId
