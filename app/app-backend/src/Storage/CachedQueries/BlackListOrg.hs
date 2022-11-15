{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.BlackListOrg
  ( findByShortId,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.BlackListOrg
import Domain.Types.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.BlackListOrg as Queries

findByShortId :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => ShortId BlackListOrg -> m (Maybe BlackListOrg)
findByShortId shortId_ =
  Hedis.get (makeShortIdKey shortId_) >>= \case
    Just a -> return . Just $ coerce @(BlackListOrgD 'Unsafe) @BlackListOrg a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheOrganization /=<< Queries.findByShortId shortId_

cacheOrganization :: (HasCacheConfig r, HedisFlow m r) => BlackListOrg -> m ()
cacheOrganization org = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeShortIdKey org.shortId) (coerce @BlackListOrg @(BlackListOrgD 'Unsafe) org) expTime

makeShortIdKey :: ShortId BlackListOrg -> Text
makeShortIdKey shortId = "CachedQueries:BlackListOrg:ShortId-" <> shortId.getShortId
