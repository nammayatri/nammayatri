{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Organization
  ( findById,
    update,
    loadAllProviders,
    clearCache,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Organization
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Organization as Queries

findById :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id Organization -> m (Maybe Organization)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just a -> return . Just $ coerce @(OrganizationD 'Unsafe) @Organization a
    Nothing -> flip whenJust cacheOrganization /=<< Queries.findById id

-- Call it after any update
clearCache :: HedisFlow m r => Organization -> m ()
clearCache org =
  Hedis.del (makeIdKey org.id)

cacheOrganization :: (HasCacheConfig r, HedisFlow m r) => Organization -> m ()
cacheOrganization org = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey org.id
  Hedis.setExp idKey (coerce @Organization @(OrganizationD 'Unsafe) org) expTime

makeIdKey :: Id Organization -> Text
makeIdKey id = "CachedQueries:Organization:Id-" <> id.getId

update :: Organization -> Esq.SqlDB ()
update = Queries.update

loadAllProviders :: Esq.Transactionable m => m [Organization]
loadAllProviders = Queries.loadAllProviders
