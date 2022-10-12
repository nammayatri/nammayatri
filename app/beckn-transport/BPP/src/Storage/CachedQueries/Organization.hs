{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Domain.Types.Organization
import qualified Storage.Queries.Organization as Queries

newtype StoredOrganization = StoredOrganization Organization
  deriving newtype (Generic)
  deriving anyclass (FromJSON, ToJSON)

findById :: (HedisFlow m r, EsqDBFlow m r) => Id Organization -> m (Maybe Organization)
findById id =
  Hedis.get (makeIdKey id) >>= \case
    Just (StoredOrganization a) -> return $ Just a
    Nothing -> flip whenJust cacheOrganization /=<< Queries.findById id

-- Call it after any update
clearCache :: HedisFlow m r => Organization -> m ()
clearCache org =
  Hedis.del (makeIdKey org.id)

cacheOrganization :: HedisFlow m r => Organization -> m ()
cacheOrganization org = do
  let idKey = makeIdKey org.id
  Hedis.setExp idKey (StoredOrganization org) expTime
  where
    expTime = 60 * 60 * 24

makeIdKey :: Id Organization -> Text
makeIdKey id = "CachedQueries:Organization:Id:" <> id.getId

update :: Organization -> Esq.SqlDB ()
update = Queries.update

loadAllProviders :: Esq.Transactionable m => m [Organization]
loadAllProviders = Queries.loadAllProviders
