{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Organization
  ( findByShortId,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Organization
import qualified Storage.Queries.Organization as Queries

newtype StoredOrganization = StoredOrganization Organization
  deriving newtype (Generic)
  deriving anyclass (FromJSON, ToJSON)

findByShortId :: (HedisFlow m r, EsqDBFlow m r) => ShortId Organization -> m (Maybe Organization)
findByShortId shortId_ =
  Hedis.get (makeShortIdKey shortId_) >>= \case
    Just (StoredOrganization a) -> return $ Just a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheOrganization /=<< Queries.findByShortId shortId_

cacheOrganization :: HedisFlow m r => Organization -> m ()
cacheOrganization org = do
  Hedis.setExp (makeShortIdKey org.shortId) (StoredOrganization org) expTime
  where
    expTime = 60 * 60 * 24

makeShortIdKey :: ShortId Organization -> Text
makeShortIdKey shortId = "CachedQueries:Organization:ShortId:" <> shortId.getShortId
