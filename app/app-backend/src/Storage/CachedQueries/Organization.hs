{-# LANGUAGE DerivingStrategies #-}
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
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Organization
import qualified Storage.Queries.Organization as Queries

findByShortId :: (HedisFlow m r, EsqDBFlow m r) => ShortId Organization -> m (Maybe Organization)
findByShortId shortId_ =
  Hedis.get (makeShortIdKey shortId_) >>= \case
    Just a -> return . Just $ coerce @(OrganizationD 'Unsafe) @Organization a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheOrganization /=<< Queries.findByShortId shortId_

cacheOrganization :: HedisFlow m r => Organization -> m ()
cacheOrganization org = do
  Hedis.setExp (makeShortIdKey org.shortId) (coerce @Organization @(OrganizationD 'Unsafe) org) expTime
  where
    expTime = 60 * 60 * 24

makeShortIdKey :: ShortId Organization -> Text
makeShortIdKey shortId = "CachedQueries:Organization:ShortId:" <> shortId.getShortId
