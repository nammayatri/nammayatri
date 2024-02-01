{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.WhiteListOrg
  ( findBySubscriberId,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.WhiteListOrg
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber)
import Kernel.Utils.Common
import qualified Storage.Queries.WhiteListOrg as Queries

findBySubscriberId :: (CacheFlow m r, EsqDBFlow m r) => ShortId Subscriber -> m (Maybe WhiteListOrg)
findBySubscriberId subscriberId =
  Hedis.safeGet (makeShortIdKey subscriberId) >>= \case
    Just a -> return . Just $ coerce @(WhiteListOrgD 'Unsafe) @WhiteListOrg a
    Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheOrganization /=<< Queries.findBySubscriberId subscriberId

cacheOrganization :: (CacheFlow m r) => WhiteListOrg -> m ()
cacheOrganization org = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeShortIdKey org.subscriberId) (coerce @WhiteListOrg @(WhiteListOrgD 'Unsafe) org) expTime

makeShortIdKey :: ShortId Subscriber -> Text
makeShortIdKey subscriberId = "CachedQueries:WhiteListOrg:SubscriberId-" <> subscriberId.getShortId
