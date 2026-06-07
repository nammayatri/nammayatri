{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | In-memory (L1) cached lookups for the @route@ table. Routes are static config
--   data read on hot WMB/bus paths; an in-process cache avoids the per-call KV/DB
--   round-trip. NOTE: there is no explicit invalidation, so an edited route may be
--   served stale from a pod's in-mem cache for up to 'inMemCacheTtl' seconds.
module Storage.CachedQueries.Route
  ( findByRouteCode,
    findByRouteId,
  )
where

import Domain.Types.Route (Route)
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Route as Queries

inMemCacheTtl :: Seconds
inMemCacheTtl = 3600

findByRouteCode :: (CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Route)
findByRouteCode code =
  IM.withInMemCache ["CachedQueries:Route:Code-" <> code] inMemCacheTtl $
    Queries.findByRouteCode code

findByRouteId :: (CacheFlow m r, EsqDBFlow m r) => Id Route -> m (Maybe Route)
findByRouteId routeId =
  IM.withInMemCache ["CachedQueries:Route:Id-" <> routeId.getId] inMemCacheTtl $
    Queries.findByRouteId routeId
