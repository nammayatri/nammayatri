{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FRFSRouteTypeMapping
  ( findAllByRouteCodeAndIntegratedBppConfigId,
    clearCache,
  )
where

import Domain.Types.FRFSRouteTypeMapping
import Domain.Types.IntegratedBPPConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSRouteTypeMapping as Queries

findAllByRouteCodeAndIntegratedBppConfigId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Id IntegratedBPPConfig -> m [FRFSRouteTypeMapping]
findAllByRouteCodeAndIntegratedBppConfigId routeCode integratedBppConfigId = do
  Hedis.safeGet (routeTypeMappingCacheKey routeCode integratedBppConfigId) >>= \case
    Just mappings -> return mappings
    Nothing -> do
      mappings <- Queries.findAllByRouteCodeAndIntegratedBppConfigId routeCode integratedBppConfigId
      -- Most routes carry no type, so the empty list is cached too; the shorter negative ttl bounds
      -- how long a freshly assigned type stays invisible if an explicit clearCache is ever missed.
      expTime <- if null mappings then pure negativeCacheTtlSec else fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (routeTypeMappingCacheKey routeCode integratedBppConfigId) mappings expTime
      return mappings

clearCache :: CacheFlow m r => Text -> Id IntegratedBPPConfig -> m ()
clearCache routeCode integratedBppConfigId =
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (routeTypeMappingCacheKey routeCode integratedBppConfigId)

negativeCacheTtlSec :: Int
negativeCacheTtlSec = 300

routeTypeMappingCacheKey :: Text -> Id IntegratedBPPConfig -> Text
routeTypeMappingCacheKey routeCode integratedBppConfigId =
  "CachedQueries:FRFSRouteTypeMapping:RouteCode-" <> routeCode <> ":IntegratedBppConfigId-" <> integratedBppConfigId.getId
