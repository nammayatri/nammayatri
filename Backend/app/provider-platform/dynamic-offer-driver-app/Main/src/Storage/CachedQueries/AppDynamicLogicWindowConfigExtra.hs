{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.AppDynamicLogicWindowConfigExtra
  ( create,
    findByDomainAndVersion,
    findAllByDomain,
    clearCacheByDomain,
  )
where

import Domain.Types.AppDynamicLogicWindowConfig
import Kernel.Beam.Functions (findOneWithDb)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (CacheFlow, MonadFlow, throwError)
import qualified Lib.Yudhishthira.Types as LYT
import qualified Sequelize as Se
import qualified Storage.Beam.AppDynamicLogicWindowConfig as Beam
import qualified Storage.Queries.AppDynamicLogicWindowConfig as Queries

-- | Rows are written once per version and never updated, so the only writer is 'create',
-- which clears the domain's cache.
--
-- The table is in kv_configs.disableForKV (ddl migration 0931), so the write goes straight
-- to Postgres instead of Redis + drainer: a lost drain would otherwise leave the version
-- EVENTS only until the Redis copy expires, then silently TIME. The row is read back from
-- Postgres, so a failed write (or an environment without that migration) fails the save.
create :: (MonadFlow m, Esq.EsqDBFlow m r, CacheFlow m r) => AppDynamicLogicWindowConfig -> m ()
create row = do
  Queries.create row
  clearCacheByDomain row.domain
  stored :: Maybe AppDynamicLogicWindowConfig <- findOneWithDb [Se.And [Se.Is Beam.domain $ Se.Eq row.domain, Se.Is Beam.version $ Se.Eq row.version]]
  when (isNothing stored) $
    throwError $
      InternalError $
        "The events window of " <> show row.domain <> " version " <> show row.version
          <> " was written but isn't in Postgres yet, so it would be lost if Redis dropped it. Check that app_dynamic_logic_window_config is in kv_configs.disableForKV (ddl migration 0931)."

-- | Nothing = the version is TIME. The whole domain is cached as one list, empty included,
-- so TIME versions (no row) are answered from the cache instead of the database.
findByDomainAndVersion :: (CacheFlow m r, Esq.EsqDBFlow m r) => LYT.LogicDomain -> Int -> m (Maybe AppDynamicLogicWindowConfig)
findByDomainAndVersion domain version =
  find (\row -> row.version == version) <$> findAllByDomain domain

findAllByDomain :: (CacheFlow m r, Esq.EsqDBFlow m r) => LYT.LogicDomain -> m [AppDynamicLogicWindowConfig]
findAllByDomain domain =
  Hedis.safeGet (makeDomainKey domain) >>= \case
    Just rows -> pure rows
    Nothing -> cacheByDomain domain /=<< Queries.findAllByDomain domain

cacheByDomain :: (CacheFlow m r) => LYT.LogicDomain -> [AppDynamicLogicWindowConfig] -> m ()
cacheByDomain domain rows = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeDomainKey domain) rows expTime

-- Deletes run on both clouds' Redis (primary + secondary): reads may be served cross-cloud,
-- so a single-cloud delete could leave a stale list behind.
clearCacheByDomain :: Hedis.HedisFlow m r => LYT.LogicDomain -> m ()
clearCacheByDomain domain =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.del (makeDomainKey domain)

makeDomainKey :: LYT.LogicDomain -> Text
makeDomainKey domain = "driver-offer:CachedQueries:AppDynamicLogicWindowConfig:Domain-" <> show domain
