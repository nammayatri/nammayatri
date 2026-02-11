{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Role
  ( create,
    findById,
    findByName,
    findByDashboardAccessType,
    findAllInDashboardAccessType,
    findAllByLimitOffset,
    findAllWithLimitOffset,
    findParentRolesRecursivelyCached,
    -- clearCache,
    clearCacheById,
    makeParentRolesKey,
  )
where

import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Role as Queries

findAllWithLimitOffset ::
  BeamFlow m r =>
  Maybe Integer ->
  Maybe Integer ->
  Maybe Text ->
  m [DRole.Role]
findAllWithLimitOffset = Queries.findAllWithLimitOffset

-- TODO do we need prefix for rider and provider dashboard?
makeParentRolesKey :: Id DRole.Role -> Text
makeParentRolesKey roleId = "dashboard:parentRoles:" <> roleId.getId

findParentRolesRecursivelyCached ::
  BeamFlow m r =>
  DRole.Role ->
  m [DRole.Role]
findParentRolesRecursivelyCached role =
  Hedis.safeGet (makeParentRolesKey role.id) >>= \case
    Just parents -> pure parents
    Nothing -> do
      parents <- Queries.findParentRolesRecursively role
      cacheParentRoles role.id parents
      pure parents

cacheParentRoles :: CacheFlow m r => Id DRole.Role -> [DRole.Role] -> m ()
cacheParentRoles roleId parents = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeParentRolesKey roleId) parents expTime

-- clearCache :: HedisFlow m r => DRole.Role -> m ()
-- clearCache role = clearCacheById role.id

-- Call after any update that may change a role's parent chain (e.g. update role, change parentRoleId).
clearCacheById :: HedisFlow m r => Id DRole.Role -> m ()
clearCacheById roleId = Hedis.del (makeParentRolesKey roleId)

-- TODO add caching for other queries?
create :: BeamFlow m r => DRole.Role -> m ()
create = Queries.create

findById :: BeamFlow m r => Id DRole.Role -> m (Maybe DRole.Role)
findById = Queries.findById

findByName :: BeamFlow m r => Text -> m (Maybe DRole.Role)
findByName = Queries.findByName

findByDashboardAccessType :: BeamFlow m r => DRole.DashboardAccessType -> m (Maybe DRole.Role)
findByDashboardAccessType = Queries.findByDashboardAccessType

findAllInDashboardAccessType :: BeamFlow m r => [DRole.DashboardAccessType] -> m [DRole.Role]
findAllInDashboardAccessType = Queries.findAllInDashboardAccessType

findAllByLimitOffset :: BeamFlow m r => Maybe Integer -> Maybe Integer -> m [DRole.Role]
findAllByLimitOffset = Queries.findAllByLimitOffset
