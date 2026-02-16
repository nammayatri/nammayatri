{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Role
  ( create,
    findById,
    findByName,
    findByDashboardAccessType,
    findAllInDashboardAccessType,
    findAllByLimitOffset,
    findAllWithLimitOffset,
    findAll,
    -- findParentRolesRecursivelyCached,
    updateById,
    -- clearCache,
    -- cacheParentRolesRecursively,
    -- clearCacheById,
    -- makeParentRolesKey,
    makeRoleDescendantsKey,
    cacheRoleHierarchy,
  )
where

import qualified SharedLogic.Roles as SRoles
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
-- import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
-- import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Role as Queries

-- TODO do we need prefix for rider and provider dashboard?
-- makeParentRolesKey :: Id DRole.Role -> Text
-- makeParentRolesKey roleId = "dashboard:parentRoles:" <> roleId.getId -- roleAncestors

-- TODO do we need prefix for rider and provider dashboard?
-- | All children (down the tree), leaf has empty list
makeRoleDescendantsKey :: Id DRole.Role -> Text
makeRoleDescendantsKey roleId = "dashboard:roleDescendants:" <> roleId.getId

-- cacheParentRolesRecursively :: BeamFlow m r => DRole.Role -> m [DRole.Role]
-- cacheParentRolesRecursively role = do
--   parents <- Queries.findParentRolesRecursively role
--   logDebug $ "Cache parent roles recursively: rodeId: " <> role.id.getId <> "; roleName: " <> show role.name <> "; parents: " <> show (parents <&> (.name))
--   -- cacheParentRoles role.id parents
--   expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
--   Hedis.setExp (makeParentRolesKey role.id) parents expTime
--   pure parents


-- TODO remove if we will use calculateRoleHierarchy, because cache will be calculated for all roles at once
-- findParentRolesRecursivelyCached ::
--   BeamFlow m r =>
--   DRole.Role ->
--   m [DRole.Role]
-- findParentRolesRecursivelyCached role =
--   Hedis.safeGet (makeParentRolesKey role.id) >>= \case
--     Just parents -> pure parents
--     Nothing -> cacheParentRolesRecursively role

-- parents <- Queries.findParentRolesRecursively role
-- cacheParentRoles role.id parents
-- pure parents

-- cacheParentRoles :: CacheFlow m r => Id DRole.Role -> [DRole.Role] -> m ()
-- cacheParentRoles roleId parents = do
--   expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
--   Hedis.setExp (makeParentRolesKey roleId) parents expTime

-- clearCache :: HedisFlow m r => DRole.Role -> m ()
-- clearCache role = clearCacheById role.id

-- Call after any update that may change a role's parent chain (e.g. update role, change parentRoleId).
-- clearCacheById :: HedisFlow m r => Id DRole.Role -> m ()
-- clearCacheById roleId = Hedis.del (makeParentRolesKey roleId)

-- TODO add caching for other queries?
create :: BeamFlow m r => DRole.Role -> m ()
create = Queries.create

findById :: BeamFlow m r => Id DRole.Role -> m (Maybe DRole.Role)
findById = Queries.findById

findByName :: BeamFlow m r => Text -> m (Maybe DRole.Role)
findByName = Queries.findByName

findAll :: BeamFlow m r => m [DRole.Role]
findAll = Queries.findAll

findByDashboardAccessType :: BeamFlow m r => DRole.DashboardAccessType -> m (Maybe DRole.Role)
findByDashboardAccessType = Queries.findByDashboardAccessType

findAllInDashboardAccessType :: BeamFlow m r => [DRole.DashboardAccessType] -> m [DRole.Role]
findAllInDashboardAccessType = Queries.findAllInDashboardAccessType

findAllByLimitOffset :: BeamFlow m r => Maybe Integer -> Maybe Integer -> m [DRole.Role]
findAllByLimitOffset = Queries.findAllByLimitOffset

findAllWithLimitOffset ::
  BeamFlow m r =>
  Maybe Integer ->
  Maybe Integer ->
  Maybe Text ->
  m [DRole.Role]
findAllWithLimitOffset = Queries.findAllWithLimitOffset

updateById :: BeamFlow m r => DRole.Role -> m ()
updateById = Queries.updateById

cacheRoleHierarchy :: BeamFlow m r => [SRoles.RoleHierarchy] -> m ()
cacheRoleHierarchy rolesHierarchy = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  forM_ rolesHierarchy $ \roleHierarchy ->
    -- currently we don't store ancestors
    Hedis.setExp (makeRoleDescendantsKey roleHierarchy.role.id) roleHierarchy.roleDescendants expTime
