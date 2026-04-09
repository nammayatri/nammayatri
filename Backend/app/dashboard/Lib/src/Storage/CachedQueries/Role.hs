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
    updateById,
    makeRoleDescendantsKey,
    cacheRoleHierarchy,
    findRoleDescendants,
  )
where

import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Roles as SRoles
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Role as Queries

-- TODO do we need prefix for rider and provider dashboard?

-- | All children (down the tree), leaf has empty list
makeRoleDescendantsKey :: Id DRole.Role -> Text
makeRoleDescendantsKey roleId = "dashboard:roleDescendants:" <> roleId.getId

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
  logInfo $ "Cache role hierarchy for roles number: " <> show (length rolesHierarchy)
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  forM_ rolesHierarchy $ \roleHierarchy ->
    -- currently we don't store ancestors
    Hedis.setExp (makeRoleDescendantsKey roleHierarchy.role.id) roleHierarchy.roleDescendants expTime
  logInfo "Cache role hierarchy finished"

findRoleDescendants :: BeamFlow m r => Id DRole.Role -> m [Id DRole.Role]
findRoleDescendants roleId = do
  -- Try to get from cache first
  mbCachedDescendants :: Maybe [Id DRole.Role] <- Hedis.safeGet (makeRoleDescendantsKey roleId)
  case mbCachedDescendants of
    Just descendants -> pure descendants
    Nothing -> do
      -- Cache miss: fetch all roles from DB and rebuild cache
      logInfo $ "Descendants cache for roleId: " <> roleId.getId <> " missed, rebuild cache"
      allRoles <- findAll
      case SRoles.calculateRoleHierarchy allRoles of
        Left (SRoles.CycleDetected cycleRoles) ->
          throwError $ InternalError $ "Cycle detected in roles hierarchy: " <> show cycleRoles
        Right rolesHierarchy -> do
          -- Cache the entire hierarchy
          cacheRoleHierarchy rolesHierarchy
          -- Find the requested role's descendants
          case find (\rh -> rh.role.id == roleId) rolesHierarchy of
            Just roleHierarchy -> pure $ map (.id) roleHierarchy.roleDescendants
            Nothing -> pure []
