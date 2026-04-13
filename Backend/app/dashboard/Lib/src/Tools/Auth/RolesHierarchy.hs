module Tools.Auth.RolesHierarchy
  ( checkRoleIsDescenantOfRequestor,
  )
where

import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.CachedQueries.Role as CQRole
import Tools.Auth.Api
import Tools.Error
  ( AuthError (AccessDenied),
    RoleError (RoleDoesNotExist),
  )

checkRoleIsDescenantOfRequestor :: BeamFlow m r => ApiTokenInfo -> Id DRole.Role -> m ()
checkRoleIsDescenantOfRequestor apiTokenInfo roleId = do
  when (apiTokenInfo.userActionType `elem` (fromMaybe [] apiTokenInfo.merchant.userActionTypesForDescendantsCheck <&> (.getUserActionType))) $ do
    let requestorRoleId = apiTokenInfo.person.roleId
    logInfo $
      "Check that role is descendant of requestor: "
        <> apiTokenInfo.person.id.getId
        <> "; userActionType: "
        <> show apiTokenInfo.userActionType
        <> "; requestorRoleId: "
        <> requestorRoleId.getId
        <> "; roleId: "
        <> roleId.getId
    requestorDashboardAccessType <- case apiTokenInfo.person.dashboardAccessType of
      Just dashboardAccessType -> pure dashboardAccessType
      Nothing -> do
        requestorRole <- CQRole.findById requestorRoleId >>= fromMaybeM (RoleDoesNotExist requestorRoleId.getId)
        pure requestorRole.dashboardAccessType

    unless (requestorDashboardAccessType == DRole.DASHBOARD_ADMIN) $ do
      requestorDescendants <- CQRole.findRoleDescendants requestorRoleId
      unless (roleId `elem` requestorDescendants) $ do
        logError $
          "Couldn't assign role which is not descendant of requestor: "
            <> apiTokenInfo.person.id.getId
            <> "; userActionType: "
            <> show apiTokenInfo.userActionType
            <> "; requestorRoleId: "
            <> requestorRoleId.getId
            <> "; roleId: "
            <> roleId.getId
        throwError AccessDenied
