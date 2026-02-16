{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.Account (putAccountUpdateRole) where

import qualified API.Client.RiderPlatform.Management
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.CachedQueries.Role as CQRole
import qualified "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error
  ( AuthError (AccessDenied),
    PersonError (PersonDoesNotExist),
    RoleError (RoleDoesNotExist),
  )

putAccountUpdateRole ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Person ->
  Kernel.Types.Id.Id Dashboard.Common.Role ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putAccountUpdateRole _merchantShortId _opCity apiTokenInfo personId' roleId' = do
  let personId = Kernel.Types.Id.cast personId'
      roleId = Kernel.Types.Id.cast roleId'
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  role <- CQRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)

  let requestorRoleId = apiTokenInfo.person.roleId
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
          <> "; requestorRoleId: "
          <> requestorRoleId.getId
          <> "; roleId: "
          <> roleId.getId
      throwError AccessDenied

  QP.updatePersonRole personId role
  pure Kernel.Types.APISuccess.Success
