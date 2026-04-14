module Domain.Action.RiderPlatform.Management.Account (putAccountUpdateRole) where

import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.CachedQueries.Role as CQRole
import qualified "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Api
import qualified Tools.Auth.RolesHierarchy as RolesHierarchy
import "lib-dashboard" Tools.Error
  ( PersonError (PersonDoesNotExist),
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

  RolesHierarchy.checkRoleIsDescenantOfRequestor apiTokenInfo roleId

  QP.updatePersonRole personId role
  pure Kernel.Types.APISuccess.Success
