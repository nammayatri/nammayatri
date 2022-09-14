module Tools.Roles where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.AccessMatrix as DMatrix
import Domain.Types.Person as DP
import Domain.Types.Role as DRole
import qualified Storage.Queries.AccessMatrix as QAccessMatrix
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Role as QRole

-- TODO make tests for this logic
verifyAccessLevel :: EsqDBFlow m r => RequiredAccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevel requiredAccessLevel personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  case requiredAccessLevel of
    RequiredApiAccessLevel apiAccessLevel -> do
      mbAccessMatrixItem <- QAccessMatrix.findByRoleAndEntity person.roleId apiAccessLevel.apiEntity
      let userAccessType = maybe DMatrix.USER_NO_ACCESS (.userAccessType) mbAccessMatrixItem
      unless (checkUserAccess userAccessType apiAccessLevel.apiAccessType) $
        throwError AccessDenied
      pure person.id
    RequiredDashboardAccessLevel DRole.DASHBOARD_ADMIN -> do
      role <- QRole.findById person.roleId >>= fromMaybeM (PersonDoesNotExist personId.getId) -- FIXME >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
      if role.dashboardAccessType == DRole.DASHBOARD_ADMIN
        then pure person.id
        else throwError AccessDenied
    RequiredDashboardAccessLevel DRole.DASHBOARD_USER ->
      pure person.id

checkUserAccess :: UserAccessType -> ApiAccessType -> Bool
checkUserAccess USER_FULL_ACCESS _ = True
checkUserAccess USER_READ_ACCESS READ_ACCESS = True
checkUserAccess USER_WRITE_ACCESS WRITE_ACCESS = True
checkUserAccess _ _ = False
