module Domain.Action.ProviderPlatform.CheckVerification where

import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import qualified "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest), PersonError (PersonDoesNotExist))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM, throwError)
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.CachedQueries.Role as CQRole
import qualified "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Tools.Error (RoleError (RoleNotFound))

checkFleetOwnerVerification :: ID.Id DP.Person -> Environment.Flow ()
checkFleetOwnerVerification personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  role <- CQRole.findById person.roleId >>= fromMaybeM (RoleNotFound person.roleId.getId)
  let isRoleInLs = role.dashboardAccessType `elem` [DRole.RENTAL_FLEET_OWNER, DRole.FLEET_OWNER]
  when (isRoleInLs && person.verified == Just False) $
    throwError (InvalidRequest "Fleet owner is not verified")
