{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Keeps @person_role_assignment@ in step with the places that change a person's role or city
-- access. The one-off backfill only copies what exists on the day it runs; without this, a city
-- removed afterwards would leave its assignment behind, and a person created afterwards would have
-- none.
--
-- Every writer calls 'syncPersonAssignments' after it writes. The sync works from the person's
-- current role and city access, not from what the writer did, so it gives the same rows whichever
-- writer called it and however often it is called.
module Domain.Action.Dashboard.AssignmentSync
  ( syncPersonAssignments,
    removePersonAssignments,
    revokeAllOnFleet,
  )
where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonRoleAssignment as DPRA
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonRoleAssignment as QPRA
import qualified Storage.Queries.Role as QRole

data Wanted = Wanted
  { merchantId :: Id DMerchant.Merchant,
    scopeType :: DPRA.ScopeType,
    scopeId :: Text
  }

-- | Bring the person's city assignments and their own-fleet assignment in line with their current
-- role and city access: create what is missing, bring back what was revoked and is wanted again, and
-- revoke what is no longer wanted. Assignments on other people's fleets are not touched here.
--
-- A city or merchant role an admin granted by hand ('manuallyGranted') does not follow the person's own
-- role. It stays until it is revoked, or the person can no longer reach its city or merchant.
--
-- A fleet owner's own fleet is an assignment on a fleet, and their city access only routes requests.
-- An operator's fleet access is read from the operator association, so operators get no assignment.
-- Everyone else holds their role on each city they can reach.
syncPersonAssignments :: BeamFlow m r => Id DP.Person -> m ()
syncPersonAssignments personId =
  QP.findById personId >>= \case
    Nothing -> pure ()
    Just person -> do
      mbRole <- QRole.findById person.roleId
      access <- QAccess.findAllMerchantAccessByPersonId personId
      let accessType = (.dashboardAccessType) <$> mbRole
          isFleetOwner = accessType `elem` [Just DRole.FLEET_OWNER, Just DRole.RENTAL_FLEET_OWNER]
          isOperator = accessType == Just DRole.DASHBOARD_OPERATOR
          wanted
            | isFleetOwner = [Wanted m DPRA.FLEET personId.getId | Just m <- [person.merchantId]]
            | isOperator || isNothing mbRole = []
            | otherwise = [Wanted a.merchantId DPRA.CITY (show a.operatingCity) | a <- access]
      cityHeld <- QPRA.findAllByPersonIdAndScopeType personId DPRA.CITY
      fleetHeld <- QPRA.findAllByPersonIdAndScopeType personId DPRA.FLEET
      merchantHeld <- QPRA.findAllByPersonIdAndScopeType personId DPRA.MERCHANT
      -- Only the person's own fleet is managed here; assignments on other fleets belong to the fleet flows.
      let held = cityHeld <> filter (\a -> a.scopeId == personId.getId) fleetHeld
          sameScope a w = a.merchantId == w.merchantId && a.scopeType == w.scopeType && a.scopeId == w.scopeId
          isWanted a = a.roleId == person.roleId && any (sameScope a) wanted
          canReach a = case a.scopeType of
            DPRA.CITY -> any (\m -> m.merchantId == a.merchantId && show m.operatingCity == a.scopeId) access
            DPRA.MERCHANT -> any (\m -> m.merchantId == a.merchantId) access
            DPRA.FLEET -> True
          -- What the sync manages goes when it is no longer wanted; what was granted by hand goes when its
          -- city or merchant is out of the person's reach.
          noLongerWanted = [a | a <- held, not a.manuallyGranted, not (isWanted a)]
          outOfReach = [a | a <- cityHeld <> merchantHeld, a.manuallyGranted, not (canReach a)]
      QPRA.reactivateAsSynced [a.id | a <- held, isWanted a, a.status /= DPRA.ACTIVE]
      QPRA.updateStatusByIds DPRA.REVOKED [a.id | a <- noLongerWanted <> outOfReach, a.status /= DPRA.REVOKED]
      forM_ wanted $ \w ->
        unless (any (\a -> a.roleId == person.roleId && sameScope a w) held) $ do
          assignmentId <- generateGUID
          now <- getCurrentTime
          QPRA.create
            DPRA.PersonRoleAssignment
              { id = assignmentId,
                personId = personId,
                roleId = person.roleId,
                scopeType = w.scopeType,
                scopeId = w.scopeId,
                merchantId = w.merchantId,
                status = DPRA.ACTIVE,
                grantedBy = Nothing,
                manuallyGranted = False,
                handoverFrom = Nothing,
                createdAt = now,
                updatedAt = now
              }

-- | Remove every assignment of a person who is being deleted. The table references person, so this
-- has to run before the person row goes.
removePersonAssignments :: BeamFlow m r => Id DP.Person -> m ()
removePersonAssignments = QPRA.deleteAllByPersonId

-- | Close a fleet's access: every active or pending assignment on it is revoked, and the fleet owner
-- and everyone else keep their accounts. This is what rejecting a fleet does; it used to delete the
-- owner's dashboard account. The merchant comes from the caller, since a fleet created as a scope has no
-- dashboard person to read it from.
revokeAllOnFleet :: BeamFlow m r => Id DMerchant.Merchant -> Id DP.Person -> m ()
revokeAllOnFleet merchantId fleetOwnerId = do
  held <- QPRA.findAllByScope DPRA.FLEET fleetOwnerId.getId merchantId
  QPRA.updateStatusByIds DPRA.REVOKED [a.id | a <- held, a.status /= DPRA.REVOKED]
