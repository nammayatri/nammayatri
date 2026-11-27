{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The life of a fleet assignment: offered to a person, accepted by them, handed over, revoked.
-- A fleet is a scope, not a login, so who may work on it is a set of assignments on that scope.
module Domain.Action.Dashboard.FleetAssignment
  ( FleetAssignmentAPIEntity (..),
    PersonAssignmentAPIEntity (..),
    InviteToFleetReq (..),
    listFleetAssignments,
    listPersonAssignments,
    listMyFleetAssignments,
    inviteToFleet,
    acceptFleetAssignment,
    revokeFleetAssignment,
  )
where

import Data.List (nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Person as DPerson
import qualified Domain.Types.FleetScopeMode as DFSM
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonRoleAssignment as DPRA
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonRoleAssignment as QPRA
import qualified Storage.Queries.Role as QRole
import qualified Tools.Auth.Common as Auth
import Tools.Auth.Dashboard (TokenInfo (..))
import qualified Tools.Auth.FleetScope as FleetScope
import Tools.Error

data FleetAssignmentAPIEntity = FleetAssignmentAPIEntity
  { id :: Id DPRA.PersonRoleAssignment,
    personId :: Id DP.Person,
    roleId :: Id DRole.Role,
    fleetId :: Text,
    status :: DPRA.AssignmentStatus,
    grantedBy :: Maybe (Id DP.Person),
    handoverFrom :: Maybe (Id DP.Person),
    createdAt :: UTCTime,
    -- | Filled in the list of everyone on a fleet, where the ids alone say nothing to the reader.
    personName :: Maybe Text,
    roleName :: Maybe Text,
    -- | Filled on an offer in the caller's own list: who made it. The fleet's name is not kept in this database.
    grantedByName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data InviteToFleetReq = InviteToFleetReq
  { -- | The person to invite, by their dashboard e-mail.
    email :: Text,
    -- | When true, the inviter's own assignment on the fleet is revoked once the invitee accepts. This
    -- is how a fleet is handed over.
    handover :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | One of a person's role assignments, whatever its scope: a merchant, a city or a fleet.
data PersonAssignmentAPIEntity = PersonAssignmentAPIEntity
  { id :: Id DPRA.PersonRoleAssignment,
    roleId :: Id DRole.Role,
    roleName :: Text,
    scopeType :: DPRA.ScopeType,
    scopeId :: Text,
    status :: DPRA.AssignmentStatus,
    grantedBy :: Maybe (Id DP.Person),
    -- | A city or merchant role an admin granted beside the person's own, which can be revoked on its own.
    manuallyGranted :: Bool,
    handoverFrom :: Maybe (Id DP.Person),
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

toEntity :: DPRA.PersonRoleAssignment -> FleetAssignmentAPIEntity
toEntity a =
  FleetAssignmentAPIEntity
    { id = a.id,
      personId = a.personId,
      roleId = a.roleId,
      fleetId = a.scopeId,
      status = a.status,
      grantedBy = a.grantedBy,
      handoverFrom = a.handoverFrom,
      createdAt = a.createdAt,
      personName = Nothing,
      roleName = Nothing,
      grantedByName = Nothing
    }

-- | Whether the caller may manage a fleet's assignments: they hold an active assignment on it, or
-- they are an admin of the merchant, who is not tied to any one fleet.
mayManageFleet :: BeamFlow m r => TokenInfo -> Text -> m Bool
mayManageFleet tokenInfo fleetId = do
  held <- QPRA.findAllByScope DPRA.FLEET fleetId tokenInfo.merchantId
  if any (\a -> a.personId == tokenInfo.personId && a.status == DPRA.ACTIVE) held
    then pure True
    else do
      person <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonDoesNotExist tokenInfo.personId.getId)
      role <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
      pure $ DPerson.isAdminTier role.dashboardAccessType

requireMayManage :: BeamFlow m r => TokenInfo -> Text -> m ()
requireMayManage tokenInfo fleetId = do
  allowed <- mayManageFleet tokenInfo fleetId
  unless allowed $ throwError AccessDenied

-- | These persons' names, read together.
namesOf :: BeamFlow m r => [Id DP.Person] -> m [(Id DP.Person, Text)]
namesOf ids = map (\p -> (p.id, T.strip (p.firstName <> " " <> p.lastName))) <$> QP.findAllByIds (nub ids)

-- | Everyone who holds or was offered a role on the fleet.
listFleetAssignments :: BeamFlow m r => TokenInfo -> Text -> m [FleetAssignmentAPIEntity]
listFleetAssignments tokenInfo fleetId = do
  requireMayManage tokenInfo fleetId
  held <- QPRA.findAllByScope DPRA.FLEET fleetId tokenInfo.merchantId
  personNames <- namesOf (map (.personId) held)
  roles <- QRole.findAllByIds (nub (map (.roleId) held))
  let roleNames = [(r.id, r.name) | r <- roles]
  pure
    [ (toEntity a) {personName = lookup a.personId personNames, roleName = lookup a.roleId roleNames}
      | a <- held
    ]

-- | Every role a person holds or was offered in this merchant, with the scope each applies to. A person may read
-- their own; reading someone else's takes an admin-tier role.
listPersonAssignments :: BeamFlow m r => TokenInfo -> Id DP.Person -> m [PersonAssignmentAPIEntity]
listPersonAssignments tokenInfo personId = do
  unless (personId == tokenInfo.personId) $ do
    caller <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonDoesNotExist tokenInfo.personId.getId)
    callerRole <- QRole.findById caller.roleId >>= fromMaybeM (RoleDoesNotExist caller.roleId.getId)
    unless (DPerson.isAdminTier callerRole.dashboardAccessType) $ throwError AccessDenied
  held <- filter (\a -> a.merchantId == tokenInfo.merchantId) <$> QPRA.findAllByPersonId personId
  roles <- map (\r -> (r.id, r.name)) <$> QRole.findAllByIds (nub (map (.roleId) held))
  pure
    [ PersonAssignmentAPIEntity
        { id = a.id,
          roleId = a.roleId,
          roleName = fromMaybe a.roleId.getId (lookup a.roleId roles),
          scopeType = a.scopeType,
          scopeId = a.scopeId,
          status = a.status,
          grantedBy = a.grantedBy,
          manuallyGranted = a.manuallyGranted,
          handoverFrom = a.handoverFrom,
          createdAt = a.createdAt
        }
      | a <- held
    ]

-- | The offers made to the caller, and the fleets they hold, in this merchant. This is where a person
-- finds an offer to accept.
listMyFleetAssignments :: BeamFlow m r => TokenInfo -> m [FleetAssignmentAPIEntity]
listMyFleetAssignments tokenInfo = do
  mine <- QPRA.findAllByPersonId tokenInfo.personId
  let shown = [a | a <- mine, a.scopeType == DPRA.FLEET, a.merchantId == tokenInfo.merchantId, a.status /= DPRA.REVOKED]
  inviterNames <- namesOf [inviter | a <- shown, a.status == DPRA.INVITED, Just inviter <- [a.grantedBy]]
  pure
    [ (toEntity a) {grantedByName = if a.status == DPRA.INVITED then a.grantedBy >>= (`lookup` inviterNames) else Nothing}
      | a <- shown
    ]

-- | Offer a person a role on the fleet. The offer does nothing until they accept it.
inviteToFleet :: (BeamFlow m r, EncFlow m r) => TokenInfo -> Text -> InviteToFleetReq -> m FleetAssignmentAPIEntity
inviteToFleet tokenInfo fleetId req = do
  requireMayManage tokenInfo fleetId
  invitee <- QP.findByEmail req.email >>= fromMaybeM (PersonNotFound req.email)
  -- An operator reaches a fleet through the operator link, never through a role on it: fleet scope passes an
  -- operator over, so a role offered to one would give nothing it claims to.
  inviteeRole <- QRole.findById invitee.roleId >>= fromMaybeM (RoleDoesNotExist invitee.roleId.getId)
  when (inviteeRole.dashboardAccessType == DRole.DASHBOARD_OPERATOR) $
    throwError $ InvalidRequest "An operator cannot be invited to a fleet. Link the operator to the fleet instead"
  held <- QPRA.findAllByScope DPRA.FLEET fleetId tokenInfo.merchantId
  -- The invitee gets the role the fleet's admins already hold; v1 has one fleet role per fleet type. A fleet
  -- with no active assignment, one that was closed, gives the role of its latest assignment, whatever its status.
  roleId <- case [a.roleId | a <- held, a.status == DPRA.ACTIVE] <> map (.roleId) (reverse (sortOn (.createdAt) held)) of
    r : _ -> pure r
    [] -> throwError $ InvalidRequest "This fleet has never had an assignment, so there is no role to offer"
  let earlier = find (\a -> a.personId == invitee.id && a.roleId == roleId) held
  when (any (\a -> a.status `elem` [DPRA.ACTIVE, DPRA.INVITED]) earlier) $
    throwError $ InvalidRequest "This person already holds or has been offered a role on the fleet"
  -- Someone who was on the fleet before has a revoked row for this role, and there can be only one.
  assignmentId <- maybe generateGUID (pure . (.id)) earlier
  now <- getCurrentTime
  let assignment =
        DPRA.PersonRoleAssignment
          { id = assignmentId,
            personId = invitee.id,
            roleId = roleId,
            scopeType = DPRA.FLEET,
            scopeId = fleetId,
            merchantId = tokenInfo.merchantId,
            status = DPRA.INVITED,
            grantedBy = Just tokenInfo.personId,
            manuallyGranted = False,
            handoverFrom = if req.handover then Just tokenInfo.personId else Nothing,
            createdAt = now,
            updatedAt = now
          }
  if isJust earlier then QPRA.offerAgain assignment else QPRA.create assignment
  pure $ toEntity assignment

-- | Accept an offer. If it was a handover, the previous admin's assignment is revoked in the same step,
-- so the fleet is never left without an active admin.
acceptFleetAssignment :: (BeamFlow m r, Auth.AuthFlow m r, Redis.HedisFlow m r) => TokenInfo -> Id DPRA.PersonRoleAssignment -> m APISuccess
acceptFleetAssignment tokenInfo assignmentId = do
  offered <- QPRA.findById assignmentId >>= fromMaybeM (InvalidRequest "No such offer")
  unless (offered.personId == tokenInfo.personId) $ throwError AccessDenied
  withFleetAssignmentsLocked offered.merchantId offered.scopeId $ do
    -- Read again under the lock: the offer may have been revoked, or answered, since it was first read.
    assignment <- QPRA.findById assignmentId >>= fromMaybeM (InvalidRequest "No such offer")
    unless (assignment.status == DPRA.INVITED && assignment.scopeType == DPRA.FLEET) $
      throwError $ InvalidRequest "This offer is not waiting for an answer"
    person <- QP.findById assignment.personId >>= fromMaybeM (PersonDoesNotExist assignment.personId.getId)
    rolesBefore <- FleetScope.rolesHeldBy person
    QPRA.activateIfInvited assignment.id
    accepted <- QPRA.findById assignment.id
    unless (((.status) <$> accepted) == Just DPRA.ACTIVE) $
      throwError $ InvalidRequest "This offer is not waiting for an answer"
    -- Only on a merchant in ENFORCE does the new assignment give access, so only there can an open session gain any.
    mbMerchant <- QMerchant.findById assignment.merchantId
    when (((.fleetScopeMode) <$> mbMerchant) == Just DFSM.ENFORCE) $
      QRole.findById assignment.roleId >>= mapM_ (FleetScope.endSessionsOnTwoFaRoleGrant assignment.personId rolesBefore)
    whenJust assignment.handoverFrom $ \previous -> do
      held <- QPRA.findAllByScope DPRA.FLEET assignment.scopeId assignment.merchantId
      let handedOver = [a | a <- held, a.personId == previous, a.status == DPRA.ACTIVE]
      QPRA.updateStatusByIds DPRA.REVOKED (map (.id) handedOver)
      -- A handover that would leave the fleet with no admin leaves the previous one in place.
      removedAdmins <- fleetAdminsAmong handedOver
      unless (null removedAdmins) $
        void $ restoreIfNoActiveAdmin assignment.merchantId assignment.scopeId (map (.id) removedAdmins)
    pure Success

-- | Take a role away. A fleet always keeps one active admin: revoking the last one is refused, unless
-- the caller is a merchant admin, who can close a fleet's access altogether. Only an assignment holding a
-- fleet admin role counts as an admin, so a fleet with one admin and some staff still keeps its admin.
revokeFleetAssignment :: (BeamFlow m r, Redis.HedisFlow m r) => TokenInfo -> Id DPRA.PersonRoleAssignment -> m APISuccess
revokeFleetAssignment tokenInfo assignmentId = do
  target <- QPRA.findById assignmentId >>= fromMaybeM (InvalidRequest "No such assignment")
  unless (target.scopeType == DPRA.FLEET) $ throwError $ InvalidRequest "Not a fleet assignment"
  requireMayManage tokenInfo target.scopeId
  withFleetAssignmentsLocked target.merchantId target.scopeId $ do
    -- Read again under the lock, so two admins revoking each other cannot both see the other still active.
    assignment <- QPRA.findById assignmentId >>= fromMaybeM (InvalidRequest "No such assignment")
    held <- QPRA.findAllByScope DPRA.FLEET assignment.scopeId assignment.merchantId
    activeAdmins <- fleetAdminsAmong [a | a <- held, a.status == DPRA.ACTIVE]
    let isActiveAdmin = any ((== assignment.id) . (.id)) activeAdmins
        otherAdmins = filter ((/= assignment.id) . (.id)) activeAdmins
        keepOneAdmin = throwError $ InvalidRequest "A fleet must keep one active admin"
    -- Whether this removal has to leave an admin behind: it takes an admin away, and the caller is not admin tier.
    mustKeepOne <-
      if not isActiveAdmin
        then pure False
        else do
          person <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonDoesNotExist tokenInfo.personId.getId)
          role <- QRole.findById person.roleId >>= fromMaybeM (RoleDoesNotExist person.roleId.getId)
          pure $ not (DPerson.isAdminTier role.dashboardAccessType)
    when (mustKeepOne && null otherAdmins) keepOneAdmin
    QPRA.updateStatus DPRA.REVOKED assignment.id
    when mustKeepOne $ do
      restored <- restoreIfNoActiveAdmin assignment.merchantId assignment.scopeId [assignment.id]
      when restored keepOneAdmin
    pure Success

-- | The roles that make someone an admin of a fleet.
fleetAdminRoles :: [DRole.DashboardAccessType]
fleetAdminRoles = [DRole.FLEET_OWNER, DRole.RENTAL_FLEET_OWNER]

-- | Of these assignments, the ones that hold a fleet admin role.
fleetAdminsAmong :: BeamFlow m r => [DPRA.PersonRoleAssignment] -> m [DPRA.PersonRoleAssignment]
fleetAdminsAmong assignments = do
  roles <- QRole.findAllByIds (nub (map (.roleId) assignments))
  let adminRoleIds = [r.id | r <- roles, r.dashboardAccessType `elem` fleetAdminRoles]
  pure [a | a <- assignments, a.roleId `elem` adminRoleIds]

-- | Call after revoking admin assignments. If the fleet is left with no active admin, they are made active
-- again and this answers True.
--
-- A check made before the write is not enough on its own: the lock below only orders the requests one
-- service handles, and the services share this database but need not share a Redis. Looking again after
-- the write covers that. Of the changes made at the same moment, the one that looks last sees all of their
-- writes, so a fleet cannot end up with no admin. At worst each of them is refused.
restoreIfNoActiveAdmin :: BeamFlow m r => Id DMerchant.Merchant -> Text -> [Id DPRA.PersonRoleAssignment] -> m Bool
restoreIfNoActiveAdmin merchantId fleetId revoked = do
  held <- QPRA.findAllByScope DPRA.FLEET fleetId merchantId
  activeAdmins <- fleetAdminsAmong [a | a <- held, a.status == DPRA.ACTIVE]
  let noneLeft = null activeAdmins
  when noneLeft $ QPRA.updateStatusByIds DPRA.ACTIVE revoked
  pure noneLeft

-- | Changes to a fleet's assignments that one service handles run one at a time per fleet. Another service
-- takes its own lock; 'restoreIfNoActiveAdmin' and 'QPRA.activateIfInvited' cover that.
withFleetAssignmentsLocked :: (BeamFlow m r, Redis.HedisFlow m r) => Id DMerchant.Merchant -> Text -> m a -> m a
withFleetAssignmentsLocked merchantId fleetId =
  Redis.withLockRedisAndReturnValue ("dsh:fleetAssignments:" <> merchantId.getId <> ":" <> fleetId) 10
