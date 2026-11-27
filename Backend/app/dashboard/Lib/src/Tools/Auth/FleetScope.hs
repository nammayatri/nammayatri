{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.FleetScope
  ( AuthorizedScope (..),
    noScope,
    resolveFleetScope,
    SessionState (..),
    sessionState,
    withAssignedRoleAccess,
    wideRoleIdsHere,
    addedRoleCities,
    allowsEndpoint,
    selectFleets,
    everyRoleSkipsTwoFa,
    mayOpenByOtp,
    markOtpSession,
    isRefusedOtpSession,
    assertOtpSessionStillAllowed,
    assertOtpSessionStillAllowedByIds,
    enforcedAssignmentHoldersOf,
    rolesHeldBy,
    requestedCities,
    citiesAccessCheck,
    endSessionsOnTwoFaRoleGrant,
    toggleFleetSelection,
  )
where

import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Types.Capability as DC
import qualified Domain.Types.FleetScopeMode as DFSM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonRoleAssignment as DPRA
import qualified Domain.Types.RegistrationToken as DReg
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant (FromHttpApiData (parseUrlPiece))
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonRoleAssignment as QPRA
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Role as QRole
import qualified Tools.Auth.Capability as Capability
import qualified Tools.Auth.Common as Common
import Tools.Error (MerchantError (UserDisabled))

-- | What a request may touch, from the caller's active assignments: the cities and the fleets.
data AuthorizedScope = AuthorizedScope
  { cityIds :: [City.City],
    fleetIds :: [Text],
    -- | The caller reaches this endpoint only through their fleet assignments, so what it returns is
    -- limited to 'fleetIds'. False when a city or merchant assignment also grants the endpoint (that role
    -- covers everything in its scope, fleets included), and for callers left out of the check.
    fleetBound :: Bool,
    -- | Set when only a role held beside the person's own allows this call: the cities of this merchant that
    -- such a role covers. A request that names cities, or another merchant, is held to these
    -- ('addedRoleCities').
    limitedToCities :: Maybe [City.City]
  }
  deriving (Show, Eq)

noScope :: AuthorizedScope
noScope = AuthorizedScope {cityIds = [], fleetIds = [], fleetBound = False, limitedToCities = Nothing}

-- | Works out from the caller's active assignments whether they may call the endpoint, and which
-- cities and fleets they hold.
--
-- With the merchant switched OFF this does nothing. In SHADOW the answer is compared with the old
-- one, which allowed the call or the request would not have got this far, and a difference is only
-- logged. In ENFORCE the answer from assignments is the one that applies.
--
-- It also says whether the caller is fleet bound ('fleetBound'): the endpoint reaches them only through
-- their fleet assignments, none of their city or merchant assignments grants it. This depends on where the
-- assignment that grants the endpoint sits, never on the role's name or type, so custom roles are treated by
-- the scope they are held at. A capability granted to the person directly is not tied to any scope, so it counts
-- for every assignment they hold.
--
-- Two kinds of caller are left out. A super admin bypasses the capability check altogether. An
-- operator's fleet access is read from the operator association and never becomes an assignment.
resolveFleetScope ::
  (BeamFlow m r, Redis.HedisFlow m r) =>
  DM.Merchant ->
  City.City ->
  DP.Person ->
  Text ->
  [DC.CapabilityEndpoint] ->
  Capability.CachedAccess ->
  -- | The fleets this session has chosen ('fleetSelection').
  [Text] ->
  m AuthorizedScope
resolveFleetScope merchant city person endpointId endpoints access selection =
  case merchant.fleetScopeMode of
    DFSM.OFF -> pure noScope
    mode -> do
      mbRole <- QRole.findById person.roleId
      if access.adminTier == DC.superAdminTier || (mbRole <&> (.dashboardAccessType)) == Just DRole.DASHBOARD_OPERATOR
        then pure noScope
        else do
          assignments <- QPRA.findAllActiveByPersonIdAndMerchantId person.id merchant.id
          when (null assignments) $
            logTagWarning "FLEET_SCOPE_NO_ASSIGNMENT" $
              "personId: " <> person.id.getId <> ", endpointId: " <> endpointId
          let capabilityIds = map (.capabilityId.getId) endpoints
              grants roleId = any (`elem` capabilityIds) <$> Capability.computeEffective person.id roleId
              here = filter (appliesHere merchant city) assignments
          grantingRoles <- filterM grants (nub (map (.roleId) here))
          let grantedHere = filter ((`elem` grantingRoles) . (.roleId)) here
              viaWide = any ((/= DPRA.FLEET) . (.scopeType)) grantedHere
              viaFleet = any ((== DPRA.FLEET) . (.scopeType)) grantedHere
              allowed = viaWide || viaFleet
          unless allowed $ do
            let detail = "personId: " <> person.id.getId <> ", endpointId: " <> endpointId <> ", city: " <> show city
            case mode of
              DFSM.ENFORCE -> do
                logTagError "FLEET_SCOPE_DENIED" detail
                throwError AccessDenied
              _ -> logTagError "FLEET_SCOPE_SHADOW_DIFF" $ "the old check allowed this call and the assignments do not. " <> detail
          pure $ narrowTo selection (scopeOf (viaFleet && not viaWide) assignments)

-- | In ENFORCE, the city and merchant roles a person holds beside their own add to what they may call,
-- inside that city or merchant. A role held on a fleet adds nothing here: some APIs do not narrow what they
-- return or change by fleet, so letting such a role add them would reach past the fleet. The assignments are
-- read only when the person's own role does not already allow the call.
withAssignedRoleAccess ::
  BeamFlow m r =>
  DM.Merchant ->
  City.City ->
  DP.Person ->
  [DC.CapabilityEndpoint] ->
  Capability.CachedAccess ->
  m Capability.CachedAccess
withAssignedRoleAccess merchant city person endpoints access
  | merchant.fleetScopeMode /= DFSM.ENFORCE || access.adminTier == DC.superAdminTier || allowsEndpoint access endpoints = pure access
  | otherwise = do
    assignments <- QPRA.findAllActiveByPersonIdAndMerchantId person.id merchant.id
    case filter (/= person.roleId) (wideRoleIdsHere merchant city assignments) of
      [] -> pure access
      addedRoleIds -> do
        added <- Capability.computeEffectiveForRoleGroups person.id [addedRoleIds]
        pure
          Capability.CachedAccess
            { Capability.adminTier = access.adminTier,
              Capability.capabilities = nub (access.capabilities <> concatMap Set.toList added)
            }

-- | For a call only the added roles allow: the cities they allow it in. A role held on the merchant covers
-- every city the merchant operates in.
addedRoleCities :: BeamFlow m r => DM.Merchant -> DP.Person -> [DC.CapabilityEndpoint] -> m [City.City]
addedRoleCities merchant person endpoints = do
  assignments <- QPRA.findAllActiveByPersonIdAndMerchantId person.id merchant.id
  let added = [a | a <- assignments, a.scopeType /= DPRA.FLEET, a.roleId /= person.roleId]
      roleIds = nub (map (.roleId) added)
  capabilitiesByRole <- Capability.computeEffectiveForRoleGroups person.id [[roleId] | roleId <- roleIds]
  let needed = map (.capabilityId.getId) endpoints
      grantingRoles = [roleId | (roleId, held) <- zip roleIds capabilitiesByRole, any (`Set.member` held) needed]
      granting = filter ((`elem` grantingRoles) . (.roleId)) added
  pure $
    if any ((== DPRA.MERCHANT) . (.scopeType)) granting
      then merchant.supportedOperatingCities
      else nub $ mapMaybe (\a -> readMaybe (T.unpack a.scopeId)) granting

-- | Whether the person's own role, with their own grants and denials, allows the call.
allowsEndpoint :: Capability.CachedAccess -> [DC.CapabilityEndpoint] -> Bool
allowsEndpoint access = any ((`elem` access.capabilities) . (.capabilityId.getId))

-- | The roles held on this city or on the merchant, which cover everything inside it.
wideRoleIdsHere :: DM.Merchant -> City.City -> [DPRA.PersonRoleAssignment] -> [Id DRole.Role]
wideRoleIdsHere merchant city assignments =
  nub [a.roleId | a <- assignments, a.scopeType /= DPRA.FLEET, appliesHere merchant city a]

-- | A fleet assignment applies wherever the request is routed, because for a fleet the city only
-- routes. A city assignment applies to its own city, and a merchant assignment to its merchant.
appliesHere :: DM.Merchant -> City.City -> DPRA.PersonRoleAssignment -> Bool
appliesHere merchant city assignment = case assignment.scopeType of
  DPRA.FLEET -> True
  DPRA.CITY -> assignment.scopeId == show city
  DPRA.MERCHANT -> assignment.scopeId == merchant.id.getId

-- | What both application servers need to know about a session, kept on its row in the dashboard
-- database: they share that database, and need not share a Redis.
data SessionState = SessionState
  { openedByOtp :: Bool,
    -- | The fleets this session has chosen to work on, when it has chosen: none chosen means all of the
    -- person's fleets. The choice belongs to the session (its token), not to the person, so a second
    -- open session is not affected by it.
    fleetSelection :: [Text]
  }

sessionStateOf :: DReg.RegistrationToken -> SessionState
sessionStateOf session = SessionState {openedByOtp = session.isOtpSession, fleetSelection = fromMaybe [] session.selectedFleetIds}

-- | The state of a session that is still open. Its row is the record of that: ending a session deletes the
-- row, and disabling it marks the row. A server can still hold the token in its own cache, and need not
-- share a Redis with the server that ended it, so every path that accepts a session reads the row here.
sessionState :: BeamFlow m r => RegToken -> m SessionState
sessionState token = do
  session <- QR.findByToken token >>= fromMaybeM (InvalidToken token)
  unless session.enabled $ throwError UserDisabled
  pure $ sessionStateOf session

selectFleets :: BeamFlow m r => RegToken -> [Text] -> m ()
selectFleets token fleetIds =
  QR.updateSelectedFleetIds token (if null fleetIds then Nothing else Just (nub fleetIds))

-- | The old per-fleet on/off toggles, expressed as a change to this session's selection. Enabling with
-- @onlyCurrent@ selects just those fleets, enabling adds them, disabling takes them out. Selecting
-- every fleet held clears the selection, which means the same thing. The last fleet cannot be turned off.
toggleFleetSelection :: BeamFlow m r => RegToken -> Id DP.Person -> Id DM.Merchant -> [Text] -> Bool -> Bool -> m ()
toggleFleetSelection token personId merchantId ids enable onlyCurrent = do
  assignments <- QPRA.findAllActiveByPersonIdAndMerchantId personId merchantId
  let held = nub [a.scopeId | a <- assignments, a.scopeType == DPRA.FLEET]
  unless (all (`elem` held) ids) $ throwError AccessDenied
  current <- (.fleetSelection) <$> sessionState token
  let base = if null current then held else current
      next
        | enable && onlyCurrent = nub ids
        | enable = nub (base <> ids)
        | otherwise = filter (`notElem` ids) base
  when (null next) $ throwError (InvalidRequest "Select at least one fleet")
  selectFleets token (if all (`elem` next) held then [] else next)

narrowTo :: [Text] -> AuthorizedScope -> AuthorizedScope
narrowTo [] scope = scope
narrowTo selection scope = scope {fleetIds = filter (`elem` selection) scope.fleetIds}

-- | The rule a password login follows: one role that needs two-factor is enough for the person to need
-- it. True only when every role in 'heldRoleIds' is exempt. A role that cannot be read counts as needing
-- it.
everyRoleSkipsTwoFa :: BeamFlow m r => DP.Person -> m Bool
everyRoleSkipsTwoFa person = do
  roleIds <- heldRoleIds person
  roles <- QRole.findAllByIds roleIds
  pure $ length roles == length roleIds && all (.twoFaExempt) roles

-- | Role types a phone OTP was enough for before fleet scope.
fleetOtpLoginRoles :: [DRole.DashboardAccessType]
fleetOtpLoginRoles = [DRole.FLEET_OWNER, DRole.RENTAL_FLEET_OWNER, DRole.DASHBOARD_OPERATOR]

otpOpenableRole :: DRole.Role -> Bool
otpOpenableRole role = role.twoFaExempt || role.dashboardAccessType `elem` fleetOtpLoginRoles

-- | Whether a phone OTP alone may open, or keep open, a session on this merchant. Every ENFORCE
-- assignment role must be exempt from two-factor, and so must the person's own role. While the merchant
-- is not in ENFORCE an own role of a fleet-side type is enough without the flag, as before fleet scope,
-- so fleet owners and operators do not depend on any role data being updated first.
mayOpenByOtp :: BeamFlow m r => DM.Merchant -> DP.Person -> m Bool
mayOpenByOtp merchant person = do
  mbOwnRole <- QRole.findById person.roleId
  assignedIds <- enforcedAssignmentRoleIds person.id
  assignedRoles <- QRole.findAllByIds assignedIds
  let ownRoleOk
        | merchant.fleetScopeMode == DFSM.ENFORCE = (.twoFaExempt)
        | otherwise = otpOpenableRole
  pure $ maybe False ownRoleOk mbOwnRole && length assignedRoles == length assignedIds && all (.twoFaExempt) assignedRoles

-- | A session minted by phone OTP is remembered as one, so that it can be refused later if its holder
-- comes to hold a role that needs two-factor. Without this an OTP session opened as a fleet owner would
-- carry over into a role needing two-factor granted afterwards, which is the weakest login reaching a
-- role it could never have opened. The mark is on the session's row, so it lasts exactly as long as the
-- session does, and every server sees it.
markOtpSession :: BeamFlow m r => RegToken -> m ()
markOtpSession = QR.markOtpSession

-- | An OTP-minted session whose holder may no longer keep one ('mayOpenByOtp').
isRefusedOtpSession :: BeamFlow m r => SessionState -> DM.Merchant -> DP.Person -> m Bool
isRefusedOtpSession session merchant person =
  if session.openedByOtp then not <$> mayOpenByOtp merchant person else pure False

-- | Refuse such a session. Its holder has to log in with password and TOTP.
assertOtpSessionStillAllowed :: BeamFlow m r => SessionState -> DM.Merchant -> DP.Person -> m ()
assertOtpSessionStillAllowed session merchant person = do
  refused <- isRefusedOtpSession session merchant person
  when refused $ do
    logTagError "OTP_SESSION_REFUSED" $ "personId: " <> person.id.getId
    throwError AccessDenied

-- | 'assertOtpSessionStillAllowed' for a path that has only the ids a token gives. It also refuses a session
-- that has been ended ('sessionState'). The person and the merchant are read only for a session an OTP opened.
assertOtpSessionStillAllowedByIds :: BeamFlow m r => RegToken -> Id DP.Person -> Id DM.Merchant -> m ()
assertOtpSessionStillAllowedByIds token personId merchantId = do
  session <- sessionState token
  when session.openedByOtp $ do
    person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    assertOtpSessionStillAllowed session merchant person

-- | The cities a list request names (a comma-separated list of names or std codes, read the way the city
-- in the URL is). Empty when it names none.
requestedCities :: Maybe Text -> [City.City]
requestedCities mbNames =
  mapMaybe (either (const Nothing) Just . parseUrlPiece) (filter (not . T.null) (maybe [] (map T.strip . T.splitOn ",") mbNames))

-- | A list that names cities stays within the ones the caller has access to in this merchant.
citiesAccessCheck :: BeamFlow m r => Id DP.Person -> Id DM.Merchant -> [City.City] -> m ()
citiesAccessCheck personId merchantId cities =
  forM_ cities $ \city ->
    QAccess.findByPersonIdAndMerchantIdAndCity personId merchantId city >>= \case
      Nothing -> throwError AccessDenied
      Just _ -> pure ()

-- | The roles that decide how a person may log in ('heldRoleIds').
rolesHeldBy :: BeamFlow m r => DP.Person -> m [DRole.Role]
rolesHeldBy person = heldRoleIds person >>= QRole.findAllByIds

heldRoleIds :: BeamFlow m r => DP.Person -> m [Id DRole.Role]
heldRoleIds person = do
  assignedIds <- enforcedAssignmentRoleIds person.id
  pure $ nub (person.roleId : assignedIds)

-- | The roles of a person's active assignments on merchants in ENFORCE. On any other merchant an
-- assignment gives no access, so it must not change how anyone logs in. It goes by the assignment's own
-- merchant, not the one being logged in to, because a session can switch merchant without logging in again.
enforcedAssignmentRoleIds :: BeamFlow m r => Id DP.Person -> m [Id DRole.Role]
enforcedAssignmentRoleIds personId = do
  assignments <- QPRA.findAllByPersonId personId
  counted <- onEnforcedMerchants [a | a <- assignments, a.status == DPRA.ACTIVE]
  pure $ nub (map (.roleId) counted)

-- | Who holds a role through an active assignment that counts ('enforcedAssignmentRoleIds'): besides the
-- people whose own role it is, the ones a change to the role's two-factor exemption reaches.
enforcedAssignmentHoldersOf :: BeamFlow m r => Id DRole.Role -> m [Id DP.Person]
enforcedAssignmentHoldersOf roleId = do
  assignments <- QPRA.findAllActiveByRoleId roleId
  counted <- onEnforcedMerchants assignments
  pure $ nub (map (.personId) counted)

onEnforcedMerchants :: BeamFlow m r => [DPRA.PersonRoleAssignment] -> m [DPRA.PersonRoleAssignment]
onEnforcedMerchants assignments = do
  merchants <- QMerchant.findAllByIds (nub (map (.merchantId) assignments))
  let enforced = [m.id | m <- merchants, m.fleetScopeMode == DFSM.ENFORCE]
  pure [a | a <- assignments, a.merchantId `elem` enforced]

-- | The ticket dashboard has an OTP login of its own, which neither checks roles nor marks the session.
ticketRoleTypes :: [DRole.DashboardAccessType]
ticketRoleTypes = [DRole.TICKET_DASHBOARD_USER, DRole.TICKET_DASHBOARD_MERCHANT, DRole.TICKET_DASHBOARD_ADMIN, DRole.TICKET_DASHBOARD_APPROVER]

isTicketRole :: DRole.Role -> Bool
isTicketRole role = role.dashboardAccessType `elem` ticketRoleTypes

-- | Called when a person is given a role that needs two-factor, with the roles they held before. Every
-- session they hold is ended when one of those may have been opened by an OTP, so none of them carries
-- over into the new role; they log in again with password and TOTP. That is the case when an OTP could
-- open all the earlier roles, whether or not a session is marked as an OTP one, and when they held a
-- ticket role, whose OTP login marks nothing. Otherwise their sessions are left alone. A new ticket
-- role, or one an OTP may open, ends nothing.
endSessionsOnTwoFaRoleGrant :: (Common.AuthFlow m r, Redis.HedisFlow m r) => Id DP.Person -> [DRole.Role] -> DRole.Role -> m ()
endSessionsOnTwoFaRoleGrant personId rolesBefore newRole =
  when (needsTwoFa && mayHaveOtpSession) $ do
    Common.cleanCachedTokens personId
    QR.deleteAllByPersonId personId
  where
    needsTwoFa = not (otpOpenableRole newRole || isTicketRole newRole)
    mayHaveOtpSession = all otpOpenableRole rolesBefore || any isTicketRole rolesBefore

-- | The scope the assignments give, with whether the caller reaches the endpoint only through their fleets
-- ('fleetBound'), which the assignments alone do not say.
scopeOf :: Bool -> [DPRA.PersonRoleAssignment] -> AuthorizedScope
scopeOf bound assignments =
  AuthorizedScope
    { cityIds = nub $ mapMaybe (\a -> if a.scopeType == DPRA.CITY then readMaybe (T.unpack a.scopeId) else Nothing) assignments,
      fleetIds = nub [a.scopeId | a <- assignments, a.scopeType == DPRA.FLEET],
      fleetBound = bound,
      limitedToCities = Nothing
    }
