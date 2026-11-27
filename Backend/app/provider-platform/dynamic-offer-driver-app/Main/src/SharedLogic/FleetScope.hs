{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The one place that decides which fleets a request may touch.
--
-- The fleet endpoints used to work out "whose fleet is this?" in dozens of slightly different ways:
-- from a hierarchy setting on the merchant, from a member table, or by trusting an id in the request.
-- Each of them is now called through one of the @scoped...@ functions here, which answer in the way
-- the merchant's @fleet_scope_mode@ says:
--
--   * OFF: the old answer, untouched.
--   * SHADOW: the old answer, and any fleet the caller's assignments do not cover is logged.
--   * ENFORCE: the answer comes from the caller's authorized scope, and a fleet outside it is refused.
--
-- Only callers who are fleet bound ('fleetBound' in the scope) are held to it: the ones who reach the endpoint
-- through their fleet assignments alone, whatever their role. A caller whose city or merchant assignment grants
-- the endpoint is authorised by capability, and an operator's fleets are checked against the operator
-- association in the domain handlers.
--
-- An operator is never fleet bound: the scope skips the role, and a role invitation to a fleet changes nothing for
-- them. In ENFORCE the reads below that take an operator are held to what is linked to them instead
-- ('isEnforcedOperator').
module SharedLogic.FleetScope
  ( authorizeFleetIds,
    scopedFleetOwnerId,
    scopedFleetOwnerIds,
    scopedMbFleetOwnerAndRequestorId,
    scopedFleetOwnerAndRequestorId,
    callerOrScopedFleetOwnerId,
    passThroughFleetAndRequestor,
    enforcedSingleFleet,
    enforcedFleetOwnersInfo,
    heldFleetOwnerList,
    subjectRequestor,
    subjectActingFleet,
    isEnforced,
    reachableCities,
    assertFleetInScope,
    assertDriverInScope,
    assertSubjectInScope,
    assertRcInScope,
    assertOperatorOperatesFleet,
    restrictToScope,
    isEnforcedOperator,
    isHeldToOwnRecords,
    operatorFleetIds,
    OperatorDriverReach (..),
    operatorDriverReach,
    assertOperatorOwnsFleet,
    assertOperatorMayReadProfile,
    assertOperatorMayReadRide,
    restrictListToCaller,
    scopedLiveMapFleet,
    assertMayReadSubject,
    assertMayReadDocumentList,
    assertMayReadDocument,
    leaveOutDisabledFleets,
  )
where

import qualified "this" API.Types.ProviderPlatform.Fleet.Driver as Common
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.FleetScopeMode as DFSM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified "lib-dashboard" Domain.Types.PersonRoleAssignment as DPRA
import qualified Domain.Types.Ride as DRide
import qualified "lib-dashboard" Domain.Types.Role as DDashboardRole
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Environment
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Fleet as SFleet
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.DriverOperatorAssociationExtra as QDOAExtra
import qualified Storage.Queries.DriverRCAssociationExtra as QDRCAExtra
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDAExtra
import qualified Storage.Queries.FleetOperatorAssociationExtra as QFOAExtra
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QDashboardAccess
import qualified Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.PersonRoleAssignment as QDashAssignment
import qualified Storage.Queries.VehicleRegistrationCertificate as QVRC
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCExtra
import Tools.Auth.DashboardUser (DashboardUser, dashboardRequestorId, requestorCityAccess, requestorHasFleetMemberHierarchy, requestorIsFleetOwner)
import qualified "lib-dashboard" Tools.Auth.FleetScope as LibFleetScope

-- | Refuse, or in SHADOW log, a fleet the caller's authorized scope does not cover, and apply the
-- fleet's own status where the merchant asks for it. See 'scopedFleetOwnerId' for how it is used.
authorizeFleetIds :: DashboardUser -> [Text] -> Flow ()
authorizeFleetIds user fleetIds = do
  let mode = user.merchant.fleetScopeMode
  when (mode /= DFSM.OFF && user.scope.fleetBound) $ do
    let outside = filter (`notElem` user.scope.fleetIds) fleetIds
        detail = "personId: " <> user.personId.getId <> ", fleetIds: " <> show outside
    unless (null outside) $ case mode of
      DFSM.ENFORCE -> do
        logTagError "FLEET_SCOPE_DENIED" detail
        throwError AccessDenied
      _ -> logTagError "FLEET_SCOPE_SHADOW_DIFF" $ "the old check allowed a fleet the assignments do not cover. " <> detail
    checkFleetsVerified user mode fleetIds

-- | A fleet whose documents are not verified yet cannot be changed, in a merchant that does not
-- verify a fleet at login. This is what the person's @verified@ flag used to stand for; that flag was
-- only ever a copy of the fleet's own. Every fleet owner has an information row, so a fleet without one
-- counts as not verified. Other merchants have no such gate.
checkFleetsVerified :: DashboardUser -> DFSM.FleetScopeMode -> [Text] -> Flow ()
checkFleetsVerified user mode fleetIds =
  when (user.merchant.verifyFleetWhileLogin /= Just True) $ do
    unverified <- filterM (fmap (maybe True (not . (.verified))) . QFOI.findByPrimaryKey . Id) fleetIds
    unless (null unverified) $ case mode of
      DFSM.ENFORCE -> throwError (InvalidRequest "Fleet owner is not verified")
      _ -> logTagError "FLEET_SCOPE_SHADOW_UNVERIFIED" $ "personId: " <> user.personId.getId <> ", fleetIds: " <> show unverified

-- | In ENFORCE, the fleets a list covers, with their names: the one the request names, or all the ones the
-- caller reaches. For a fleet bound caller those are the selected fleets of their scope, and for an operator the
-- fleets linked to them. 'Nothing' when neither applies, and the old path runs.
--
-- An operator with no linked fleet gets an empty list. The list queries read no fleets as no filter, so a caller
-- of this must answer that with an empty page.
enforcedFleetOwnersInfo :: DashboardUser -> Maybe Text -> Flow (Maybe [SFleet.FleetOwnerInfo])
enforcedFleetOwnersInfo user mbRequestedFleetId
  | isEnforcedOperator user = do
    fleetIds <- case mbRequestedFleetId of
      Just fleetId -> do
        assertOperatorOwnsFleet user fleetId
        pure [fleetId]
      Nothing -> operatorFleetIds user
    -- A link left behind by a person who no longer exists is skipped, so it does not fail the whole list.
    Just . mapMaybe snd <$> fleetOwnersInfoOf fleetIds
  | not (isEnforced user) = pure Nothing
  | otherwise = do
    fleetIds <- authorizeListedFleetIds user (maybe user.scope.fleetIds (: []) mbRequestedFleetId)
    infos <- fleetOwnersInfoOf fleetIds
    Just <$> forM infos (\(fleetId, mbInfo) -> mbInfo & fromMaybeM (PersonNotFound fleetId))

-- | As 'authorizeFleetIds', for a read that covers several fleets: gives the ones it covers. A fleet that is not
-- verified yet is left out, where that is checked, so it does not hide the others. When that would leave nothing
-- they all stay, and the check refuses the read with its reason. Asking for a fleet outside the scope is refused
-- whatever its state.
authorizeListedFleetIds :: DashboardUser -> [Text] -> Flow [Text]
authorizeListedFleetIds user fleetIds = do
  listed <-
    if user.merchant.verifyFleetWhileLogin /= Just True && all (`elem` user.scope.fleetIds) fleetIds
      then do
        fleets <- QFOI.findAllByPrimaryKeys (map Id fleetIds)
        let verifiedIds = [fleet.fleetOwnerPersonId.getId | fleet <- fleets, fleet.verified]
        pure $ keepIfAny (`elem` verifiedIds) fleetIds
      else pure fleetIds
  authorizeFleetIds user listed
  pure listed

-- | Of the fleets a list covers, the enabled ones, once fleet scope is enforced and the merchant checks that fleets
-- are enabled. When none is, all are passed on, and the list's own check refuses them with its reason.
leaveOutDisabledFleets :: DashboardUser -> ShortId DM.Merchant -> (a -> Text) -> [a] -> Flow [a]
leaveOutDisabledFleets user merchantShortId fleetOf fleets
  | not (isEnforced user) = pure fleets
  | otherwise = do
    merchant <- findMerchantByShortId merchantShortId
    if merchant.fleetOwnerEnabledCheck /= Just True
      then pure fleets
      else do
        infos <- QFOI.findAllByPrimaryKeys (map (Id . fleetOf) fleets)
        let enabledIds = [info.fleetOwnerPersonId.getId | info <- infos, info.enabled]
        pure $ keepIfAny ((`elem` enabledIds) . fleetOf) fleets

-- | The entries that pass, or all of them when none does.
keepIfAny :: (a -> Bool) -> [a] -> [a]
keepIfAny passes entries = case filter passes entries of
  [] -> entries
  passing -> passing

-- | Each fleet id with its names, where its person exists. Two reads, however many fleets.
fleetOwnersInfoOf :: [Text] -> Flow [(Text, Maybe SFleet.FleetOwnerInfo)]
fleetOwnersInfoOf fleetIds = do
  -- Despite its name, this reads any person by id, from Redis as well as Postgres. 'QP.findAllByPersonIds' reads
  -- Postgres alone and so misses a fleet created a moment ago, which would fail its owner's lists.
  persons <- QP.getDriversByIdIn (map Id fleetIds)
  fleets <- QFOI.findAllByPrimaryKeys (map Id fleetIds)
  let infoOf fleetId person =
        SFleet.FleetOwnerInfo
          fleetId
          (person.firstName <> maybe "" (" " <>) person.lastName)
          (find (\fleet -> fleet.fleetOwnerPersonId.getId == fleetId) fleets >>= (.fleetName))
          fleetId
  pure [(fleetId, infoOf fleetId <$> find (\person -> person.id.getId == fleetId) persons) | fleetId <- fleetIds]

-- | The fleets the caller may choose between in the fleet picker, once fleet scope is enforced: every fleet
-- they hold by role assignment, whether or not the session has selected it. Nothing here reads the fleet
-- member association.
heldFleetOwnerList :: DashboardUser -> Flow Common.FleetOwnerListRes
heldFleetOwnerList user = do
  held <- runInDashboardDb $ QDashAssignment.findAllActiveByPersonIdAndMerchantId user.personId user.merchant.id
  ownersList <- forM (nub [a.scopeId | a <- held, a.scopeType == DPRA.FLEET]) $ \fleetId -> do
    person <- QP.findById (Id fleetId) >>= fromMaybeM (PersonNotFound fleetId)
    mbFleet <- QFOI.findByPrimaryKey (Id fleetId)
    pure
      Common.FleetOwnerListAPIEntity
        { fleetOwnerId = fleetId,
          fleetOwnerName = person.firstName <> maybe "" (" " <>) person.lastName,
          fleetName = mbFleet >>= (.fleetName),
          fleetGroup = Nothing,
          groups = [],
          order = Nothing,
          enabled = maybe False (.enabled) mbFleet
        }
  pure Common.FleetOwnerListRes {ownersList}

-- | In ENFORCE the fleet comes from the scope: the one the request names if the scope holds it, or the
-- only one the caller has. With several and none named, the caller has to choose one first.
enforcedFleetId :: DashboardUser -> Maybe Text -> Flow Text
enforcedFleetId user = \case
  Just fleetId -> pure fleetId
  Nothing -> case user.scope.fleetIds of
    [fleetId] -> pure fleetId
    [] -> throwError (InvalidRequest "No fleet to work on")
    _ -> throwError (InvalidRequest "Choose a fleet first")

isEnforced :: DashboardUser -> Bool
isEnforced user = user.merchant.fleetScopeMode == DFSM.ENFORCE && user.scope.fleetBound

-- | The cities a list is built for, passed on as the list's @cities@ parameter.
--
-- A caller who is fleet bound sees their fleets within the cities they hold: those they have dashboard
-- access to in this merchant, and the one in the URL. The request may name some of them to narrow the
-- list; naming one they do not hold is refused. Naming none means all they hold. The fleet's data in a
-- city they do not hold is left out.
--
-- Everyone else keeps the old rule: the cities the request names have to be among those they can reach,
-- and naming none means the city in the URL.
reachableCities :: DashboardUser -> Text -> Context.City -> Maybe Text -> Flow (Maybe Text)
reachableCities user merchantShortId urlCity mbCities
  | isEnforced user = do
    accesses <- runInDashboardDb $ QDashboardAccess.findByPersonIdAndMerchantId user.personId user.merchant.id
    let held = nub (urlCity : map (.operatingCity) accesses)
    unless (all (`elem` held) named) $ throwError AccessDenied
    pure . Just . T.intercalate "," . map show $ if null named then held else named
  | otherwise = do
    unless (null named) $
      requestorCityAccess user merchantShortId named >>= \case
        Just (_, []) -> pure ()
        _ -> throwError AccessDenied
    pure mbCities
  where
    named = LibFleetScope.requestedCities mbCities

-- | The checks for what a request reaches by an id of its own: a fleet owner, a driver, a certificate. In
-- ENFORCE a fleet owner may act only on what their fleets hold, whatever id the request names. The same
-- routes have always answered for any id in the merchant; admins and operators are authorised by capability
-- and are not held to a scope here.
assertFleetInScope :: DashboardUser -> Text -> Flow ()
assertFleetInScope user fleetId = when (isEnforced user) $ authorizeFleetIds user [fleetId]

-- | The fleet a driver is actively linked to. A driver who has only been invited, or whose link has lapsed,
-- belongs to no fleet here, as in the old check.
driverFleetOwnerId :: Id DP.Person -> Flow (Maybe Text)
driverFleetOwnerId driverId = fmap (.fleetOwnerId) <$> QFDAExtra.findByDriverId driverId True

-- | A driver is in scope when the fleet they are actively linked to is.
assertDriverInScope :: DashboardUser -> Id DP.Person -> Flow ()
assertDriverInScope user driverId =
  when (isEnforced user) $
    driverFleetOwnerId driverId >>= \case
      Just fleetOwnerId -> authorizeFleetIds user [fleetOwnerId]
      Nothing -> throwError AccessDenied

-- | For a route whose subject may be a driver or the fleet itself (its own documents, verification or profile):
-- once fleet scope is enforced it has to be a fleet in the caller's scope, or a driver in one.
assertSubjectInScope :: DashboardUser -> Text -> Flow ()
assertSubjectInScope user subjectId =
  unless (subjectId `elem` user.scope.fleetIds) $ assertDriverInScope user (Id subjectId)

-- | For a route that names a driver, or the fleet itself, in its path: the requestor the shared function should
-- check the subject against. Once fleet scope is enforced the subject has to be a fleet in the caller's scope
-- or a driver in one, which is checked here, and no requestor is passed on, so the fleet member association
-- is not consulted. Otherwise the old rule applies: the dashboard person is the requestor.
subjectRequestor :: DashboardUser -> Text -> Flow (Maybe Text)
subjectRequestor user subjectId
  | isEnforced user = do
    assertSubjectInScope user subjectId
    pure Nothing
  | otherwise = pure (Just (dashboardRequestorId user))

-- | For a route that reads or changes one driver, or a fleet, named by id, once fleet scope is enforced: the fleet
-- the shared function should treat as the requestor, so that its own "the requestor holds the subject" check holds.
-- A fleet in the caller's scope acts for itself; a driver acts through the fleet that holds them, which has to be
-- in the caller's scope. Anything else is refused. Only meant for an enforced caller ('isEnforced').
subjectActingFleet :: DashboardUser -> Text -> Flow Text
subjectActingFleet user subjectId
  | subjectId `elem` user.scope.fleetIds = pure subjectId
  | otherwise =
    driverFleetOwnerId (Id subjectId) >>= \case
      Just fleetOwnerId -> do
        authorizeFleetIds user [fleetOwnerId]
        pure fleetOwnerId
      Nothing -> throwError AccessDenied

-- | A certificate, named by its id or its number, is in scope when it belongs to a fleet that is. One that
-- does not exist is left for the route to report; one that belongs to no fleet is refused.
assertRcInScope :: DashboardUser -> Maybe Text -> Maybe Text -> Flow ()
assertRcInScope user mbRcId mbRcNumber =
  when (isEnforced user) $
    VRCExtra.findVehicleInfoByRcIdOrVehicleNo mbRcId mbRcNumber >>= \case
      Nothing -> pure ()
      Just rc -> maybe (throwError AccessDenied) (\fleetId -> authorizeFleetIds user [fleetId]) rc.fleetOwnerId

-- | An operator may act only on a fleet that is linked to them. Other roles are authorised by capability.
assertOperatorOperatesFleet :: DashboardUser -> Text -> Flow ()
assertOperatorOperatesFleet user fleetId =
  when (user.person.dashboardAccessType == Just DDashboardRole.DASHBOARD_OPERATOR) $
    QFOAExtra.findByFleetOwnerIdAndOperatorId (Id fleetId) (Id (dashboardRequestorId user)) True >>= \case
      Nothing -> throwError AccessDenied
      Just _ -> pure ()

-- | An operator in a merchant that enforces fleet scope. They may reach the fleets linked to them, those fleets'
-- drivers, and the drivers linked to them directly.
isEnforcedOperator :: DashboardUser -> Bool
isEnforcedOperator user =
  user.merchant.fleetScopeMode == DFSM.ENFORCE
    && user.person.dashboardAccessType == Just DDashboardRole.DASHBOARD_OPERATOR

-- | Whether a caller's reads are limited to their own fleets and drivers.
isHeldToOwnRecords :: DashboardUser -> Bool
isHeldToOwnRecords user = isEnforced user || isEnforcedOperator user

operatorFleetIds :: DashboardUser -> Flow [Text]
operatorFleetIds user = nub . map (.fleetOwnerId) <$> QFOAExtra.findAllActiveByOperatorId (dashboardRequestorId user)

operatorOperatesFleet :: DashboardUser -> Text -> Flow Bool
operatorOperatesFleet user fleetId =
  isJust <$> QFOAExtra.findByFleetOwnerIdAndOperatorId (Id fleetId) (Id (dashboardRequestorId user)) True

assertOperatorOwnsFleet :: DashboardUser -> Text -> Flow ()
assertOperatorOwnsFleet user fleetId = unlessM (operatorOperatesFleet user fleetId) $ throwError AccessDenied

operatorLinkedDirectly :: DashboardUser -> Id DP.Person -> Flow Bool
operatorLinkedDirectly user driverId =
  isJust <$> QDOAExtra.findByDriverIdAndOperatorId driverId (Id (dashboardRequestorId user)) True

-- | How an operator reaches a driver.
data OperatorDriverReach = LinkedDirectly | ThroughFleet Text

operatorDriverReach :: DashboardUser -> Id DP.Person -> Flow (Maybe OperatorDriverReach)
operatorDriverReach user driverId = do
  direct <- operatorLinkedDirectly user driverId
  if direct
    then pure (Just LinkedDirectly)
    else
      driverFleetOwnerId driverId >>= \case
        Nothing -> pure Nothing
        Just fleetId -> do
          operates <- operatorOperatesFleet user fleetId
          pure $ if operates then Just (ThroughFleet fleetId) else Nothing

-- | In ENFORCE an operator reads their own profile and those of the fleets linked to them.
assertOperatorMayReadProfile :: DashboardUser -> Text -> Flow ()
assertOperatorMayReadProfile user personId =
  when (isEnforcedOperator user && personId /= dashboardRequestorId user) $ assertOperatorOwnsFleet user personId

-- | In ENFORCE an operator opens only a ride of a fleet linked to them, or of a driver linked to them directly.
assertOperatorMayReadRide :: DashboardUser -> DRide.Ride -> Flow ()
assertOperatorMayReadRide user ride =
  when (isEnforcedOperator user) $ do
    ofOwnFleet <- maybe (pure False) (operatorOperatesFleet user . getId) ride.fleetOwnerId
    unless ofOwnFleet $ unlessM (operatorLinkedDirectly user ride.driverId) $ throwError AccessDenied

-- | 'restrictToScope' for a list an operator can also read: they keep the entries of the fleets linked to them.
restrictListToCaller :: DashboardUser -> (a -> Text) -> [a] -> Flow [a]
restrictListToCaller user fleetOf entries
  | isEnforcedOperator user = do
    fleetIds <- operatorFleetIds user
    pure $ filter ((`elem` fleetIds) . fleetOf) entries
  | otherwise = pure $ restrictToScope user fleetOf entries

-- | The fleet and requestor the live map works with. In ENFORCE an operator acts as themselves: with a fleet
-- of theirs the map shows that fleet's drivers, and with none all of the operator's.
scopedLiveMapFleet :: DashboardUser -> Maybe Text -> Flow (Maybe Text, Text)
scopedLiveMapFleet user mbRequestedFleetId
  | isEnforcedOperator user = do
    whenJust mbRequestedFleetId $ assertOperatorOwnsFleet user
    pure (mbRequestedFleetId, dashboardRequestorId user)
  | otherwise = scopedMbFleetOwnerAndRequestorId user mbRequestedFleetId

-- | Whether a caller who is held to their own records may read this person's: their own, those of a fleet of
-- theirs, or those of a driver they reach. A driver whose link is not active yet is nobody's, as on uploads.
mayReadSubject :: DashboardUser -> Text -> Flow Bool
mayReadSubject user subjectId
  | subjectId == dashboardRequestorId user = pure True
  | isEnforcedOperator user = do
    operates <- operatorOperatesFleet user subjectId
    if operates then pure True else isJust <$> operatorDriverReach user (Id subjectId)
  | subjectId `elem` user.scope.fleetIds = pure True
  | otherwise =
    driverFleetOwnerId (Id subjectId) >>= \case
      Just fleetId | fleetId `elem` user.scope.fleetIds -> do
        authorizeFleetIds user [fleetId]
        pure True
      _ -> pure False

-- | As 'mayReadSubject', for a vehicle: it belongs to a fleet of theirs, or it is linked to a person they may read.
mayReadVehicle :: DashboardUser -> Id DVRC.VehicleRegistrationCertificate -> Flow Bool
mayReadVehicle user rcId =
  QVRC.findById rcId >>= \case
    Nothing -> pure False
    Just rc -> do
      ofOwnFleet <- case rc.fleetOwnerId of
        Nothing -> pure False
        Just fleetId
          | isEnforcedOperator user -> operatorOperatesFleet user fleetId
          | otherwise -> pure (fleetId `elem` user.scope.fleetIds)
      if ofOwnFleet
        then pure True
        else do
          links <- QDRCAExtra.findAllActiveAssociationByRCId rcId
          not . null <$> filterM (mayReadSubject user . (.driverId.getId)) links

assertMayReadSubject :: DashboardUser -> Text -> Flow ()
assertMayReadSubject user subjectId =
  when (isHeldToOwnRecords user) $ unlessM (mayReadSubject user subjectId) $ throwError AccessDenied

-- | The document list takes a person and, for vehicle documents, a certificate; both have to be readable.
assertMayReadDocumentList :: DashboardUser -> Text -> Maybe Text -> Flow ()
assertMayReadDocumentList user subjectId mbRcId =
  when (isHeldToOwnRecords user) $ do
    assertMayReadSubject user subjectId
    whenJust mbRcId $ \rcId -> unlessM (mayReadVehicle user (Id rcId)) $ throwError AccessDenied

-- | One document, given who and which vehicle it is of: either of them being readable is enough. A document
-- with neither cannot be shown to be the caller's, so it is refused.
assertMayReadDocument :: DashboardUser -> Maybe Text -> Maybe Text -> Flow ()
assertMayReadDocument user mbOwnerId mbRcId =
  when (isHeldToOwnRecords user) $ do
    ofOwnPerson <- maybe (pure False) (mayReadSubject user) mbOwnerId
    unless ofOwnPerson $ do
      ofOwnVehicle <- maybe (pure False) (mayReadVehicle user . Id) mbRcId
      unless ofOwnVehicle $ throwError AccessDenied

-- | In ENFORCE, only the entries of a list that belong to a fleet in scope.
restrictToScope :: DashboardUser -> (a -> Text) -> [a] -> [a]
restrictToScope user fleetOf entries
  | isEnforced user = filter ((`elem` user.scope.fleetIds) . fleetOf) entries
  | otherwise = entries

-- | Which fleet this request is about, for the endpoints that act on one.
scopedFleetOwnerId :: DashboardUser -> Maybe Text -> Flow Text
scopedFleetOwnerId user mbRequestedFleetId = do
  fleetOwnerId <-
    if isEnforced user
      then enforcedFleetId user mbRequestedFleetId
      else SFleet.getFleetOwnerId (dashboardRequestorId user) mbRequestedFleetId
  authorizeFleetIds user [fleetOwnerId]
  pure fleetOwnerId

-- | Which fleets this request is about, for the endpoints that can span several, with their names.
scopedFleetOwnerIds :: DashboardUser -> Maybe Text -> Flow [(Text, Text)]
scopedFleetOwnerIds user mbRequestedFleetId
  | isEnforced user = do
    fleetIds <- authorizeListedFleetIds user (maybe user.scope.fleetIds pure mbRequestedFleetId)
    infos <- fleetOwnersInfoOf fleetIds
    forM infos $ \(fleetId, mbInfo) -> do
      info <- mbInfo & fromMaybeM (PersonNotFound fleetId)
      pure (fleetId, info.fleetOwnerName)
  | otherwise = do
    fleets <- SFleet.getFleetOwnerIds (dashboardRequestorId user) mbRequestedFleetId
    authorizeFleetIds user (map fst fleets)
    pure fleets

-- | The fleet, if the endpoint needs one, and the id to act as, for the endpoints that take both.
scopedMbFleetOwnerAndRequestorId :: DashboardUser -> Maybe Text -> Flow (Maybe Text, Text)
scopedMbFleetOwnerAndRequestorId user mbRequestedFleetId =
  if isEnforced user
    then do
      fleetId <- enforcedFleetId user mbRequestedFleetId
      authorizeFleetIds user [fleetId]
      pure (Just fleetId, fleetId)
    else do
      result@(mbFleetOwnerId, _) <-
        SFleet.getMbFleetOwnerAndRequestorIdMerchantBased (requestorHasFleetMemberHierarchy user) (requestorIsFleetOwner user) (dashboardRequestorId user) mbRequestedFleetId
      authorizeFleetIds user (maybeToList mbFleetOwnerId)
      pure result

-- | As 'scopedMbFleetOwnerAndRequestorId', for the endpoints where the fleet is mandatory.
scopedFleetOwnerAndRequestorId :: DashboardUser -> Maybe Text -> Flow (Text, Text)
scopedFleetOwnerAndRequestorId user mbRequestedFleetId = do
  (mbFleetOwnerId, requestorId) <- scopedMbFleetOwnerAndRequestorId user mbRequestedFleetId
  fleetOwnerId <- mbFleetOwnerId & fromMaybeM (InvalidRequest "fleetOwnerId required")
  pure (fleetOwnerId, requestorId)

-- | For a route that names no fleet and has always acted as the caller: once fleet scope is enforced the
-- fleet from the caller's scope, otherwise the caller, as before.
callerOrScopedFleetOwnerId :: DashboardUser -> Flow Text
callerOrScopedFleetOwnerId user
  | isEnforced user = scopedFleetOwnerId user Nothing
  | otherwise = pure (dashboardRequestorId user)

-- | Until fleet scope is enforced, for a route that hands the fleet the request names, if any, straight to
-- the shared function, with the caller as the requestor. Enforced callers use 'enforcedSingleFleet'.
passThroughFleetAndRequestor :: DashboardUser -> Maybe Text -> Flow (Maybe Text, Text)
passThroughFleetAndRequestor user mbRequestedFleetId = do
  authorizeFleetIds user (maybeToList mbRequestedFleetId)
  pure (mbRequestedFleetId, dashboardRequestorId user)

-- | Once fleet scope is enforced, for a route about one fleet: the one the request names, or the only one the
-- caller has, from their scope, with its name.
enforcedSingleFleet :: DashboardUser -> Maybe Text -> Flow [(Text, Text)]
enforcedSingleFleet user mbRequestedFleetId = do
  fleetId <- scopedFleetOwnerId user mbRequestedFleetId
  scopedFleetOwnerIds user (Just fleetId)
