{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Verifying a dashboard operator's session inside driver-app, so dashboard
-- requests need not be proxied through provider-dashboard just to be
-- authorized.
--
-- The verification itself is lib-dashboard's; all this adds is routing those
-- queries at the dashboard database, which lives in a separate Postgres
-- database from atlas_driver_offer_bpp.
--
-- Contrast with 'Tools.Auth.DashboardTokenAuth', which only compares one shared
-- static token and cannot tell one operator from another.
module Tools.Auth.DashboardUser
  ( verifyDashboardUser,
    verifyDashboardSession,
    DashboardUser (..),
    DashboardAuthFlow,
    requestorFleetFlag,
    dashboardRequestorId,
    requestorHasFleetMemberHierarchy,
    requestorIsFleetOwner,
    checkFleetOwnerVerification,
    resolveRequestorTopic,
    updateDashboardPersonRole,
    dashboardRequestorIdForDriver,
    dashboardRequestorName,
    updateDashboardPersonVerified,
    FleetOwnerRegistration (..),
    beginFleetOwnerRegistration,
    completeFleetOwnerRegistration,
  )
where

import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.Merchant as DDashboardMerchant
import qualified "lib-dashboard" Domain.Types.Person as DDashboardPerson
import qualified "lib-dashboard" Domain.Types.Role as DDashboardRole
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.External.Encryption (EncryptedHashed, decrypt, encrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error (AuthError (AccessDenied), GenericError (InvalidRequest), PersonError (PersonDoesNotExist))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import "lib-dashboard" Storage.Beam.BeamFlow (BeamFlow)
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.AuditTransaction as QAudit
import qualified "lib-dashboard" Storage.Queries.Person as QDashboardPerson
import qualified "lib-dashboard" Storage.Queries.Role as QDashboardRole
import qualified "lib-dashboard" Tools.Auth.Common as DashboardCommon
import qualified "lib-dashboard" Tools.Auth.Verify as Verify
import qualified "lib-dashboard" Tools.DashboardTopic as DTopic
import qualified "lib-dashboard" Tools.Error as DashboardError

-- | What an application server needs in its environment to verify a dashboard
-- session: the dashboard token caches, the registration-token policy, and the
-- password-expiry setting. Satisfied by driver-app's AppEnv.
type DashboardAuthFlow m r =
  ( DashboardCommon.AuthFlow m r,
    Redis.HedisFlow m r,
    MonadMask m,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  )

-- | Who the caller is, once their session and capability have checked out.
data DashboardUser = DashboardUser
  { personId :: Id DDashboardPerson.Person,
    merchant :: DDashboardMerchant.Merchant,
    city :: City.City,
    person :: DDashboardPerson.Person
  }

-- | @verifyDashboardUser serverName endpointId token@ resolves the session,
-- enforces the capability registered for @endpointId@, and checks merchant and
-- city scope.
--
-- @endpointId@ is the same string the dashboard uses -- MODULE\/RESOURCE\/ACTION,
-- as stored in capability_endpoint -- which driver-app can build from the
-- generated action types it already owns, with no dependency on the dashboard's
-- API packages.
--
-- Every query inside runs against the dashboard database. The scope is kept
-- tight on purpose: anything else run inside it would resolve there too.
-- | @pathSegments@ carries the request path so Layer C (resource scoping) can
-- read the resource id out of a @{param}@ capture. Without it a route served
-- directly here would be authorized more loosely than the same route served
-- through provider-dashboard.
verifyDashboardUser :: DashboardAuthFlow m r => DSN.ServerName -> Text -> [Text] -> RegToken -> m DashboardUser
verifyDashboardUser serverName endpointId pathSegments token = do
  verified <- runInDashboardDb $ do
    v <- Verify.verifyDashboardUser serverName endpointId pathSegments token
    -- The proxy wrote one audit row per action; keep that record when the route
    -- is served here instead. Only the write belongs to the dashboard database,
    -- so it stays inside this block and not around the handler.
    --
    -- Auth runs before the request body is decoded, so unlike the proxy's row
    -- this one carries no request payload -- who, what and when, not the args.
    QAudit.writeAuditTransaction
      QAudit.AuditTransaction
        { requestorId = Just v.personId.getId,
          merchantId = Just v.merchant.id.getId,
          serverName = Just serverName,
          endpoint = endpointId,
          commonDriverId = Nothing,
          commonRideId = Nothing,
          request = Nothing,
          response = Nothing,
          responseError = Nothing
        }
    pure v
  pure
    DashboardUser
      { personId = verified.personId,
        merchant = verified.merchant,
        city = verified.city,
        person = verified.person
      }

-- | Authentication without the per-endpoint capability check.
--
-- Used on routes that still arrive through provider-dashboard, which has
-- already authorized them. It establishes who the operator is so the handler
-- and the audit trail can name them, and it is what lets driver-app accept a
-- real session alongside the legacy shared token during cutover.
--
-- Not sufficient on its own once a route is served directly: pair it with the
-- capability check ('verifyDashboardUser') before removing the proxy from that
-- route's path.
verifyDashboardSession :: DashboardAuthFlow m r => DSN.ServerName -> RegToken -> m DashboardUser
verifyDashboardSession serverName token = do
  verified <- runInDashboardDb $ Verify.verifySession serverName token
  pure
    DashboardUser
      { personId = verified.personId,
        merchant = verified.merchant,
        city = verified.city,
        person = verified.person
      }

-- | Is the calling operator a fleet owner?
--
-- Some endpoints take this, and the caller's own person id, as PATH captures on
-- their "Helper" variant -- the shape an internal caller uses. provider-dashboard
-- serves the public variant instead and fills both in from the session
-- (@Domain.Action.ProviderPlatform.RideBooking.Driver.getRequestorFleetFlag@).
-- Serving the route directly has to do the same, or the public URL gains two
-- segments that no client sends.
--
-- The role lives in the dashboard database, hence the scope switch.
requestorFleetFlag :: DashboardAuthFlow m r => DashboardUser -> m Bool
requestorFleetFlag dashboardUser = runInDashboardDb $ do
  role <-
    QDashboardRole.findById dashboardUser.person.roleId
      >>= fromMaybeM (DashboardError.RoleNotFound dashboardUser.person.roleId.getId)
  pure $
    role.dashboardAccessType == DDashboardRole.FLEET_OWNER
      || role.dashboardAccessType == DDashboardRole.RENTAL_FLEET_OWNER

-- | The calling operator's person id, as the text those endpoints expect.
--
-- Several endpoints take this as a PATH capture on their "Helper" variant --
-- named fleetOwnerId, requestorId, volunteerId or dashboardUserName depending on
-- the endpoint, but always the caller. provider-dashboard passes
-- @apiTokenInfo.personId.getId@ there; serving the route directly does the same.
dashboardRequestorId :: DashboardUser -> Text
dashboardRequestorId dashboardUser = dashboardUser.personId.getId

-- | The caller's merchant setting that decides how a fleet request resolves its
-- fleet owner. provider-dashboard branched on this before forwarding; see
-- 'SharedLogic.Fleet.getMbFleetOwnerAndRequestorIdMerchantBased'.
requestorHasFleetMemberHierarchy :: DashboardUser -> Maybe Bool
requestorHasFleetMemberHierarchy dashboardUser = dashboardUser.merchant.hasFleetMemberHierarchy

-- | Whether the caller's dashboard role is a fleet owner's. Read off the
-- session's person, the same way provider-dashboard's @DP.isFleetOwner@ did.
--
-- Unlike 'requestorFleetFlag' this needs no further query: the access type is
-- denormalised onto the person row.
requestorIsFleetOwner :: DashboardUser -> Bool
requestorIsFleetOwner dashboardUser = DDashboardPerson.isFleetOwner dashboardUser.person

-- | Refuse an unverified fleet owner.
--
-- provider-dashboard ran this guard (@Domain.Action.ProviderPlatform.CheckVerification@)
-- before forwarding the fleet write endpoints. Both the role and the verified
-- flag live in the dashboard database, so the check has to happen here rather
-- than in driver-app's own domain layer.
checkFleetOwnerVerification :: DashboardAuthFlow m r => DashboardUser -> m ()
checkFleetOwnerVerification dashboardUser = do
  isFleetOwner <- requestorFleetFlag dashboardUser
  when (isFleetOwner && dashboardUser.person.verified == Just False) $
    throwError (InvalidRequest "Fleet owner is not verified")

-- | The alert topic the calling operator subscribes to.
--
-- provider-dashboard resolved this from the session before forwarding the
-- notification routes, and passed it as a path capture the public URL does not
-- carry ('Tools.DashboardTopic.resolveTopicForPerson').
--
-- The fleet-owner ids are taken as an argument, already resolved: that lookup
-- reads the APPLICATION database while everything else here reads the
-- dashboard one, and only one scope can be active at a time.
resolveRequestorTopic :: (DashboardAuthFlow m r, BeamFlow m r) => [Text] -> DashboardUser -> m Text
resolveRequestorTopic fleetOwnerIds dashboardUser =
  runInDashboardDb $ do
    (topic, _) <- DTopic.resolveTopicForPerson (const $ pure fleetOwnerIds) dashboardUser.personId
    pure topic.getTopic

-- | Move a dashboard user to another dashboard role.
--
-- The whole of this route lived in provider-dashboard
-- (@Domain.Action.ProviderPlatform.Management.Account.putAccountUpdateRole@):
-- it reads and writes only the dashboard database and never reached an
-- application server, so serving it here means doing that work rather than
-- forwarding it. Merchant and city scope are already enforced by the
-- 'DashboardUserAuth' combinator that guards the route.
updateDashboardPersonRole :: (DashboardAuthFlow m r, BeamFlow m r) => Text -> Text -> m ()
updateDashboardPersonRole personId roleId = runInDashboardDb $ do
  person <- QDashboardPerson.findById (Id personId) >>= fromMaybeM (PersonDoesNotExist personId)
  oldRole <- QDashboardRole.findById person.roleId >>= fromMaybeM (DashboardError.RoleDoesNotExist person.roleId.getId)
  newRole <- QDashboardRole.findById (Id roleId) >>= fromMaybeM (DashboardError.RoleDoesNotExist roleId)
  when (DDashboardRole.isBppSyncRole oldRole || DDashboardRole.isBppSyncRole newRole) $
    throwError DashboardError.RoleConversionNotAllowed
  QDashboardPerson.updatePersonRole (Id personId) newRole

-- | The requestor id these driver-document endpoints expect, which is NOT
-- simply the caller.
--
-- provider-dashboard computed it as @determineRequestorId@: it is sent only
-- when the merchant has no fleet-member hierarchy AND the caller is neither an
-- admin nor the driver themselves. Sending the caller unconditionally would
-- change who the application server treats as acting on the document.
dashboardRequestorIdForDriver :: DashboardUser -> Text -> Maybe Text
dashboardRequestorIdForDriver dashboardUser driverId =
  case dashboardUser.merchant.hasFleetMemberHierarchy of
    Just False
      | not (DDashboardPerson.isAdmin dashboardUser.person || dashboardUser.personId.getId == driverId) ->
        Just dashboardUser.personId.getId
    _ -> Nothing

-- | Clear (or set) a dashboard user's verified flag.
--
-- provider-dashboard did this itself after unlinking a mandatory document; the
-- flag lives in the dashboard database, so the write has to happen here when
-- the route is served directly.
updateDashboardPersonVerified :: (DashboardAuthFlow m r, BeamFlow m r) => Text -> Bool -> m ()
updateDashboardPersonVerified personId verified =
  runInDashboardDb $ QDashboardPerson.updatePersonVerifiedStatus (Id personId) verified

-- | What the dashboard resolved before forwarding a fleet-owner registration.
data FleetOwnerRegistration = FleetOwnerRegistration
  { -- | the person the fleet is being registered for, which is the caller only
    -- when the caller is themselves a fleet owner
    fleetOwnerId :: Text,
    requestorId :: Text,
    adminApprovalRequired :: Maybe Bool,
    normalizedEmail :: Maybe Text,
    -- | whether the merchant wants the strict name validation
    strongNameCheck :: Bool
  }

-- | The checks provider-dashboard ran before forwarding a fleet-owner
-- registration (@Domain.Action.ProviderPlatform.Fleet.RegistrationV2.postRegistrationV2Register'@).
--
-- All of it reads the dashboard database: who the fleet owner is, whether the
-- caller may register for them, and whether the e-mail is already taken.
beginFleetOwnerRegistration ::
  (DashboardAuthFlow m r, BeamFlow m r, EncFlow m r) =>
  DashboardUser ->
  City.City ->
  Maybe Text ->
  Maybe Text ->
  m FleetOwnerRegistration
beginFleetOwnerRegistration dashboardUser opCity mbEmail mbPersonId = do
  let merchant = dashboardUser.merchant
      requestorId = dashboardUser.personId.getId
      mbNormalizedEmail = T.toLower <$> mbEmail
  unless (opCity `elem` merchant.supportedOperatingCities) $
    throwError (InvalidRequest "Invalid request city is not supported by Merchant")
  runInDashboardDb $ do
    fleetOwner <-
      if DDashboardPerson.isFleetOwner dashboardUser.person
        then do
          when (isJust mbPersonId && mbPersonId /= Just requestorId) $ throwError AccessDenied
          pure dashboardUser.person
        else do
          personId <- mbPersonId & fromMaybeM (InvalidRequest "personId required")
          person <- QDashboardPerson.findById (Id personId) >>= fromMaybeM (PersonDoesNotExist personId)
          unless (DDashboardPerson.isFleetOwner person) $
            throwError (InvalidRequest "Person should be fleet owner")
          pure person
    whenJust mbNormalizedEmail $ \normalizedEmail -> do
      fleetOwnerEmail <- forM fleetOwner.email decrypt
      unless (mbNormalizedEmail == (T.toLower <$> fleetOwnerEmail)) $
        unlessM (isNothing <$> QDashboardPerson.findByEmail normalizedEmail) $
          throwError (InvalidRequest "Email already registered")
    pure
      FleetOwnerRegistration
        { fleetOwnerId = fleetOwner.id.getId,
          requestorId,
          adminApprovalRequired = merchant.requireAdminApprovalForFleetOnboarding,
          normalizedEmail = mbNormalizedEmail,
          strongNameCheck = fromMaybe True merchant.isStrongNameCheckRequired
        }

-- | The dashboard-database writes provider-dashboard made after the
-- application server accepted the registration: the fleet owner's name, e-mail
-- and role are kept in step, and they are marked verified once enabled.
completeFleetOwnerRegistration ::
  (DashboardAuthFlow m r, BeamFlow m r, EncFlow m r) =>
  Text ->
  Maybe Text ->
  Text ->
  Text ->
  Bool ->
  Bool ->
  m ()
completeFleetOwnerRegistration fleetOwnerId mbNormalizedEmail firstName lastName isRentalFleet enabled = do
  encEmail :: Maybe (EncryptedHashed Text) <- forM mbNormalizedEmail encrypt
  runInDashboardDb $ do
    fleetOwner <- QDashboardPerson.findById (Id fleetOwnerId) >>= fromMaybeM (PersonDoesNotExist fleetOwnerId)
    let fleetRole = if isRentalFleet then DDashboardRole.RENTAL_FLEET_OWNER else DDashboardRole.FLEET_OWNER
    fleetOwnerRole <-
      QDashboardRole.findByDashboardAccessType fleetRole
        >>= fromMaybeM (DashboardError.RoleDoesNotExist $ show fleetRole)
    when enabled $
      unless (fleetOwner.verified == Just True) $
        QDashboardPerson.updatePersonVerifiedStatus fleetOwner.id True
    let updFleetOwner = fleetOwner {DDashboardPerson.firstName = firstName, DDashboardPerson.lastName = lastName, DDashboardPerson.email = maybe fleetOwner.email Just encEmail} :: DDashboardPerson.Person
    QDashboardPerson.updatePerson updFleetOwner.id updFleetOwner
    unless (Just fleetRole == updFleetOwner.dashboardAccessType) $
      QDashboardPerson.updatePersonRole updFleetOwner.id fleetOwnerRole

-- | The caller's display name, as the joining-OTP endpoints expect it.
--
-- Their "Helper" variant takes it as a path capture; provider-dashboard looked
-- the person up and passed @firstName <> " " <> lastName@.
dashboardRequestorName :: DashboardUser -> Text
dashboardRequestorName dashboardUser =
  dashboardUser.person.firstName <> " " <> dashboardUser.person.lastName
