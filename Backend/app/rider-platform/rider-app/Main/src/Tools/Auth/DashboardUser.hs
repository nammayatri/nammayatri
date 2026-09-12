{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Verifying a dashboard operator's session inside rider-app, so dashboard
-- requests need not be proxied through provider-dashboard just to be
-- authorized.
--
-- The verification itself is lib-dashboard's; all this adds is routing those
-- queries at the dashboard database, which lives in a separate Postgres
-- database from atlas_app.
--
-- Contrast with 'Tools.Auth.DashboardTokenAuth', which only compares one shared
-- static token and cannot tell one operator from another.
module Tools.Auth.DashboardUser
  ( verifyDashboardUser,
    auditDashboardAction,
    DashboardUser (..),
    DashboardAuthFlow,
    resolveRequestorTopic,
    dashboardRequestorId,
    dashboardRequestorName,
    dashboardRequestorAccessType,
    assignDashboardRoleByAccessType,
    requestorCityAccess,
  )
where

import "lib-dashboard" Dashboard.Common ()
import qualified "lib-dashboard" Domain.Types.Merchant as DDashboardMerchant
import qualified "lib-dashboard" Domain.Types.Person as DDashboardPerson
import qualified "lib-dashboard" Domain.Types.Role as DDashboardRole
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error (GenericError (InvalidRequest), PersonError (PersonDoesNotExist))
import Kernel.Types.HideSecrets (HideSecrets (..))
import Kernel.Types.Id (Id (..), ShortId (..))
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import "lib-dashboard" Storage.Beam.BeamFlow (BeamFlow)
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.AuditTransaction as QAudit
import qualified "lib-dashboard" Storage.Queries.Merchant as QDashboardMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QDashboardAccess
import qualified "lib-dashboard" Storage.Queries.Person as QDashboardPerson
import qualified "lib-dashboard" Storage.Queries.Role as QDashboardRole
import "lib-dashboard" Tools.Auth.ApiAuth (dashboardApiHitsCountKey)
import qualified "lib-dashboard" Tools.Auth.Common as DashboardCommon
import qualified "lib-dashboard" Tools.Auth.Verify as Verify
import qualified "lib-dashboard" Tools.DashboardTopic as DTopic

-- | What an application server needs in its environment to verify a dashboard
-- session: the dashboard token caches, the registration-token policy, and the
-- password-expiry setting. Satisfied by rider-app's AppEnv.
type DashboardAuthFlow m r =
  ( DashboardCommon.AuthFlow m r,
    Redis.HedisFlow m r,
    MonadMask m,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int],
    HasFlowEnv m r '["dashboardApiRateLimitOptions" ::: APIRateLimitOptions]
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
-- as stored in capability_endpoint -- which rider-app can build from the
-- generated action types it already owns, with no dependency on the dashboard's
-- API packages.
--
-- Every query inside runs against the dashboard database. The scope is kept
-- tight on purpose: anything else run inside it would resolve there too.
--
-- @pathSegments@ carries the request path so Layer C (resource scoping) can
-- read the resource id out of a @{param}@ capture. Without it a route served
-- directly here would be authorized more loosely than the same route served
-- through provider-dashboard.
verifyDashboardUser :: DashboardAuthFlow m r => DSN.ServerName -> Text -> [Text] -> RegToken -> m DashboardUser
verifyDashboardUser serverName endpointId pathSegments token = do
  verified <- runInDashboardDb $ Verify.verifyDashboardUser serverName endpointId pathSegments token
  -- The same per-operator limit provider-dashboard applies in
  -- 'Tools.Auth.ApiAuth'. It lives in Redis, so it runs outside the dashboard
  -- database scope. A rejected call never reaches the handler's audit write.
  rateLimitOptions <- asks (.dashboardApiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (dashboardApiHitsCountKey verified.personId) rateLimitOptions
  pure
    DashboardUser
      { personId = verified.personId,
        merchant = verified.merchant,
        city = verified.city,
        person = verified.person
      }

-- | Record a direct-dashboard action in the shared audit trail, with the request
-- body when there is one.
--
-- Called by the generated @API.Action.DashboardAuth@ handler of every non-GET
-- endpoint once Servant has decoded the body. That is why it is not part of
-- 'verifyDashboardUser': auth runs before the body is read. The body goes
-- through 'hideSecrets', exactly as provider-dashboard stores it.
auditDashboardAction ::
  (DashboardAuthFlow m r, HideSecrets req) =>
  DSN.ServerName ->
  Text ->
  DashboardUser ->
  Maybe req ->
  m ()
auditDashboardAction serverName endpointId dashboardUser mbReq =
  runInDashboardDb $
    QAudit.writeAuditTransaction
      QAudit.AuditTransaction
        { requestorId = Just dashboardUser.personId.getId,
          merchantId = Just dashboardUser.merchant.id.getId,
          serverName = Just serverName,
          endpoint = endpointId,
          commonDriverId = Nothing,
          commonRideId = Nothing,
          request = encodeToText . hideSecrets <$> mbReq,
          response = Nothing,
          responseError = Nothing
        }

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

-- | The calling operator's person id, as the text those endpoints expect.
--
-- Several endpoints take this as a PATH capture on their "Helper" variant --
-- always the caller. rider-dashboard passes @apiTokenInfo.personId.getId@
-- there; serving the route directly does the same.
dashboardRequestorId :: DashboardUser -> Text
dashboardRequestorId dashboardUser = dashboardUser.personId.getId

-- | The caller's display name, as the block/unblock endpoints expect it.
--
-- Their "Helper" variant takes it as a path capture; rider-dashboard looked the
-- person up and passed @firstName <> " " <> lastName@. Mirrors driver-app's
-- accessor of the same name.
dashboardRequestorName :: DashboardUser -> Text
dashboardRequestorName dashboardUser =
  dashboardUser.person.firstName <> " " <> dashboardUser.person.lastName

-- | The caller's dashboard role.
--
-- rider-dashboard resolved this from the session before forwarding -- the
-- ticket-dashboard onboarding routes as @getDashboardAccessType@, the refund
-- route to decide auto-approval -- and the application server trusts what it is
-- given. Serving those routes directly has to resolve it here rather than take
-- the client's word for it.
--
-- The role lives in the dashboard database, hence the scope switch.
dashboardRequestorAccessType :: DashboardAuthFlow m r => DashboardUser -> m DDashboardRole.DashboardAccessType
dashboardRequestorAccessType dashboardUser = runInDashboardDb $ do
  role <-
    QDashboardRole.findById dashboardUser.person.roleId
      >>= fromMaybeM (InvalidRequest "Role is not assigned for this user")
  pure role.dashboardAccessType

-- | Move a dashboard user onto the role that carries an access type.
--
-- rider-dashboard did this itself after an onboarding step was approved
-- (@dashboardSideHandler@); the role lives in the dashboard database, so the
-- write has to happen here when the route is served directly.
assignDashboardRoleByAccessType :: DashboardAuthFlow m r => Text -> DDashboardRole.DashboardAccessType -> m ()
assignDashboardRoleByAccessType personId accessType = runInDashboardDb $ do
  person <- QDashboardPerson.findById (Id personId) >>= fromMaybeM (PersonDoesNotExist personId)
  role <-
    QDashboardRole.findByDashboardAccessType accessType
      >>= fromMaybeM (InvalidRequest $ "No role for access type " <> show accessType)
  QDashboardPerson.updatePersonRole person.id role

-- | Which of @cities@ the caller may act on for the merchant with this short
-- id, as @merchant_access@ records it. 'Nothing' when no such merchant exists.
--
-- The dashboards checked this per merchant and city before forwarding a bulk
-- rollout; the application server has no view of either table, so a route
-- served directly has to check it here.
requestorCityAccess :: DashboardAuthFlow m r => DashboardUser -> Text -> [City.City] -> m (Maybe ([City.City], [City.City]))
requestorCityAccess dashboardUser merchantShortId cities = runInDashboardDb $ do
  mbMerchant <- QDashboardMerchant.findByShortId (ShortId merchantShortId)
  forM mbMerchant $ \merchant -> do
    allowed <-
      filterM
        (fmap isJust . QDashboardAccess.findByPersonIdAndMerchantIdAndCity dashboardUser.personId merchant.id)
        cities
    pure (allowed, filter (`notElem` allowed) cities)
