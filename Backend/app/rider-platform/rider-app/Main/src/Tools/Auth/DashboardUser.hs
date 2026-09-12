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
    verifyDashboardSession,
    DashboardUser (..),
    DashboardAuthFlow,
    resolveRequestorTopic,
    dashboardRequestorId,
    dashboardRequestorName,
  )
where

import qualified "lib-dashboard" Domain.Types.Merchant as DDashboardMerchant
import qualified "lib-dashboard" Domain.Types.Person as DDashboardPerson
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import "lib-dashboard" Storage.Beam.BeamFlow (BeamFlow)
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.AuditTransaction as QAudit
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
-- as stored in capability_endpoint -- which rider-app can build from the
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
-- and the audit trail can name them, and it is what lets rider-app accept a
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
