{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Per-endpoint authorization for dashboard routes served directly by
-- driver-app.
--
-- The dashboard's own combinator carries the endpoint identity as three
-- promoted 'UserActionType' constructors. Those types live in the dashboard's
-- API packages, which an application server cannot depend on, so here the same
-- identity is carried as a type-level string -- the exact value stored in
-- @capability_endpoint.endpoint_id@:
--
-- > DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_MANAGEMENT/GEOHASH_AREA/GET_GEOHASH_AREA_LIST"
--
-- Contrast with 'Tools.Auth.DashboardTokenAuth', which is applied per API tree,
-- compares one shared static token, and authorizes nothing.
module Tools.Auth.DashboardUserAuth
  ( DashboardUserAuth,
    VerifyDashboardUser,
    EndpointId,
    verifyDashboardUserAction,
    DashboardUser,
    -- Re-exported so the generated DashboardAuth modules can fill in
    -- session-derived path captures without importing a second module.
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
    module Reexport,
  )
where

import Data.Singletons (SingI, fromSing, sing)
import qualified Data.Text as T
import "lib-dashboard" Domain.Types.ServerName as Reexport (ServerName (..))
import qualified "lib-dashboard" Domain.Types.ServerName as DSN
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Kernel.Prelude
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import Kernel.Utils.Servant.HeaderAuth (VerificationMethod (..))
import Servant
import Tools.Auth.DashboardUser (DashboardAuthFlow, DashboardUser, FleetOwnerRegistration (..), beginFleetOwnerRegistration, checkFleetOwnerVerification, completeFleetOwnerRegistration, dashboardRequestorId, dashboardRequestorIdForDriver, dashboardRequestorName, requestorFleetFlag, requestorHasFleetMemberHierarchy, requestorIsFleetOwner, resolveRequestorTopic, updateDashboardPersonRole, updateDashboardPersonVerified)
import qualified Tools.Auth.DashboardUser as DashboardUser
import "lib-dashboard" Tools.Servant.HeaderAuth

-- | Verifies the operator's session and enforces the capability registered for
-- this endpoint. The handler receives the resolved 'DashboardUser'.
type DashboardUserAuth (serverName :: DSN.ServerName) (endpointId :: Symbol) =
  HeaderAuthWithPayload "token" VerifyDashboardUser (EndpointId serverName endpointId)

data VerifyDashboardUser

-- | Which server and which endpoint, both fixed at the type level.
data EndpointId (serverName :: DSN.ServerName) (endpointId :: Symbol)

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (DashboardUserAuth sn eid :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod VerifyDashboardUser where
  type VerificationResult VerifyDashboardUser = DashboardUser
  verificationDescription =
    "Verifies a dashboard operator's session and their capability for this endpoint."

instance VerificationMethodWithPayload VerifyDashboardUser where
  type VerificationPayloadType VerifyDashboardUser = (DSN.ServerName, Text)

instance
  forall (sn :: DSN.ServerName) (eid :: Symbol).
  (SingI sn, KnownSymbol eid) =>
  VerificationPayload (DSN.ServerName, Text) (EndpointId sn eid)
  where
  toPayloadType _ = (fromSing (sing @sn), T.pack (symbolVal (Proxy @eid)))

verifyDashboardUserAction ::
  DashboardAuthFlow m r =>
  VerificationActionWithPayload VerifyDashboardUser m
verifyDashboardUserAction =
  VerificationActionWithPayload $ \(serverName, endpointId) token ->
    DashboardUser.verifyDashboardUser serverName endpointId token
