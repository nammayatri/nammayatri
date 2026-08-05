{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Capability where

import qualified Domain.Action.Dashboard.Capability as DCap
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant hiding (throwError)
import Storage.Beam.BeamFlow
import Tools.Auth

-- Access-control capability surface (dashboard unification Phase 4).
-- /user/capabilities is the contract every frontend/backend consumes for
-- capability checks; /admin/capability/* is the management surface behind
-- the Access Control UI. Gated by DashboardAuth tiers until the enforcement
-- flip; fine-grained guards (no-self-escalation, super-admin existence
-- guard) live in Domain.Action.Dashboard.Capability.

type API =
  "user"
    :> "capabilities"
    :> DashboardAuth 'DASHBOARD_USER
    :> Get '[JSON] DCap.UserCapabilitiesRes
    :<|> "admin"
      :> "capability"
      :> ( "list"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> Get '[JSON] DCap.ListCapabilitiesRes
             :<|> "endpoints"
               :> DashboardAuth 'DASHBOARD_ADMIN
               :> Capture "capabilityId" Text
               :> Get '[JSON] DCap.CapabilityEndpointsRes
             :<|> "role"
               :> DashboardAuth 'DASHBOARD_ADMIN
               :> Capture "roleId" (Id DRole.Role)
               :> Get '[JSON] DCap.RoleCapabilitiesRes
             :<|> "role"
               :> DashboardAuth 'DASHBOARD_ADMIN
               :> Capture "roleId" (Id DRole.Role)
               :> ReqBody '[JSON] DCap.UpdateRoleCapabilitiesReq
               :> Post '[JSON] APISuccess
             :<|> "person"
               :> DashboardAuth 'DASHBOARD_ADMIN
               :> Capture "personId" (Id DP.Person)
               :> Get '[JSON] DCap.PersonCapabilitiesRes
             :<|> "person"
               :> DashboardAuth 'DASHBOARD_ADMIN
               :> Capture "personId" (Id DP.Person)
               :> ReqBody '[JSON] DCap.UpsertPersonCapabilityReq
               :> Post '[JSON] APISuccess
             :<|> "person"
               :> DashboardAuth 'DASHBOARD_ADMIN
               :> Capture "personId" (Id DP.Person)
               :> "remove"
               :> Capture "capabilityId" Text
               :> Delete '[JSON] APISuccess
         )

handler :: BeamFlow' => FlowServer API
handler =
  getUserCapabilities
    :<|> ( listCapabilities
             :<|> getCapabilityEndpoints
             :<|> getRoleCapabilities
             :<|> updateRoleCapabilities
             :<|> getPersonCapabilities
             :<|> upsertPersonCapability
             :<|> deletePersonCapability
         )

getUserCapabilities :: BeamFlow' => TokenInfo -> FlowHandler DCap.UserCapabilitiesRes
getUserCapabilities = withFlowHandlerAPI' . DCap.getUserCapabilities

listCapabilities :: BeamFlow' => TokenInfo -> FlowHandler DCap.ListCapabilitiesRes
listCapabilities = withFlowHandlerAPI' . DCap.listCapabilities

getCapabilityEndpoints :: BeamFlow' => TokenInfo -> Text -> FlowHandler DCap.CapabilityEndpointsRes
getCapabilityEndpoints tokenInfo = withFlowHandlerAPI' . DCap.getCapabilityEndpoints tokenInfo

getRoleCapabilities :: BeamFlow' => TokenInfo -> Id DRole.Role -> FlowHandler DCap.RoleCapabilitiesRes
getRoleCapabilities tokenInfo = withFlowHandlerAPI' . DCap.getRoleCapabilities tokenInfo

updateRoleCapabilities :: BeamFlow' => TokenInfo -> Id DRole.Role -> DCap.UpdateRoleCapabilitiesReq -> FlowHandler APISuccess
updateRoleCapabilities tokenInfo roleId = withFlowHandlerAPI' . DCap.updateRoleCapabilities tokenInfo roleId

getPersonCapabilities :: BeamFlow' => TokenInfo -> Id DP.Person -> FlowHandler DCap.PersonCapabilitiesRes
getPersonCapabilities tokenInfo = withFlowHandlerAPI' . DCap.getPersonCapabilities tokenInfo

upsertPersonCapability :: BeamFlow' => TokenInfo -> Id DP.Person -> DCap.UpsertPersonCapabilityReq -> FlowHandler APISuccess
upsertPersonCapability tokenInfo personId = withFlowHandlerAPI' . DCap.upsertPersonCapability tokenInfo personId

deletePersonCapability :: BeamFlow' => TokenInfo -> Id DP.Person -> Text -> FlowHandler APISuccess
deletePersonCapability tokenInfo personId = withFlowHandlerAPI' . DCap.deletePersonCapability tokenInfo personId
