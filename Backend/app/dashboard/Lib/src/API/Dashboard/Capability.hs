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
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common (FlowHandlerR, FlowServerR)
import Servant hiding (throwError)
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

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

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
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

getUserCapabilities :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DCap.UserCapabilitiesRes
getUserCapabilities = withDashboardDbFlowHandlerAPI . DCap.getUserCapabilities

listCapabilities :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DCap.ListCapabilitiesRes
listCapabilities = withDashboardDbFlowHandlerAPI . DCap.listCapabilities

getCapabilityEndpoints :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Text -> FlowHandlerR r DCap.CapabilityEndpointsRes
getCapabilityEndpoints tokenInfo = withDashboardDbFlowHandlerAPI . DCap.getCapabilityEndpoints tokenInfo

getRoleCapabilities :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DRole.Role -> FlowHandlerR r DCap.RoleCapabilitiesRes
getRoleCapabilities tokenInfo = withDashboardDbFlowHandlerAPI . DCap.getRoleCapabilities tokenInfo

updateRoleCapabilities :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DRole.Role -> DCap.UpdateRoleCapabilitiesReq -> FlowHandlerR r APISuccess
updateRoleCapabilities tokenInfo roleId = withDashboardDbFlowHandlerAPI . DCap.updateRoleCapabilities tokenInfo roleId

getPersonCapabilities :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> FlowHandlerR r DCap.PersonCapabilitiesRes
getPersonCapabilities tokenInfo = withDashboardDbFlowHandlerAPI . DCap.getPersonCapabilities tokenInfo

upsertPersonCapability :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DCap.UpsertPersonCapabilityReq -> FlowHandlerR r APISuccess
upsertPersonCapability tokenInfo personId = withDashboardDbFlowHandlerAPI . DCap.upsertPersonCapability tokenInfo personId

deletePersonCapability :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> Text -> FlowHandlerR r APISuccess
deletePersonCapability tokenInfo personId = withDashboardDbFlowHandlerAPI . DCap.deletePersonCapability tokenInfo personId
