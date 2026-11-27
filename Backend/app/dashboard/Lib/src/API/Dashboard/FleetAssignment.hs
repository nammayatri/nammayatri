{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.FleetAssignment where

import qualified Domain.Action.Dashboard.FleetAssignment as DFA
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonRoleAssignment as DPRA
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (FlowHandlerR, FlowServerR)
import Servant hiding (throwError)
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

-- Who may work on a fleet. Any signed-in person can call these; what they may do is decided in the
-- handlers: manage the assignments of a fleet they are an admin of, or accept an offer made to them.
type API =
  "user"
    :> ( "fleetAssignment"
           :> ( DashboardAuth 'DASHBOARD_USER
                  :> "mine"
                  :> Get '[JSON] [DFA.FleetAssignmentAPIEntity]
                  :<|> DashboardAuth 'DASHBOARD_USER
                    :> Capture "fleetId" Text
                    :> Get '[JSON] [DFA.FleetAssignmentAPIEntity]
                  :<|> DashboardAuth 'DASHBOARD_USER
                    :> Capture "fleetId" Text
                    :> "invite"
                    :> ReqBody '[JSON] DFA.InviteToFleetReq
                    :> Post '[JSON] DFA.FleetAssignmentAPIEntity
                  :<|> DashboardAuth 'DASHBOARD_USER
                    :> "accept"
                    :> Capture "assignmentId" (Id DPRA.PersonRoleAssignment)
                    :> Post '[JSON] APISuccess
                  :<|> DashboardAuth 'DASHBOARD_USER
                    :> "revoke"
                    :> Capture "assignmentId" (Id DPRA.PersonRoleAssignment)
                    :> Post '[JSON] APISuccess
              )
           :<|> "assignments"
             :> DashboardAuth 'DASHBOARD_USER
             :> Capture "personId" (Id DP.Person)
             :> Get '[JSON] [DFA.PersonAssignmentAPIEntity]
       )

handler :: DashboardLoginFlow r => FlowServerR r API
handler = (listMyFleetAssignments :<|> listFleetAssignments :<|> inviteToFleet :<|> acceptFleetAssignment :<|> revokeFleetAssignment) :<|> listPersonAssignments

listMyFleetAssignments :: DashboardLoginFlow r => TokenInfo -> FlowHandlerR r [DFA.FleetAssignmentAPIEntity]
listMyFleetAssignments = withDashboardDbFlowHandlerAPI . DFA.listMyFleetAssignments

listFleetAssignments :: DashboardLoginFlow r => TokenInfo -> Text -> FlowHandlerR r [DFA.FleetAssignmentAPIEntity]
listFleetAssignments tokenInfo = withDashboardDbFlowHandlerAPI . DFA.listFleetAssignments tokenInfo

inviteToFleet :: DashboardLoginFlow r => TokenInfo -> Text -> DFA.InviteToFleetReq -> FlowHandlerR r DFA.FleetAssignmentAPIEntity
inviteToFleet tokenInfo fleetId = withDashboardDbFlowHandlerAPI . DFA.inviteToFleet tokenInfo fleetId

acceptFleetAssignment :: DashboardLoginFlow r => TokenInfo -> Id DPRA.PersonRoleAssignment -> FlowHandlerR r APISuccess
acceptFleetAssignment tokenInfo = withDashboardDbFlowHandlerAPI . DFA.acceptFleetAssignment tokenInfo

revokeFleetAssignment :: DashboardLoginFlow r => TokenInfo -> Id DPRA.PersonRoleAssignment -> FlowHandlerR r APISuccess
revokeFleetAssignment tokenInfo = withDashboardDbFlowHandlerAPI . DFA.revokeFleetAssignment tokenInfo

listPersonAssignments :: DashboardLoginFlow r => TokenInfo -> Id DP.Person -> FlowHandlerR r [DFA.PersonAssignmentAPIEntity]
listPersonAssignments tokenInfo = withDashboardDbFlowHandlerAPI . DFA.listPersonAssignments tokenInfo
