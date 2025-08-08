{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.FleetVehicleAssignment where

import qualified Domain.Action.Internal.FleetVehicleAssignment as Domain
import Environment (FlowHandler, FlowServer)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant

type API =
  "fleet" :> (CreateVehicleAssignment :<|> UpdateVehicleAssignment)

type CreateVehicleAssignment =
  "create" :> "VehicleAssignment" :> ReqBody '[JSON] Domain.CreateFleetVehicleAssignmentReq :> Post '[JSON] APISuccess

type UpdateVehicleAssignment =
  "update" :> "VehicleAssignment" :> ReqBody '[JSON] Domain.UpdateFleetVehicleAssignmentReq :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = createVehicleAssignment :<|> updateVehicleAssignment

createVehicleAssignment :: Domain.CreateFleetVehicleAssignmentReq -> FlowHandler APISuccess
createVehicleAssignment = withFlowHandlerAPI . Domain.createVehicleAssignment

updateVehicleAssignment :: Domain.UpdateFleetVehicleAssignmentReq -> FlowHandler APISuccess
updateVehicleAssignment = withFlowHandlerAPI . Domain.updateVehicleAssignment
