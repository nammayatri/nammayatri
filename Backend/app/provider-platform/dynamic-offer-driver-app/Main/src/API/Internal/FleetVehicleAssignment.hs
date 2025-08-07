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
import Kernel.Utils.Error.Throwing (throwError)
import Servant hiding (throwError)
import Tools.Error

type API =
  "fleet-vehicle-assignment"
    :> Header "token" Text
    :> ( "assign"
           :> ReqBody '[JSON] Domain.VehicleAssignmentReq
           :> Post '[JSON] Domain.VehicleAssignmentResp
       )

handler :: FlowServer API
handler = assignVehicleHandler

assignVehicleHandler :: Maybe Text -> Domain.VehicleAssignmentReq -> FlowHandler Domain.VehicleAssignmentResp
assignVehicleHandler apiKey req = withFlowHandlerAPI $ do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid internal API key"
  Domain.assignVehicle req
