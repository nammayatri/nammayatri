{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.GetPickupInstructions
  ( API,
    handler,
  )
where

import Domain.Action.Internal.GetPickupInstructions (PickupInstructionResp)
import qualified Domain.Action.Internal.GetPickupInstructions as Domain
import qualified Domain.Types.Ride
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Capture "rideId" (Id Domain.Types.Ride.BPPRide)
    :> "pickupInstructions"
    :> Header "token" Text
    :> Get '[JSON] PickupInstructionResp

handler :: FlowServer API
handler = getPickupInstructionsHandler

getPickupInstructionsHandler :: Id Domain.Types.Ride.BPPRide -> Maybe Text -> FlowHandler PickupInstructionResp
getPickupInstructionsHandler bppRideId apiKey =
  withFlowHandlerAPI $ Domain.getPickupInstructions bppRideId apiKey
