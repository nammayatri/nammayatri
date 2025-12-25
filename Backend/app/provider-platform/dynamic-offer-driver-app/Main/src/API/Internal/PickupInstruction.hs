{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.PickupInstruction
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.PickupInstruction as Domain
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Error
import Servant

type API =
  Capture "driverId" Text
    :> "pickupInstruction"
    :> Header "token" Text
    :> ReqBody '[JSON] Domain.PickupInstructionReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = sendPickupInstructionHandler

sendPickupInstructionHandler :: Text -> Maybe Text -> Domain.PickupInstructionReq -> FlowHandler APISuccess
sendPickupInstructionHandler driverId apiKey req =
  withFlowHandlerAPI $ Domain.sendPickupInstruction driverId apiKey req
