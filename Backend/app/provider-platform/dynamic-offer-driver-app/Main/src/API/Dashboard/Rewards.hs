{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Rewards where
import qualified Domain.Action.Dashboard.Rewards as Domain
import Servant
import Kernel.Types.APISuccess (APISuccess)
import Environment (FlowHandler, FlowServer)
import Kernel.Utils.Common (withFlowHandlerAPI)
import Kernel.Prelude

type API =
  "driver"
    :> "rewards"
    :> ReqBody '[JSON] Domain.RewardReq
    :> Post '[JSON] APISuccess


handler :: FlowServer API
handler =
  createReward

createReward :: Domain.RewardReq -> FlowHandler APISuccess
createReward = withFlowHandlerAPI . Domain.createReward

    