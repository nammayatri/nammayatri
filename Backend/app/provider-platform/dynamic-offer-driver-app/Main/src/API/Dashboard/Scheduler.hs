{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Scheduler where

import qualified Domain.Action.Dashboard.Scheduler as DScheduler
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)

data SchedulerEndpoint
  = ListSchedulerEndpoint
  | UpdateSchedulerEndpoint
  deriving (Show, Read)

derivePersistField "SchedulerEndpoint"

type API =
  "scheduler"
    :> ( ListSchedulerAPI
           :<|> UpdateSchedulerAPI
       )

type ListSchedulerAPI =
  "listScheduler"
    :> ReqBody '[JSON] DScheduler.ListSchedulerReq
    :> Get '[JSON] DScheduler.ListSchedulerResp

type UpdateSchedulerAPI =
  "updateScheduler"
    :> ReqBody '[JSON] DScheduler.UpdateSchedulerReq
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listScheduler merchantId
    :<|> updateScheduler merchantId

listScheduler :: ShortId DM.Merchant -> DScheduler.ListSchedulerReq -> FlowHandler DScheduler.ListSchedulerResp
listScheduler merchantShortId = withFlowHandlerAPI . DScheduler.listScheduler merchantShortId

updateScheduler :: ShortId DM.Merchant -> DScheduler.UpdateSchedulerReq -> FlowHandler APISuccess
updateScheduler merchantShortId = withFlowHandlerAPI . DScheduler.updateScheduler merchantShortId
