{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Customer where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Customer as Common
import qualified Domain.Action.Dashboard.Customer as DPerson
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)

type API =
  "customer"
    :> ( Common.CustomerListAPI
           :<|> Common.CustomerDeleteAPI
           :<|> Common.CustomerBlockAPI
           :<|> Common.CustomerUnblockAPI
       )

type CustomerUpdateAPI =
  Capture "customerId" (Id DP.Person)
    :> "update"
    :> ReqBody '[JSON] Text -- DProfile.UpdateProfileReq
    :> Post '[JSON] Text -- APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCustomers merchantId
    :<|> deleteCustomer merchantId
    :<|> blockCustomer merchantId
    :<|> unblockCustomer merchantId

listCustomers ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Text ->
  FlowHandler Common.CustomerListRes
listCustomers merchantShortId mbLimit mbOffset enabled blocked =
  withFlowHandlerAPI . DPerson.listCustomers merchantShortId mbLimit mbOffset enabled blocked

deleteCustomer ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  FlowHandler APISuccess
deleteCustomer merchantShortId personId = withFlowHandlerAPI $ DPerson.deleteCustomer merchantShortId personId

blockCustomer ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  FlowHandler APISuccess
blockCustomer merchantShortId personId = withFlowHandlerAPI $ DPerson.blockCustomer merchantShortId personId

unblockCustomer ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  FlowHandler APISuccess
unblockCustomer merchantShortId personId = withFlowHandlerAPI $ DPerson.unblockCustomer merchantShortId personId
