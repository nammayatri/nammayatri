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
import qualified Domain.Action.Dashboard.Customer as DCustomer
import qualified Domain.Types.Merchant as DM
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
           :<|> Common.CustomerInfoAPI
           :<|> Common.CustomerCancellationDuesSyncAPI
           :<|> Common.GetCancellationDuesDetailsAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCustomers merchantId
    :<|> deleteCustomer merchantId
    :<|> blockCustomer merchantId
    :<|> unblockCustomer merchantId
    :<|> customerInfo merchantId
    :<|> customerCancellationDuesSync merchantId
    :<|> getCancellationDuesDetails merchantId

listCustomers ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Text ->
  FlowHandler Common.CustomerListRes
listCustomers merchantShortId mbLimit mbOffset enabled blocked =
  withFlowHandlerAPI . DCustomer.listCustomers merchantShortId mbLimit mbOffset enabled blocked

deleteCustomer ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  FlowHandler APISuccess
deleteCustomer merchantShortId personId = withFlowHandlerAPI $ DCustomer.deleteCustomer merchantShortId personId

blockCustomer ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  FlowHandler APISuccess
blockCustomer merchantShortId personId = withFlowHandlerAPI $ DCustomer.blockCustomer merchantShortId personId

unblockCustomer ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  FlowHandler APISuccess
unblockCustomer merchantShortId personId = withFlowHandlerAPI $ DCustomer.unblockCustomer merchantShortId personId

customerInfo ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  FlowHandler Common.CustomerInfoRes
customerInfo merchantShortId personId = withFlowHandlerAPI $ DCustomer.customerInfo merchantShortId personId

customerCancellationDuesSync :: ShortId DM.Merchant -> Id Common.Customer -> Common.CustomerCancellationDuesSyncReq -> FlowHandler APISuccess
customerCancellationDuesSync (ShortId merchantShortId) personId = withFlowHandlerAPI . DCustomer.customerCancellationDuesSync (ShortId merchantShortId) personId

getCancellationDuesDetails :: ShortId DM.Merchant -> Id Common.Customer -> FlowHandler Common.CancellationDuesDetailsRes
getCancellationDuesDetails (ShortId merchantShortId) personId = withFlowHandlerAPI $ DCustomer.getCancellationDuesDetails (ShortId merchantShortId) personId
