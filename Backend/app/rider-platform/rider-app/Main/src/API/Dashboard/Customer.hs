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
import Lib.Error
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as QM

type API =
  "customer"
    :> ( CustomerListAPI
           :<|> CustomerUpdateAPI
           :<|> CustomerDeleteAPI
       )

type CustomerListAPI =
  "list"
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] Text

type CustomerUpdateAPI =
  Capture "customerId" (Id DP.Person)
    :> "update"
    :> ReqBody '[JSON] Text -- DProfile.UpdateProfileReq
    :> Post '[JSON] Text -- APISuccess

type CustomerDeleteAPI = Common.CustomerDeleteAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCustomer merchantId
    :<|> updateCustomer merchantId
    :<|> deleteCustomer merchantId

listCustomer ::
  ShortId DM.Merchant ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler Text
listCustomer merchantShortId _ _ = withFlowHandlerAPI $ do
  _merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  pure "To be done"

updateCustomer ::
  ShortId DM.Merchant ->
  Id DP.Person ->
  Text ->
  FlowHandler Text
updateCustomer merchantShortId _personId _req = withFlowHandlerAPI $ do
  _merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  pure "To be done"

deleteCustomer ::
  ShortId DM.Merchant ->
  Id Common.Customer ->
  FlowHandler APISuccess
deleteCustomer merchantShortId personId = withFlowHandlerAPI $ DPerson.deleteCustomer merchantShortId personId
