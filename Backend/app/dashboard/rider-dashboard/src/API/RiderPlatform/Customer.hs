{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Customer
  ( API,
    handler,
  )
where

import qualified "rider-app" API.Dashboard.Customer as BAP
import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as BAP
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "customer"
    :> ( ApiAuth 'APP_BACKEND 'READ_ACCESS 'CUSTOMERS
           :> BAP.CustomerListAPI
           :<|> ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'CUSTOMERS
             :> BAP.CustomerUpdateAPI
           :<|> ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'CUSTOMERS
             :> BAP.CustomerDeleteAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCustomer merchantId
    :<|> updateCustomer merchantId
    :<|> deleteCustomer merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.CustomerEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.CustomerAPI endpoint) apiTokenInfo Nothing Nothing

listCustomer ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler Text
listCustomer merchantShortId apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callRiderApp checkedMerchantId (.customers.customerList) mbLimit mbOffset

updateCustomer ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id BAP.Person ->
  Text ->
  FlowHandler Text
updateCustomer merchantShortId apiTokenInfo customerId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  -- transaction <- buildTransaction Common.UpdateCustomerEndpoint apiTokenInfo customerId (Just req)
  -- T.withTransactionStoring transaction $
  Client.callRiderApp checkedMerchantId (.customers.customerUpdate) customerId req

deleteCustomer ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id Common.Customer ->
  FlowHandler APISuccess
deleteCustomer merchantShortId apiTokenInfo customerId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.DeleteCustomerEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.customers.customerDelete) customerId
