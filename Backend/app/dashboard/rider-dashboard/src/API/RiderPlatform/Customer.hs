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

import qualified Dashboard.RiderPlatform.Customer as Common
import Domain.Types.AccessMatrix.BAP
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified RiderPlatformClient.RiderApp as Client
import Servant hiding (DELETE, throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "customer"
    :> ( ApiAuth ('AppBackendBAP ('CUSTOMERS 'LIST))
           :> Common.CustomerListAPI
           :<|> ApiAuth ('AppBackendBAP ('CUSTOMERS 'DELETE))
             :> Common.CustomerDeleteAPI
           :<|> ApiAuth ('AppBackendBAP ('CUSTOMERS 'BLOCK))
             :> Common.CustomerBlockAPI
           :<|> ApiAuth ('AppBackendBAP ('CUSTOMERS 'UNBLOCK))
             :> Common.CustomerUnblockAPI
           :<|> ApiAuth ('AppBackendBAP ('CUSTOMERS 'INFO))
             :> Common.CustomerInfoAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCustomer merchantId
    :<|> deleteCustomer merchantId
    :<|> blockCustomer merchantId
    :<|> unblockCustomer merchantId
    :<|> customerInfo merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.CustomerEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.CustomerAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

listCustomer ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Text ->
  FlowHandler Common.CustomerListRes
listCustomer merchantShortId apiTokenInfo mbLimit mbOffset enabled blocked phone = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callRiderApp checkedMerchantId (.customers.customerList) mbLimit mbOffset enabled blocked phone

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

blockCustomer ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id Common.Customer ->
  FlowHandler APISuccess
blockCustomer merchantShortId apiTokenInfo customerId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.BlockCustomerEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.customers.customerBlock) customerId

unblockCustomer ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id Common.Customer ->
  FlowHandler APISuccess
unblockCustomer merchantShortId apiTokenInfo customerId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UnblockCustomerEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.customers.customerUnblock) customerId

customerInfo ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Id Common.Customer ->
  FlowHandler Common.CustomerInfoRes
customerInfo merchantShortId apiTokenInfo customerId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callRiderApp checkedMerchantId (.customers.customerInfo) customerId
