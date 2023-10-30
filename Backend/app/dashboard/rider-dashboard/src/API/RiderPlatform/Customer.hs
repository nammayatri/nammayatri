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
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "customer"
    :> ( ApiAuth 'APP_BACKEND 'CUSTOMERS 'CUSTOMER_LIST
           :> Common.CustomerListAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'CUSTOMER_DELETE
             :> Common.CustomerDeleteAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'CUSTOMER_BLOCK
             :> Common.CustomerBlockAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'CUSTOMER_UNBLOCK
             :> Common.CustomerUnblockAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'CUSTOMER_INFO
             :> Common.CustomerInfoAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  listCustomer merchantId city
    :<|> deleteCustomer merchantId city
    :<|> blockCustomer merchantId city
    :<|> unblockCustomer merchantId city
    :<|> customerInfo merchantId city

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
  City.City ->
  ApiTokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Text ->
  FlowHandler Common.CustomerListRes
listCustomer merchantShortId opCity apiTokenInfo mbLimit mbOffset enabled blocked phone = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderApp checkedMerchantId opCity (.customers.customerList) mbLimit mbOffset enabled blocked phone

deleteCustomer ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Customer ->
  FlowHandler APISuccess
deleteCustomer merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.DeleteCustomerEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId opCity (.customers.customerDelete) customerId

blockCustomer ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Customer ->
  FlowHandler APISuccess
blockCustomer merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.BlockCustomerEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId opCity (.customers.customerBlock) customerId

unblockCustomer ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Customer ->
  FlowHandler APISuccess
unblockCustomer merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.UnblockCustomerEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId opCity (.customers.customerUnblock) customerId

customerInfo ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Id Common.Customer ->
  FlowHandler Common.CustomerInfoRes
customerInfo merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRiderApp checkedMerchantId opCity (.customers.customerInfo) customerId
