module API.RiderPlatform.Customer
  ( API,
    handler,
  )
where

import qualified "rider-app" API.Dashboard.Customer as BAP
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as BAP
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant hiding (throwError)
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant

type API =
  "customer"
    :> ( ApiAuth 'APP_BACKEND 'READ_ACCESS 'CUSTOMERS
           :> BAP.CustomerListAPI
           :<|> ApiAuth 'APP_BACKEND 'WRITE_ACCESS 'CUSTOMERS
             :> BAP.CustomerUpdateAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listCustomer merchantId
    :<|> updateCustomer merchantId

listCustomer ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler Text
listCustomer merchantShortId apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callAppBackendBAP checkedMerchantId (.customers.customerList) mbLimit mbOffset

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
  Client.callAppBackendBAP checkedMerchantId (.customers.customerUpdate) customerId req
