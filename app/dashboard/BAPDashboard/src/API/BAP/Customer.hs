module API.BAP.Customer
  ( API,
    handler,
  )
where

import qualified "app-backend" API.Dashboard.Customer as BAP
import qualified BAPClient.AppBackend as Client
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "app-backend" Domain.Types.Person as BAP
import "lib-dashboard" Environment
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
  Client.callAppBackendBAP checkedMerchantId (.customers.customerUpdate) customerId req
