module API.BAP.Yatri.Customer
  ( API,
    handler,
  )
where

import qualified "app-backend" API.Dashboard as BAP
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import "lib-dashboard" Domain.Types.Person as DP
import Environment
import qualified EulerHS.Types as T
import Servant
import Tools.Auth
import qualified Tools.Client as Client

type API =
  "customer"
    :> "list"
    :> TokenAuth (ApiAccessLevel 'READ_ACCESS 'CUSTOMERS)
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listCustomer

listCustomer :: Id DP.Person -> FlowHandler Text
listCustomer _ = withFlowHandlerAPI $ do
  Client.callAppBackendYatriApi client "bapCustomerList"
  where
    bapCustomerListAPI :: Proxy BAP.CustomerListAPI
    bapCustomerListAPI = Proxy
    client =
      T.client
        bapCustomerListAPI
