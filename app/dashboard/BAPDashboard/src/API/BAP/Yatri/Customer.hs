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
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth
import qualified Tools.Client as Client

type API =
  ApiAuth 'READ_ACCESS 'CUSTOMERS
    :> BAP.CustomerListAPI

handler :: FlowServer API
handler =
  listCustomer

listCustomer ::
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler Text
listCustomer _ mbLimit mbOffset = withFlowHandlerAPI $ do
  Client.callAppBackendYatriApi client "bapCustomerList"
  where
    bapCustomerListAPI :: Proxy BAP.CustomerListAPI
    bapCustomerListAPI = Proxy
    client token =
      Client.client
        bapCustomerListAPI
        token
        mbLimit
        mbOffset
