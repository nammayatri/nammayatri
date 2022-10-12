module API.BAP.ARDU.Customer
  ( API,
    handler,
  )
where

import qualified "app-backend" API.Dashboard as BAP
import qualified BAPClient.ARDU as Client
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth

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
  Client.callARDUBAP (.customers.customerList) mbLimit mbOffset
