module API.BPP.BecknTransport.Driver
  ( API,
    handler,
  )
where

import qualified "beckn-transport" API.Dashboard.Driver as BecknTransport
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import qualified "lib-dashboard" Domain.Types.Person as DP
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth
import qualified Tools.Client as Client

type API =
  "driver"
    :> "list"
    :> ApiAuth 'READ_ACCESS 'DRIVERS
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listDriver

listDriver ::
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler Text
listDriver _ mbLimit mbOffset = withFlowHandlerAPI $ do
  Client.callBecknTransportApi client "becknTransportDriverList"
  where
    becknTransportDriverListAPI :: Proxy BecknTransport.DriverListAPI
    becknTransportDriverListAPI = Proxy
    client token =
      Client.client
        becknTransportDriverListAPI
        token
        mbLimit
        mbOffset
