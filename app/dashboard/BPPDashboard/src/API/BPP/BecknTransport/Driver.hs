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
import qualified "lib-dashboard" Domain.Types.RegistrationToken as DReg
import "lib-dashboard" Environment
import qualified EulerHS.Types as T
import Servant
import "lib-dashboard" Tools.Auth
import qualified Tools.Client as Client

type API =
  "driver"
    :> "list"
    :> ServerAuth (ServerAccess 'BECKN_TRANSPORT)
    :> TokenAuth (ApiAccessLevel 'READ_ACCESS 'DRIVERS)
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listDriver

listDriver :: DReg.ServerName -> Id DP.Person -> FlowHandler Text
listDriver _ _ = withFlowHandlerAPI $ do
  Client.callBecknTransportApi client "becknTransportDriverList"
  where
    becknTransportDriverListAPI :: Proxy BecknTransport.DriverListAPI
    becknTransportDriverListAPI = Proxy
    client =
      T.client
        becknTransportDriverListAPI
